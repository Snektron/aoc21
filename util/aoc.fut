let in_windows [n] 't (ws: i64) (as: [n]t): [][ws]t =
    tabulate (n - ws + 1) (\i -> as[i:i + ws] :> [ws]t)

let in_chunks [n] 't (cs: i64) (as: [n]t): [][cs]t =
    unflatten (n / cs) cs as

let in_windows_of_pairs [n] 't (as: [n]t): [](t, t) =
    let m = n - 1
    in zip (as[:m] :> [m]t) (as[1:] :> [m]t)

let in_pairs [n] 't (as: [n]t): [](t, t) =
    let m = assert (n % 2 == 0) (n / 2)
    in zip (as[0::2] :> [m]t) (as[1::2] :> [m]t)

let replace [n] 't (p: t -> bool) (replacement: t) (as: [n]t): [n]t =
    map (\x -> if p x then replacement else x) as

let find_index [n] 't (p: t -> bool) (as: [n]t): i64 =
    let f i j =
        if i >= 0 && j >= 0 then i64.min i j
        else i64.max i j
    in
        iota n
        |> replace (\i -> !(p as[i])) (-1)
        |> reduce_comm f (-1)

let find_index_linear [n] 't (p: t -> bool) (as: [n]t): i64 =
    loop index = -1 for i < n do
        if index < 0 && p as[i] then i else index

let count_by [n] 't (p: t -> bool) (as: [n]t): i64 =
    map p as |> map i64.bool |> i64.sum

-- Offset, length
type slice = (i32, i32)

let split_by [n] (c: u8) (input: [n]u8): []slice =
    let endings =
        iota n
        |> filter (\i -> input[i] == c)
        |> map i32.i64
    let lengths =
        endings
        |> zip (indices endings)
        |> map (\(i, x) -> if i == 0 then x else x - endings[i - 1] - 1)
    in zip (map2 (-) endings lengths) lengths

let split_lines [n] (input: [n]u8): []slice = split_by '\n' input

let parse_int (input: []u8) ((offset, len): slice): i32 =
    let (offset, len, sign) = if input[offset] == '-' then (offset + 1, len - 1, -1i32) else (offset, len, 1)
    let unsigned = loop value = 0i32 for i < len do
        let c = i32.u8 input[offset + i]
        in value * 10 - '0' + c
    in unsigned * sign

let split_fields (input: []u8) (sep: []u8) (fields: i64) ((offset, len): slice): [fields]slice =
    let is_sep o =
        any (== input[o + offset]) sep
    let is_field_start o =
        !is_sep o && (o == 0 || is_sep (o - 1))
    let is_field_end o =
        !is_sep o && (o == len - 1 || is_sep (o + 1))
    let get_offsets pred =
        loop (field, offsets) = (0, replicate fields 0) for i < len do
            if pred i
                then (field + 1, offsets with [field] = i)
                else (field, offsets)
    let (_, field_starts) = get_offsets is_field_start
    let (_, field_ends) = get_offsets is_field_end
    in
        map2 (-) field_ends field_starts
        |> map (+1)
        |> zip (map (+ offset) field_starts)

let split_regular_lines [n] (input: [n]u8): [][]u8 =
    let lines =
        map (== '\n') input
        |> map i64.bool
        |> i64.sum
    let line_length = n / lines
    let m = line_length - 1
    in
        input
        |> unflatten lines line_length
        |> map (\line -> line[:m])

let fetch_field (input: []u8) ((off, len): (i32, i32)): []u8 =
    (0..<len)
    |> map (+off)
    |> map (\i -> input[i])

let histogram [n] (m: i64) (is: [n]i64): []i64 =
    reduce_by_index
        (replicate m 0)
        (+)
        0
        is
        (replicate n 1)

let replicate_2d 't (n: i64) (m: i64) (x: t): *[n][m]t =
    replicate (n * m) x |> unflatten n m

let replicate_3d 't (n: i64) (m: i64) (k: i64) (x: t): *[n][m][k]t =
    replicate (n * m * m) x |> unflatten_3d n m k

let bit_width (x: i32): i32 = i32.num_bits - (i32.clz x)

let bit_width_64 (x: i64): i32 = i64.num_bits - (i64.clz x)

let invert [n] (as: [n]i32): [n]i32 =
    scatter
        (replicate n (-1i32))
        (as |> map i64.i32)
        (iota n |> map i32.i64)

let interlace [n][m] 't (as: [n]t) (bs: [m]t): []t =
    map
        (\i -> if i % 2 == 0 then as[i / 2] else bs[i / 2])
        (iota (n + m))

let reduce_by_index_2d [n][m][k] 't
    (dest: *[n][m]t)
    (f: t -> t -> t)
    (ne: t)
    (is: [k](i64, i64))
    (as: [k]t):
    *[n][m]t =
        reduce_by_index
            (flatten dest)
            f
            ne
            (map (\(i, j) -> i * m + j) is)
            as
        |> unflatten n m

let reduce_by_index_3d [n][m][k][l] 't
    (dest: *[n][m][k]t)
    (f: t -> t -> t)
    (ne: t)
    (is: [l](i64, i64, i64))
    (as: [l]t):
    *[n][m][k]t =
        reduce_by_index
            (flatten_3d dest)
            f
            ne
            (map (\(x, y, z) -> x * k * m + y * m + z) is)
            as
        |> unflatten_3d n m k

let split_lines_pad_regular (pad: u8) (input: []u8) =
    let lines = split_lines input
    let m =
        lines
        |> map (.1)
        |> i32.maximum
        |> i64.i32
    in map
        (\(off, len) ->
            tabulate
                m
                (\i -> if i < i64.i32 len then input[i64.i32 off + i] else pad))
        lines

let cartesian [n][m] 't 'u 'v (f: t -> u -> v) (as: [n]t) (bs: [m]u) =
    map (\a -> map (f a) bs) as
    |> flatten

let all2 [n] 't 'u (f: t -> u -> bool) (as: [n]t) (bs: [n]u) =
    map2 f as bs
    |> reduce (&&) true
