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
        if i > 0 && j > 0 then i64.min i j
        else i64.max i j
    in
        iota n
        |> replace (\i -> !(p as[i])) (-1)
        |> reduce_comm f (-1)

let count_by [n] 't (p: t -> bool) (as: [n]t): i64 =
    map p as |> map i64.bool |> i64.sum

-- Offset, length
type slice = (i32, i32)

let split_lines [n] (input: [n]u8): []slice =
    let endings =
        iota n
        |> filter (\i -> input[i] == '\n')
        |> map i32.i64
    let lengths =
        endings
        |> zip (indices endings)
        |> map (\(i, x) -> if i == 0 then x else x - endings[i - 1] - 1)
    in zip (map2 (-) endings lengths) lengths

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

