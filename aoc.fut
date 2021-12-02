let in_windows [n] 't (ws: i64) (as: [n]t): [][ws]t =
    tabulate (n - ws + 1) (\i -> as[i:i + ws] :> [ws]t)

let in_windows_of_pairs [n] 't (as: [n]t): [](t, t) =
    let m = n - 1
    in zip (as[:m] :> [m]t) (as[1:] :> [m]t)

let in_pairs [n] 't (as: [n]t): [](t, t) =
    let m = assert (n % 2 == 0) (n / 2)
    in zip (as[0::2] :> [m]t) (as[1::2] :> [m]t)

let split_lines [n] (input: [n]u8): [](i32, i32) =
    let endings =
        iota n
        |> filter (\i -> input[i] == '\n')
        |> map i32.i64
    let lengths =
        endings
        |> zip (indices endings)
        |> map (\(i, x) -> if i == 0 then x else x - endings[i - 1] - 1)
    in zip (map2 (-) endings lengths) lengths

let parse_int (input: []u8) ((offset, len): (i32, i32)): i32 =
    let (offset, len, sign) = if input[offset] == '-' then (offset + 1, len - 1, -1i32) else (offset, len, 1)
    let unsigned = loop value = 0i32 for i < len do
        let c = i32.u8 input[offset + i]
        in value * 10 - '0' + c
    in unsigned * sign
