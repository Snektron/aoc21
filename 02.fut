import "aoc"

let parse (input: []u8) =
    input
    |> split_lines
    |> map (\(off, len) -> (input[off], i32.u8 (input[off + len - 1] - '0')))

let solve_horiz (input: [](u8, i32)) =
    input
    |> map (\(c, amt) -> if c == 'f' then amt else 0)
    |> i32.sum

entry part1 (input: []u8) =
    let parsed = parse input
    let h = solve_horiz parsed
    let v =
        parsed
        |> map (\(c, amt) -> if c == 'u' then -amt else if c == 'd' then amt else 0)
        |> i32.sum
    in h * v

entry part2 (input: []u8) =
    let parsed = parse input
    let h = solve_horiz parsed
    let v =
        parsed
        |> map (\(c, amt) -> if c == 'u' then -amt else if c == 'd' then amt else 0)
        |> scan (+) 0
        |> map2 (\(c, amt) aim -> if c == 'f' then amt * aim else 0) parsed
        |> i32.sum
    in h * v
