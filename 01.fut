import "util/aoc"

let parse (input: []u8): []i32 =
    split_lines input
    |> map (parse_int input)

let solve [n] (off: i64) (input: [n]i32): i32 =
    tabulate (n - off) (\i -> input[i] < input[i + off])
    |> map i32.bool
    |> i32.sum

entry part1 (input: []u8) = parse input |> solve 1
entry part2 (input: []u8) = parse input |> solve 3
