-- ==
-- entry: part1 part2
-- compiled random input { [10000000]i32 } auto output
-- input @ input/01.in auto output

let solve [n] (off: i64) (input: [n]i32) =
    tabulate (n - off) (\i -> input[i] < input[i + off])
    |> map i32.bool
    |> i32.sum

entry part1 = solve 1

entry part2 = solve 3
