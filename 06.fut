import "util/aoc"

let solve (days: i32) (input: []u8) =
    input
    |> replace (== ',') '\n'
    |> split_lines
    |> map (parse_int input)
    |> map i64.i32
    |> histogram 9
    |> iterate
        days
        (\fish -> (rotate 1 (copy fish)) with [6] = fish[0] + fish[7])
    |> i64.sum

entry part1 = solve 80
entry part2 = solve 256
