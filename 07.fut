import "util/aoc"
import "util/sorts/radix_sort"

let parse (input: []u8) =
    input
    |> replace (== ',') '\n'
    |> split_lines
    |> map (parse_int input)

entry part1 (input: []u8) =
    let crabs = parse input
    in
        indices crabs
        |> map i32.i64
        |> map
            (\i ->
                crabs
                |> map ((i-) >-> i32.abs)
                |> i32.sum)
        |> i32.minimum

entry part2 (input: []u8) =
    let crabs = parse input
    in
        indices crabs
        |> map i32.i64
        |> map
            (\i ->
                crabs
                |> map ((i-) >-> i32.abs)
                |> map (\d -> d * (d + 1))
                |> i32.sum)
        |> i32.minimum
        |> (/2)
