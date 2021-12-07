import "util/aoc"
import "util/sorts/radix_sort"

-- ==
-- entry: part1
-- input @ datasets/07.example.in output { 37i32 }
-- input @ datasets/07.in output { 343468i32 }

-- ==
-- entry: part2
-- input @ datasets/07.example.in output { 168i32 }
-- input @ datasets/07.in output { 96086265i32 }

let parse (input: []u8) =
    input
    |> replace (== ',') '\n'
    |> split_lines
    |> map (parse_int input)

entry part1 (input: []u8) =
    let crabs = parse input
    let sort_bits = 1 + bit_width (i32.maximum crabs)
    let crabs = radix_sort_int sort_bits i32.get_bit crabs
    let cost target =
        crabs
        |> map (\crab -> i32.abs (crab - target))
        |> i32.sum
    in
        i32.min (cost crabs[length crabs / 2]) (cost crabs[length crabs / 2 + 1])

entry part2 (input: []u8) =
    let crabs = parse input
    let target = (i32.sum crabs) / (i32.i64 (length crabs))
    let cost target =
        crabs
        |> map (\crab -> i32.abs (crab - target))
        |> map (\d -> d * (d + 1))
        |> i32.sum
        |> (/2)
    in
        i32.min (cost target) (cost (target + 1))