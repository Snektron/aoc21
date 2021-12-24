import "util/aoc"
import "util/sorts/radix_sort"

-- ==
-- entry: part1
-- compiled input @ datasets/24.in output { 94992994195998i64 }

-- ==
-- entry: part2
-- compiled input @ datasets/24.in output { 21191861151161i64 }

let parse (input: []u8) =
    let group_instructions = [4, 5, 15]
    in
        input
        |> replace (== 'n') ' '
        |> split_lines
        |> map (split_fields input " " 3)
        |> in_chunks 18
        |> map
            (\group ->
                group_instructions
                |> map (\i -> group[i])
                |> map (.[2])
                |> map (parse_int input)
                |> map i64.i32)

let solve (f: i64 -> (i64, i64)) (input: []u8) =
    let [n] (vs: [n][3]i64) = parse input
    let powers_of_ten =
        scan (*) 1 (replicate n 10)
        |> rotate (-1)
        |> map2 (\i y -> if i == 0 then 1 else y) (iota n)
    let brackets =
        vs
        |> map (.[0])
        |> map (== 1)
    let (is, vs) =
        brackets
        |> map (\b -> if b then 1i8 else -1i8)
        |> scan (+) 0
        |> map2 (\b d -> d - i8.bool b) brackets
        |> zip (iota n)
        |> radix_sort 8 (\bit (_, depth) -> i8.get_bit bit depth)
        |> map (.0)
        |> in_pairs
        |> map
            (\(left, right) ->
                let (left_digit, right_digit) = f (vs[left, 2] + vs[right, 1])
                in [(left, left_digit), (right, right_digit)])
        |> flatten
        |> unzip
    in scatter
        (replicate n 0)
        is
        vs
    |> reverse
    |> map2 (*) powers_of_ten
    |> i64.sum


entry part1 (input: []u8) =
    solve
        (\y ->
            if y < 0 then
                (9, 9 + y)
            else
                (9 - y, 9))
        input

entry part2 (input: []u8) =
    solve
        (\y ->
            if y < 0 then
                (-y + 1, 1)
            else
                (1, y + 1))
        input
