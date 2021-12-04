import "util/aoc"

let contract_bits [n] (bits: [n]i32): i32 =
    iota n
    |> map i32.i64
    |> map2 (<<) bits
    |> reduce (|) 0

let parse (input: []u8): (i64, []i32) =
    let lines = split_regular_lines input
        |> map (map ((== '1') >-> i32.bool))
    let n = length lines[0]
    in (n, map (reverse >-> contract_bits) lines)

let most_common_bit [n] (bit: i32) (words: [n]i32): i32 =
    let count =
        map (i32.get_bit bit) words
        |> i32.sum
    in i32.bool (count * 2 >= i32.i64 n)

entry part1 (input: []u8) =
    let (n, words) = parse input
    let x = tabulate n (\i -> most_common_bit (i32.i64 i) words) |> contract_bits
    in x * (!x & ((1 << i32.i64 n) - 1))

let solve (x: i32) (n: i64) (words: []i32) =
    let (_, sol) =
        loop (i, words) = (n - 1, words) while length words > 1 do
            let bit = x ^ most_common_bit (i32.i64 i) words
            in (i - 1, filter (\w -> i32.get_bit (i32.i64 i) w == bit) words)
    in head sol

entry part2 (input: []u8) =
    let (n, words) = parse input
    in (solve 0 n words) * (solve 1 n words)
