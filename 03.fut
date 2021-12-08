import "util/aoc"

-- ==
-- entry: part1
-- input @ datasets/03.example.in output { 198i64 }
-- input @ datasets/03.in output { 3885894i64 }

-- ==
-- entry: part2
-- input @ datasets/03.example.in output { 230i32 }
-- input @ datasets/03.in output { 4375225i32 }

let contract_bits [n] (bits: [n]i16): i16 =
    map2
        (\c i -> c << i16.i64 (n - i - 1))
        bits
        (iota n)
    |> reduce (|) 0

let parse [n] (bits: i64) (input: [n]u8): []i16 =
    input
    |> map (\x -> if x == '1' then 1 else 0)
    |> unflatten (n / (bits + 1)) (bits + 1)
    |> map (\line -> line[:bits])
    |> map (map i16.u8 >-> contract_bits)

let most_common_bit [n] (bit: i32) (words: [n]i16): i16 =
    let count =
        map (i16.get_bit bit) words
        |> i32.sum
    in i16.bool (count * 2 >= i32.i64 n)

entry part1 (input: []u8) =
    let n = find_index (== '\n') input
    let words = parse n input
    let x = tabulate n (\i -> most_common_bit (i32.i64 i) words) |> reverse |> contract_bits
    let x = i64.i16 x
    in x * (!x & ((1 << n) - 1))

let solve (x: i16) (n: i64) (words: []i16) =
    let (_, sol) =
        loop (i, words) = (n - 1, words) while length words > 1 do
            let bit = x ^ most_common_bit (i32.i64 i) words
            in (i - 1, filter (\w -> i16.get_bit (i32.i64 i) w == i32.i16 bit) words)
    in i32.i16 (head sol)

entry part2 (input: []u8) =
    let n = find_index (== '\n') input
    let words = parse n input
    in (solve 0 n words) * (solve 1 n words)

