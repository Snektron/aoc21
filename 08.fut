import "util/aoc"

-- ==
-- entry: part1
-- input @ datasets/08.example.1.in output { 0i32 }
-- input @ datasets/08.example.2.in output { 26i32 }
-- input @ datasets/08.in output { 261i32 }

-- ==
-- entry: part2
-- input @ datasets/08.example.1.in output { 5353i64 }
-- input @ datasets/08.example.2.in output {61229i64 }
-- input @ datasets/08.in output { 987553i64 }


let unique_nr_by_segments: []bool = [
    false, -- 0
    false, -- 1
    true,  -- 2
    true,  -- 3
    true,  -- 4
    false, -- 5
    false, -- 6
    true   -- 7
]

let pows10: []i64 = [1000, 100, 10, 1]

entry part1 (input: []u8) =
    split_lines input
    |> map (split_fields input " |" 14)
    |> map (map (.1))
    |> map (\fields -> fields[10:] :> [4]i32)
    |> flatten
    |> map (\c -> unique_nr_by_segments[c])
    |> map i32.bool
    |> i32.sum

let field_to_segment (input: []u8) ((off, len): (i32, i32)): u8 =
    loop result = 0 for i < len do
        result | (1 << (input[off + i] - 'a'))

let resolve_segments (fields: [14]u8) =
    let input = fields[:10]
    let output = fields[10:] :> [4]u8
    let find_digit p = input[find_index_linear p input]
    let invert = (^0x7F)
    let contains a b = a & b == b
    let s1 = find_digit (\d -> u8.popc d == 2)
    let s4 = find_digit (\d -> u8.popc d == 4)
    let s7 = find_digit (\d -> u8.popc d == 3)
    let s8 = find_digit (\d -> u8.popc d == 7)
    let s9 = find_digit (\d -> d != s8 && (d `contains` (s4 | s7)))
    let s6 = find_digit (\d -> d != s8 && (d `contains` (invert s7)))
    let s0 = find_digit (\d -> d != s8 && (d `contains` (s1 | invert s9)))
    let s5 = find_digit (\d -> d != s8 && u8.popc (d ^ s6) == 1)
    let s2 = find_digit (\d -> d != s8 && d != s0 && (d `contains` (s6 ^ s9)))
    let s3 = (s2 & s5) | s1
    let segments_by_digits = [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9]
    in output
        |> map (\c -> find_index_linear (==c) segments_by_digits)
        |> map2 (\i c -> c * pows10[i]) (iota 4)
        |> i64.sum

entry part2 (input: []u8) =
    split_lines input
    |> map (split_fields input " |" 14)
    |> map (map (field_to_segment input))
    |> map (resolve_segments)
    |> i64.sum
