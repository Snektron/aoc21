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
    let x = reduce (^) 0 input
    let s = loop s = replicate 10 0 for i < 10 do
        match u8.popc input[i]
        case 2 -> s with [1] = input[i]
        case 3 -> s with [7] = input[i]
        case 4 -> s with [4] = input[i]
        case 7 -> s with [8] = input[i]
        case _ -> s
    let s = s with [0] = invert s[4] | s[1] | invert x
    let s = s with [3] = x | s[7]
    let s = s with [6] = invert s[1] | x
    let s = s with [9] = s[7] | s[4] | x
    let s = s with [5] = s[6] & s[9]
    let s = s with [2] = invert s[5] | (s[3] & invert s[1])
    in output
        |> map (\c -> find_index_linear (==c) s)
        |> map2 (\i c -> c * pows10[i]) (iota 4)
        |> i64.sum

entry part2 (input: []u8) =
    split_lines input
    |> map (split_fields input " |" 14)
    |> map (map (field_to_segment input))
    |> map (resolve_segments)
    |> i64.sum
