import "util/aoc"
import "util/sorts/radix_sort"

-- ==
-- entry: part1
-- input @ datasets/10.example.in output { 26397i32 }
-- input @ datasets/10.in output { 316851i32 }

-- ==
-- entry: part2
-- input @ datasets/10.example.in output { 288957i64 }
-- input @ datasets/10.in output { 2182912364i64 }

let stack_diff (c: u8): i32 =
    match c
    case '(' -> 1
    case '[' -> 1
    case '{' -> 1
    case '<' -> 1
    case ')' -> -1
    case ']' -> -1
    case '}' -> -1
    case '>' -> -1
    case _ -> 0

let is_opening (c: u8): bool = stack_diff c == 1
let is_closing (c: u8): bool = stack_diff c == -1

let get_closing (opening: u8): u8 =
    match opening
    case '(' -> ')'
    case '[' -> ']'
    case '{' -> '}'
    case '<' -> '>'
    case _ -> 0

let part1_score (c: u8): i32 =
    match c
    case ')' -> 3
    case ']' -> 57
    case '}' -> 1197
    case '>' -> 25137
    case _ -> 0

let is_pair (a: u8) (b: u8): bool = get_closing a == b

let match_brackets [n] (brackets: [n]u8) = -- [n]i32
    let depths =
        brackets
        |> map stack_diff
        |> scan (+) 0
        -- Decrease depth of opening brackets
        |> map2 (\b d -> if stack_diff b == 1 then d - 1 else d) brackets
    let max_depth = i32.maximum depths
    let bits = 1 + bit_width max_depth -- add one for sign
    let perm =
        iota n
        |> map i32.i64
        |> zip depths
        |> radix_sort_int bits (\bit (d, _) -> i32.get_bit bit d)
        |> map (.1)
    let unperm = invert perm
    -- If a bracket is an opening bracket and the next bracket is a closing bracket, they should form a pair
    let m = length perm - 1
    in
        iota n
        |> map (\i ->
            let b = brackets[i]
            let j = unperm[i]
            let ty = stack_diff b
            in match ty
            case 1 ->
                if i == m then -1i32
                else if is_closing brackets[perm[j + 1]] then perm[j + 1]
                else -1
            case -1 ->
                if i == 0 then -1
                else if is_opening brackets[perm[j - 1]] then perm[j - 1]
                else -1
            case 0 -> -1)

let parse (input: []u8) =
    let lines = split_lines input
    let m =
        lines
        |> map (.1)
        |> i32.maximum
        |> i64.i32
    let regular_lines =
        map
            (\(off, len) ->
                tabulate
                    m
                    (\i -> if i < i64.i32 len then input[i64.i32 off + i] else 0))
            lines
    let partner = map match_brackets regular_lines
    in zip regular_lines partner

let is_corrupted_pair (brackets: []u8) (b: u8) (i: i32) =
    !(is_opening b) && i != -1 && !(is_pair brackets[i] b)

let part1_compute_score [n] (brackets: [n]u8) (m: [n]i32) =
    let corrupted = find_index (uncurry (is_corrupted_pair brackets)) (zip brackets m)
    in if corrupted == -1
        then 0
        else part1_score brackets[corrupted]

entry part1 (input: []u8) =
    parse input
    |> map (uncurry part1_compute_score)
    |> i32.sum

let is_incomplete [n] (brackets: [n]u8) (m: [n]i32) =
    !(any (uncurry (is_corrupted_pair brackets)) (zip brackets m))

let part2_score (c: u8): i64 =
    match c
    case ')' -> 1
    case ']' -> 2
    case '}' -> 3
    case '>' -> 4
    case _ -> 0

let part2_compute_score [n] (brackets: [n]u8) (m: [n]i32) =
    map2
        (\b i -> if i == -1 then get_closing b else 0)
        brackets
        m
    |> map part2_score
    |> map (\x -> (x, if x == 0 then 1 else 5))
    |> reduce
        (\(a, ax) (b, bx) -> (b * ax + a, ax * bx))
        (0, 1)
    |> (.0)

entry part2 (input: []u8) =
    let scores =
        parse input
        |> filter (uncurry is_incomplete)
        |> map (uncurry part2_compute_score)
    let sort_bits = 1 + bit_width_64 (i64.maximum scores)
    let sorted = radix_sort_int sort_bits i64.get_bit scores
    in sorted[length sorted / 2]
