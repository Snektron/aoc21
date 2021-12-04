import "util/aoc"

let parse (input: []u8) =
    let lines = input |> split_lines
    let num_nums = input[:i64.i32 lines[0].1] |> count_by (== ',') |> (+1)
    let nums = split_fields input "," num_nums lines[0] |> map (parse_int input)
    let boards =
        tail lines
        |> filter (\(_, l) -> l != 0)
        |> map (split_fields input " " 5)
        |> map (map (parse_int input))
        |> in_chunks 5
    in (nums, boards)

let draw (num: i32) (board: [5][5]i32) =
    flatten board |> replace (== num) (-1) |> unflatten 5 5

let is_win (board: [5][5]i32) =
    (any (all (< 0)) board) || (any (all (< 0)) (transpose board))

let score (board: [5][5]i32) =
    flatten board |> replace (== -1) 0 |> i32.sum

entry part1 (input: []u8) =
    let (nums, boards) = parse input
    let (w, _, _) =
        loop (w, i, bs) = (-1, 0, copy boards) while w < 0 do
            let num = nums[i]
            let bs = map (draw num) bs
            let wi = find_index is_win bs
            let w = if wi >= 0 then num * score bs[wi] else -1
            in (w, i + 1, bs)
    in w

entry part2 (input: []u8) =
    let (nums, boards) = parse input
    let (w, _, _) =
        loop (w, i, bs) = (-1, 0, copy boards) while w < 0 do
            let num = nums[i]
            let bs = map (draw num) bs
            let (ws, ls) = partition is_win bs
            let w = if length ls == 0 then num * score (head ws) else w
            in (w, i + 1, ls)
    in w
