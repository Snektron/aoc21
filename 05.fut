import "util/aoc"

-- ==
-- entry: part1
-- input @ datasets/05.example.in output { 5i32 }
-- input @ datasets/05.in output { 6461i32 }

-- ==
-- entry: part2
-- input @ datasets/05.example.in output { 12i32 }
-- input @ datasets/05.in output { 18065i32 }

let parse (input: []u8) =
    split_lines input
    |> map (split_fields input ", ->" 4)
    |> map (map (parse_int input))
    |> map (\xs -> (xs[0], xs[1], xs[2], xs[3]))

let solve (lines: [](i32, i32, i32, i32)) =
    let dim =
        lines
        |> map (\(x1, y1, x2, y2) -> [x1, y1, x2, y2])
        |> flatten
        |> i32.maximum
    let lens = map
        (\(x1, y1, x2, y2) -> i32.max (i32.abs (x2 - x1)) (i32.abs (y2 - y1)))
        lines
    let max_len = 1 + i64.i32 (i32.maximum lens)
    let coords =
        lines
        |> map2
            (\len (x1, y1, x2, y2) ->
                iota max_len
                |> map i32.i64
                |> map (\i ->
                    if i > len then (-1, -1)
                    else (x1 + i * i32.sgn (x2 - x1), y1 + i * i32.sgn (y2 - y1))))
            lens
        |> flatten
        |> map (\(x, y) -> if x < 0 then -1 else x * dim + y)
        |> map i64.i32
    let m = length coords
    in
        reduce_by_index
            (replicate (i64.i32 (dim * dim)) 0)
            (+)
            0
            (coords :> [m]i64)
            (replicate m 1)
        |> map (>= 2)
        |> map i32.bool
        |> i32.sum

entry part1 (input: []u8) =
    parse input
    |> filter (\(x1, y1, x2, y2) -> x1 == x2 || y1 == y2)
    |> solve

entry part2 (input: []u8) =
    parse input
    |> solve
