import "util/aoc"

-- ==
-- entry: part1
-- input @ datasets/13.example.in output { 17i32 }
-- input @ datasets/13.in output { 737i32 }

-- ==
-- entry: part2
-- input @ datasets/13.example.in auto output
-- input @ datasets/13.in auto output

let parse [n] (input: [n]u8) =
    let lines = split_lines input
    let splitpoint = find_index (\(_, len) -> len == 0) lines
    let (coords, folds) = split splitpoint lines
    let coords =
        coords
        |> map (split_fields input "," 2)
        |> map (map (parse_int input))
        |> map (\fields -> (i64.i32 fields[0], i64.i32 fields[1]))
    let m =
        coords
        |> reduce
            (\(x0, y0) (x1, y1) -> (i64.max x0 x1, i64.max y0 y1))
            (0, 0)
    let paper =
        scatter_2d
            (replicate_2d (m.0 + 1) (m.1 + 1) false)
            coords
            (map (\_ -> true) coords)
        |> transpose
    let folds =
        folds
        |> tail
        |> map (split_fields input "fold ang=" 2)
        |> map (\fields ->
            let pos = parse_int input fields[1]
            in if input[fields[0].0] == 'y' then pos else -pos)
        |> map i64.i32
    in (paper, folds)

let fold [n][m] (pos: i64) (paper: [n][m]bool) =
    let fold_inner [n] [m] (pos: i64) (paper: [n][m]bool) =
        let get i j = i >= 0 && i < n && paper[i, j]
        in tabulate_2d pos m (\i j -> paper[i, j] || get (2 * pos - i) j)
    in if pos >= 0 then
        fold_inner pos paper
    else
        transpose paper
        |> fold_inner (-pos)
        |> transpose

entry part1 (input: []u8) =
    let (paper, folds) = parse input
    in
        paper
        |> fold (head folds)
        |> flatten
        |> map i32.bool
        |> i32.sum

entry part2 (input: []u8) =
    let (paper, folds) = parse input
    in
        loop paper = paper for i < length folds do
            fold folds[i] paper
