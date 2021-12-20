import "util/aoc"

-- ==
-- entry: part1
-- compiled input @ datasets/20.example.in output { 35i64 }
-- compiled input @ datasets/20.in output { 5225i64 }

-- ==
-- entry: part2
-- compiled input @ datasets/20.example.in output { 3351i64 }
-- compiled input @ datasets/20.in output { 18131i64 }

let parse (input: []u8) =
    let nl = find_index (== '\n') input
    let (alg, rest) = split nl input
    let image =
        split_regular_lines rest[2:]
        |> map (map (== '#'))
    in (map (== '#') alg, image)

let offsets: [](i64, i64) = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 0),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
]

let enhance_step [n][m] (alg: []bool) (oob_pixel: bool, img: [n][m]bool) =
    let pixel i j =
        if i > 0 && i <= n && j > 0 && j <= m then
            img[i - 1, j - 1]
        else
            oob_pixel
    let img =
        tabulate_2d
            (2 + n)
            (2 + m)
            (\y x ->
                offsets
                |> map (\(a, b) -> (a + y, b + x))
                |> map (uncurry pixel)
                |> map i64.bool
                |> reverse
                |> map2 (flip (<<)) (indices offsets)
                |> reduce (|) 0
                |> (\x -> alg[x]))
    let oob = alg[0b111111111 * i64.bool oob_pixel]
    in (oob, img)

let enhance [n][m] (steps: i64) (alg: []bool) (img: [n][m]bool) =
    let (_, img) =
        loop (oob_pixel, img) = (false, img) for _i < steps do
            enhance_step alg (oob_pixel, img)
    in img

let solve (steps: i64) (input: []u8) =
    let (alg, img) = parse input
    in
        img
        |> enhance steps alg
        |> flatten
        |> map i64.bool
        |> i64.sum

entry part1 = solve 2
entry part2 = solve 50
