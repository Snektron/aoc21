import "util/aoc"

-- ==
-- entry: part1
-- compiled input @ datasets/21.example.in output { 739785i32 }
-- compiled input @ datasets/21.in output { 711480i32 }

-- ==
-- entry: part2
-- compiled input @ datasets/21.example.in output { 444356092776315i64 }
-- compiled input @ datasets/21.in output { 265845890886828i64 }

let parse (input: []u8) =
    split_lines input
    |> map (split_fields input " " 5)
    |> map (.[4])
    |> map (parse_int input)

entry part1 (input: []u8) =
    let starts = parse input
    let (_, _, bs, _, i) =
        loop (a, b, as, bs, i) = (starts[0], starts[1], 0, 0, 0) while bs < 1000 do
            let a' = (a + 9 * i + 6 - 1) % 10 + 1
            let as = as + a'
            in (b, a', bs, as, i + 1)
    in bs * i * 3

let rolls =
    let [n] (is: [n]i64) =
        tabulate_3d 3 3 3 (\i j k -> i + j + k)
        |> flatten_3d
    in
        reduce_by_index
            (replicate 7 0i64)
            (+)
            0
            is
            (replicate n 1)
        |> map2 (\i j -> (i + 3, j)) (iota 7)

let iter (s: [10][31]i64) =
    tabulate_2d 10 31
        (\tile score ->
            map
                (\(roll, weight) ->
                    let prev_tile = (tile + 10 - roll) % 10
                    let prev_score = score - tile - 1
                    in if prev_score < 21 && prev_score >= 0 then
                        s[prev_tile, prev_score] * weight
                    else
                        0)
                rolls
            |> i64.sum)

let compute (start_space: i64) =
    let s = replicate_2d 10 31 0i64
    let s = s with [start_space - 1, 0] = 1
    let (_, stats) =
        loop (s, stats) = (s, []) while length stats == 0 || (last stats).0 != 0 || (last stats).1 != 0 do
            let (ongoing, win) =
                s
                |> map
                    (\xs ->
                        let (as, bs) = split 21 xs
                        in (i64.sum as, i64.sum bs))
                |> reduce (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)
            in (iter s, stats ++ [(ongoing, win)])
    in unzip stats

entry part2 (input: []u8) =
    let starts = parse input
    let [k1] (ongoing1: [k1]i64, won1: [k1]i64) = compute (i64.i32 starts[0])
    let [k2] (ongoing2: [k2]i64, won2: [k2]i64) = compute (i64.i32 starts[1])
    let wins1 =
        map
            (\i -> won1[i] * ongoing2[i - 1])
            (1..<k1)
        |> i64.sum
    let wins2 =
        map
            (\i -> won2[i] * ongoing1[i])
            (0..<k2)
        |> i64.sum
    in i64.max wins1 wins2
