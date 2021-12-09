import "util/aoc"
import "util/sorts/radix_sort"

-- ==
-- entry: part1
-- input @ datasets/09.example.in output { 15i32 }
-- input @ datasets/09.in output { 504i32 }

-- ==
-- entry: part2
-- input @ datasets/09.example.in output { 1134i64 }
-- input @ datasets/09.in output { 1558722i64 }

let solve1 [n][m] (g: [n][m]u8) =
    tabulate_2d
        n
        m
        (\i j ->
            let h = g[i, j]
            let a = i == 0 || h < g[i - 1, j]
            let b = i == n - 1 || h < g[i + 1, j]
            let c = j == 0 || h < g[i, j - 1]
            let d = j == m - 1 || h < g[i, j + 1]
            in if a && b && c && d then 1 + i32.u8 h - '0' else 0)
    |> flatten
    |> i32.sum

entry part1 (input: []u8) =
    split_regular_lines input |> solve1

let solve2 [n][m] (g: [n][m]u8) =
    let mask = map (map (== '9')) g
    let iter (parents: [n][m]i64): [n][m]i64 =
        tabulate_2d
            n
            m
            (\i j ->
                let a = if i == 0 || mask[i - 1, j] then -1 else parents[i - 1, j]
                let b = if i == n - 1 || mask[i + 1, j] then -1 else parents[i + 1, j]
                let c = if j == 0 || mask[i, j - 1] then -1 else parents[i, j - 1]
                let d = if j == m - 1 || mask[i, j + 1] then -1 else parents[i, j + 1]
                let x = if mask[i, j] then -1 else parents[i, j]
                in if mask[i, j] then -1 else i64.maximum [a, b, c, d, x])
    in
        tabulate_2d n m (\i j -> i * m + j)
        |> iterate
            (bit_width (i32.i64 (4 * n * m)))
            iter
        |> flatten
        |> histogram (n * m)
        |> map (\x -> [x, 0, 0])
        |> reduce_comm
            (\as bs ->
                let (_, _, result) =
                    loop (a, b, result) = (0, 0, replicate 3 0) for i < 3 do
                        if as[a] > bs[b] then
                            (a + 1, b, result with [i] = as[a])
                        else
                            (a, b + 1, result with [i] = bs[b])
                in result)
            [0, 0, 0]
        |> reduce (*) 1

entry part2 (input: []u8) =
    split_regular_lines input |> solve2
