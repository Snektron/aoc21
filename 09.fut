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
    let to_index i j = i16.i64 (i * m + j)
    in tabulate_2d
        n
        m
        (\i j ->
            let a = if i == 0 then '9' else g[i - 1, j]
            let b = if i == n - 1 then '9' else g[i + 1, j]
            let c = if j == 0 then '9' else g[i, j - 1]
            let d = if j == m - 1 then '9' else g[i, j + 1]
            let x = g[i, j]
            let m = u8.minimum [a, b, c, d, x]
            in if x == '9' || m == x then to_index i j
            else if m == a then to_index (i - 1) j
            else if m == b then to_index (i + 1) j
            else if m == c then to_index i (j - 1)
            else to_index i (j + 1))
    |> flatten
    |> iterate
        (bit_width (i32.i64 (n + m)))
        (\parents -> map (\i -> parents[i]) parents)
    |> map i64.i16
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
