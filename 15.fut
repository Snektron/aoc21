import "util/aoc"

-- ==
-- entry: part1
-- input @ datasets/15.example.in output { 40i64 }
-- input @ datasets/15.in output { 435i64 }

-- ==
-- entry: part2
-- input @ datasets/15.example.in output { 315i64 }
-- input @ datasets/15.in output { 2842i64 }

let parse (input: []u8) =
    split_regular_lines input
    |> map (map (\c -> c - '0'))
    |> map (map i64.u8)

let bellman_ford [n][m] (w: [n][m]i64) =
    let relax d =
        let relax_one i j k l =
            if i < n && j < m && k < n && l < m && d[i, j] + w[k, l] < d[k, l] then
                ((k, l), d[i, j] + w[k, l])
            else
                ((-1, -1), 0)
        let (is, dists) =
            tabulate_2d
                n
                m
                (\i j ->
                    [
                        relax_one i j (i + 1) j,
                        relax_one i j i (j + 1),
                        relax_one (i + 1) j i j,
                        relax_one i (j + 1) i j
                    ])
            |> flatten_3d
            |> unzip
        in scatter_2d
            (copy d)
            is
            dists

    let distance = replicate_2d n m 9999i64
    let distance = distance with [0, 0] = 0
    let (distance, _) =
        loop (distance, changed) = (distance, true) while changed do
            let new_distance = relax distance
            let same =
                map2 (map2 (==)) distance new_distance
                |> flatten
                |> reduce (&&) true
            in (new_distance, !same)
    in distance

entry part1 (input: []u8) =
    parse input
    |> bellman_ford
    |> flatten
    |> last

let expand [n][m] (grid: [n][m]i64) =
    tabulate_2d
        (n * 5)
        (m * 5)
        (\i j -> (grid[i % n, j % m] + (i / n) + (j / m) - 1) % 9 + 1)

entry part2 (input: []u8) =
    parse input
    |> expand
    |> bellman_ford
    |> flatten
    |> last
