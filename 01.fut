import "aoc"

-- ==
-- entry: part1
-- compiled random input { [10000000]i32 } auto output
entry part1 (input: []i32) =
    input
    |> in_pairs
    |> map (uncurry (<))
    |> map i32.bool
    |> reduce (+) 0

-- ==
-- entry: part2
-- compiled random input { [100000000]i32 } auto output
entry part2 (input: []i32) =
    input
    |> in_triplets
    |> map (\(a, b, c) -> a + b + c)
    |> part1
