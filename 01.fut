import "aoc"

entry part1 (input: []i32) =
    input
    |> in_pairs
    |> map (uncurry (<))
    |> map i32.bool
    |> reduce (+) 0

entry part2 (input: []i32) =
    input
    |> in_windows 3
    |> map (reduce (+) 0)
    |> part1
