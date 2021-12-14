import "util/aoc"

-- ==
-- entry: part1
-- input @ datasets/14.example.in output { 1588i64 }
-- input @ datasets/14.in output { 2321i64 }

-- ==
-- entry: part2
-- input @ datasets/14.example.in output { 2188189693529i64 }
-- input @ datasets/14.in output { 2399822193707i64 }

let parse (input: []u8) =
    let (template, input) = split (find_index (=='\n') input) input
    let to_index c = c - 'A'
    let template =
        template
        |> map to_index
        |> map i64.u8
    let is =
        template
        |> in_windows_of_pairs
    let template_mat =
        reduce_by_index_2d
            (replicate_2d 26 26 0i64)
            (+)
            0
            is
            (map (const 1) is)
    let (inputs, outputs) =
        input[2:]
        |> in_chunks 8
        |> map (map to_index)
        |> map (\l -> ((i64.u8 l[0], i64.u8 l[1]), i64.u8 l[6]))
        |> unzip
    let rules =
        scatter_2d
            (replicate_2d 26 26 0i64)
            inputs
            outputs
    in (rules, template_mat)

let pair_insertion (rules: [26][26]i64) (polymer: [26][26]i64) =
    let (is, vs) =
        tabulate_2d
            26
            26
            (\i j ->
                let x = polymer[i, j]
                let k = rules[i, j]
                in [((i, k), x), ((k, j), x)])
        |> flatten_3d
        |> unzip
    in
        reduce_by_index_2d
            (replicate_2d 26 26 0i64)
            (+)
            0
            is
            vs

let count_occurances (polymer: [26][26]i64) =
    let (is, vs) =
        tabulate_2d
            26
            26
            (\i j ->
                let x = polymer[i, j]
                in [(i, x), (j, x)])
        |> flatten_3d
        |> unzip
    in
        reduce_by_index
            (replicate 26 0i64)
            (+)
            0
            is
            vs
        |> map (\a -> (a + 1) / 2)

let polymerize (steps: i32) (rules: [26][26]i64) (template: [26][26]i64) =
    iterate
        steps
        (pair_insertion rules)
        template

let solve (steps: i32) (input: []u8) =
    let (rules, template) = parse input
    let polymer = polymerize steps rules template
    let counts = count_occurances polymer
    let counts = filter (>0) counts
    in i64.maximum counts - i64.minimum counts

entry part1 = solve 10
entry part2 = solve 40
