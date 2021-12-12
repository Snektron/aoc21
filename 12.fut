import "util/aoc"
import "util/sorts/radix_sort"

-- ==
-- entry: part1
-- input @ datasets/12.example.1.in output { 10i32 }
-- input @ datasets/12.example.2.in output { 19i32 }
-- input @ datasets/12.example.3.in output { 226i32 }
-- input @ datasets/12.in output { 5178i32 }

-- ==
-- entry: part2
-- input @ datasets/12.example.1.in output { 36i32 }
-- input @ datasets/12.example.2.in output { 103i32 }
-- input @ datasets/12.example.3.in output { 3509i32 }
-- input @ datasets/12.in output { 130094i32 }

let start = 0i32
let end = 1i32

let parse (input: []u8) =
    let edges =
        split_lines input
        |> map (split_fields input "-" 2)
        |> flatten
        |> map
            (\(off, len) ->
                let is_upper = input[off] <= 'Z'
                let value =
                    loop value = 0i32 for i < len do
                        value * 26 + i32.u8 (input[i + off] & !0x20) - 'A'
                in if value == 8559973 then 0 -- start
                else if value == 3045 then 1 -- end
                else if is_upper then -value - 1 -- Make large cases negative
                else value + 2) -- normal caves. Don't forget to account for start and end.
    -- Let's invent some new labelling to make all edges nice and packed.
    -- Note: putting 0 and 1 at the front ensures that they stay the same, provided we use a non-integer sort.
    let n = length edges
    let order =
        iota n
        |> map i32.i64
        |> radix_sort 32 (\bit index -> i32.get_bit bit edges[index])
    let vs =
        -- Check if the current is same as the previous
        iota n
        |> map (\i -> i == 0 || edges[order[i]] != edges[order[i - 1]])
        -- Exclusive scan to get the new label
        |> map i32.bool
        |> scan (+) 0
        |> map (\x -> x - 1)
    let num_small = vs[order |> map (\i -> edges[i]) |> find_index (<0)]
    let edges =
        -- Unsort to get the new id
        scatter
            (replicate n 0i32)
            (map i64.i32 order)
            vs
        |> in_chunks 2
        |> map (\x -> (i64.i32 x[0], i64.i32 x[1]))
    -- Construct the adjacency matrix
    let m = 1i64 + i64.i32 (last vs)
    let k = length edges
    let adj =
        scatter_2d
            (replicate (m * m) false |> unflatten m m)
            (edges :> [k](i64, i64))
            (replicate k true)
    let adj =
        transpose adj
        |> map2 (map2 (||)) adj
    in (i64.i32 num_small, adj)

let count_paths [n] 't
    (advance_one: t -> [n]t)
    (is_at_end: t -> bool)
    (is_valid: t -> bool)
    (initial: t): i32 =
    let (_, paths) =
        loop (agents, paths) = ([initial], 0) while length agents > 0 do
            let next = map advance_one agents
                |> flatten
            let n_at_end =
                next
                |> map is_at_end
                |> map i32.bool
                |> i32.sum
            in (filter is_valid next, n_at_end + paths)
    in paths

let solve1 [n] (n_small: i64) (mat: [n][n]bool) =
    let is_large i = i >= (i32.i64 n_small)
    let advance_one src =
        map2
            (\path dst ->
                if path && (is_large dst || u64.get_bit dst src.seen == 0) then
                    {node = dst, seen = u64.set_bit dst src.seen 1}
                else
                    {node = -1, seen = 0})
            (mat[src.node])
            (iota n |> map i32.i64)
    let is_at_end src = src.node == end
    let is_valid src = src.node >= 0 && src.node != end
    in count_paths advance_one is_at_end is_valid {node = start, seen = 1}

entry part1 (input: []u8) =
    let (n_small, adj) = parse input
    in solve1 n_small adj

let solve2 [n] (n_small: i64) (mat: [n][n]bool) =
    let is_large i = i >= (i32.i64 n_small)
    let null_agent = {node = -1, seen = 1, double = false}
    let advance_one src =
        map2
            (\path dst ->
                if !path then
                    null_agent
                else if is_large dst then
                    {node = dst, seen = src.seen, double = src.double}
                else if u64.get_bit dst src.seen == 0 then
                    {node = dst, seen = u64.set_bit dst src.seen 1, double = src.double}
                else if !src.double && dst != end && dst != start then
                    {node = dst, seen = src.seen, double = true}
                else null_agent)
            (mat[src.node])
            (iota n |> map i32.i64)
    let is_at_end src = src.node == end
    let is_valid src = src.node >= 0 && src.node != end
    in count_paths advance_one is_at_end is_valid {node = start, seen = 1, double = false}

entry part2 (input: []u8) =
    let (n_small, adj) = parse input
    in solve2 n_small adj
