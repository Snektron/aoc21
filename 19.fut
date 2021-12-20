import "util/aoc"
import "util/sorts/radix_sort"
import "util/segmented"

-- ==
-- entry: part1
-- compiled input @ datasets/19.example.in output { 79i64 }
-- compiled input @ datasets/19.in output { 403i64 }

-- ==
-- entry: part2
-- compiled input @ datasets/19.example.in output { 3621i32 }
-- compiled input @ datasets/19.in output { 10569i32 }

let parse [n] (input: [n]u8) =
    let lines = split_lines input
    let flags = map (\(o, _) -> input[o] == '-' && input[o + 1] == '-') lines
    let is_meta =
        lines
        |> map (\(_, l) -> l == 0)
        |> map2 (||) flags
    let bps =
        is_meta
        |> map i64.bool
        |> map (1-)
        |> segmented_reduce (+) 0 flags
    let coords =
        is_meta
        |> map (\x -> !x)
        |> zip lines
        |> filter (.1)
        |> map (.0)
        |> map (split_fields input "," 3)
        |> map (map (parse_int input))
    let offsets =
        bps
        |> scan (+) 0
        |> rotate (-1)
        |> map2 (\i x -> if i == 0 then 0 else x) (indices bps)
    in (offsets, bps, coords)

let perms: [][3]i32 =
    let signs = [
        [ 1, 1, 1],
        [ 1,-1,-1],
        [-1,-1, 1],
        [-1, 1,-1]
    ]
    in tabulate 3
        (\i -> rotate i [1, 2, 3])
    |> map (\xs -> map (map2 (*) xs) signs)
    |> flatten
    |> map (\x -> [x, [x[0], -x[2], x[1]]])
    |> flatten

let invert_permutation (key: [3]i32) =
    let is = iota 3
        |> map i32.i64
        |> map (+1)
        |> map2 (\x y -> if x < 0 then -y else y) key
    in scatter
        (replicate 3 0)
        (map (\x -> i64.i32 (i32.abs x - 1)) key)
        is

let permute [n] (a: [3]i32) (b: [n][3]i32) =
    let bt = transpose b
    in
        map (\i -> map (*(i32.sgn i)) bt[i32.abs i - 1]) a
        |> transpose

let (a: [3]i32) >^^===- (b: [3]i32) =
    map (\i -> i32.sgn i * b[i32.abs i - 1]) a

let count_local [n][m] (as: [n][3]i32) (bs: [m][3]i32) =
    map
        (\a -> any (==a) bs)
        as
    |> map i32.bool
    |> i32.sum

let count_overlapping' [n][m] (as: [n][3]i32) (bs: [m][3]i32) =
    tabulate_2d n m
        (\i j ->
            let diff = map2 (-) as[i] bs[j]
            let bs' = map (map2 (+) diff) bs
            in (count_local as bs', diff))
    |> flatten
    |> reduce
        (\(c0, d0) (c1, d1) ->
            if c0 > c1 then
                (c0, d0)
            else
                (c1, d1))
        (i32.lowest, [0, 0, 0])

let count_overlapping [n][m] (as: [n][3]i32) (bs: [m][3]i32) =
    indices perms
        |> map i32.i64
        |> map (\key ->
            let bs' = permute perms[key] bs
            let (count, diff) = count_overlapping' as bs'
            in (count, diff, key))
        |> reduce
            (\(c0, d0, k0) (c1, d1, k1) ->
                if c0 > c1 then
                    (c0, d0, k0)
                else
                    (c1, d1, k1))
            (i32.lowest, [0, 0, 0], 0)

let dfs_order [n] (m: [n][n]bool) =
    let (order, _, _) =
        loop (order, stack, seen) = ([], [0], 1) while length stack > 0 do
            let top = last stack
            let stack = init stack
            let new =
                zip (iota n) m[top]
                |> filter (\(j, keep) -> keep && (seen & (1 << j)) == 0)
                |> map (.0)
            let seen =
                reduce
                    (|)
                    seen
                    (map (1<<) new)
            let order' = map (\x -> (top, x)) new
            in (order ++ order', stack ++ new, seen)
    in order

let coords_eq (a: [3]i32) (b: [3]i32) = map2 (==) a b |> reduce (&&) true

let dedup_coords [n] (coords: [n][3]i32) =
    let sorted =
        radix_sort
            (32 * 3)
            (\bit c -> i32.get_bit (bit % 32) c[bit / 32])
            coords
    in
        iota n
        |> map (\i -> if i == 0 then false else coords_eq sorted[i] sorted[i - 1])
        |> map (\x -> !x)
        |> zip sorted
        |> filter (\(_, d) -> d)
        |> map (.0)

let match_scanners [n] (offsets: [n]i64) (bps: [n]i64) (coords: [][3]i32) =
    let coords_for i = coords[offsets[i]:offsets[i]+bps[i]]
    let [k] pairs: [k](i64, i64) =
        tabulate_2d n n (\i j -> (i, j))
        |> flatten
        |> filter (\(i, j) -> j > i)
    let (counts, diffs: [][3]i32, keys) =
        (loop results = replicate k (-1, [0, 0, 0], -1) for ii < k do
            let (i, j) = pairs[ii]
            let results = results with [ii] = count_overlapping (coords_for i) (coords_for j)
            in results)
        |> unzip3
    let (pairs, _, diffs, keys) =
        zip4 pairs counts diffs keys
        |> filter (\(_, count, _, _) -> count >= 12)
        |> unzip4
    let diff_matrix =
        scatter_2d
            (replicate_2d n n [-1, -1, -1])
            pairs
            diffs
    let perm_matrix =
        scatter_2d
            (replicate_2d n n [0, 0, 0])
            pairs
            (map (\k -> copy perms[k]) keys)
    let invpairs = map (\(i, j) -> (j, i)) pairs
    let diff_matrix =
        scatter_2d
            diff_matrix
            invpairs
            (map2
                (\x k -> (invert_permutation perms[k]) >^^===- x |> map i32.neg)
                diffs
                keys)
    let perm_matrix =
        scatter_2d
            perm_matrix
            invpairs
            (map (\k -> invert_permutation perms[k]) keys)
    let order = dfs_order (map (map (\x -> x[0] != 0)) perm_matrix)
    let final_permutation =
        loop fp = replicate n perms[0] for (src, dst) in order do
            fp with [dst] = fp[src] >^^===- perm_matrix[src, dst]
    let final_coord =
        loop fc = replicate n [0, 0, 0] for (src, dst) in order do
            fc with [dst] = map2 (+) fc[src] (final_permutation[src] >^^===- diff_matrix[src, dst])
    in (final_permutation, final_coord)

entry part1 (input: []u8) =
    let (offsets, bps, coords) = parse input
    let [n] (sp: [n][3]i32, sc: [n][3]i32) = match_scanners offsets bps coords
    let coords_for i = coords[offsets[i]:offsets[i]+bps[i]]
    let beacons =
        loop as = coords_for 0 for i < n do
            let bs =
                coords_for i
                |> permute sp[i]
                |> map (map2 (+) sc[i])
            in dedup_coords (as ++ bs)
    in length beacons

entry part2 (input: []u8) =
    let (offsets, bps, coords) = parse input
    let (_, sc) = match_scanners offsets bps coords
    in
        cartesian
            (\a b ->
                map2 (-) a b
                |> map i32.abs
                |> i32.sum)
            sc
            sc
        |> i32.maximum
