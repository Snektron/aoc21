import "util/aoc"
import "util/sorts/radix_sort"

module state = {
    -- Each amphipod takes up 4 bits
    type t = i64

    let invalid = -1i64

    let score_base = 48i64
    let first_move_base = 40i64
    let bits_per_node = 5i64
    let node_mask = 0x1Fi64

    let encode (nodes: [8]i64) =
        nodes
        |> map2 (\i v -> v << (i * bits_per_node)) (indices nodes)
        |> reduce (|) 0

    -- get the current node for amphipod i in [A, A, B, B, C, C, D, D]
    let get (i: i64) (s: t): i64 =
        (s >> (i * bits_per_node)) & node_mask

    let move (i: i64) (dst: i64) (s: t): t =
        let s = s & !(node_mask << (i * bits_per_node))
        in s | (dst << (i * bits_per_node)) | (1 << (i + first_move_base))

    let get_score (s: t): i64 =
        ((s >> score_base) & 0xFFFF)

    let add_score (score: i64) (s: t): t =
        let score = score + get_score s
        in if score > 0xFFFE then
            invalid
        else
            (s & 0xFFFF_FFFF_FFFF) | (score << score_base)

    let decode (s: t): [8]i64 =
        map (\i -> get i s) (iota 8)

    let is_target (s: t): bool =
        let a = [0, 0, 2, 2, 4, 4, 6, 6]
        let b = [1, 1, 3, 3, 5, 5, 7, 7]
        let nodes = decode s
        in all2 (||)
            (map2 (==) nodes a)
            (map2 (==) nodes b)

    let is_empty (node: i64) (s: t) =
        decode s
        |> all (!= node)

    let is_occupied (node: i64) (s: t) =
        decode s
        |> any (== node)

    let occupy_mask (s: t) =
        decode s
        |> map (1<<)
        |> reduce (|) 0

    let is_first_move (i: i64) (s: t): bool =
        (s >> (first_move_base + i)) & 1 == 0

    let set_moved (i: i64) (s: t): t =
        s | (1 << (i + first_move_base))
}

-- #############
-- #89.A.B.C.DE#
-- ###1#3#5#7###
--   #0#2#4#6#
--   #########
let part1_edges =
    let invl = (-1i64, 0i64)
        -- (dst, wt)
    in [
        [(1, 1), invl, invl, invl], -- 0
        [(0, 1), (9, 2), (10, 2), invl], -- 1
        [(3, 1), invl, invl, invl], -- 2
        [(2, 1), (10, 2), (11, 2), invl], -- 3
        [(5, 1), invl, invl, invl], -- 4
        [(4, 1), (11, 2), (12, 2), invl], -- 5
        [(7, 1), invl, invl, invl], -- 6
        [(6, 1), (12, 2), (13, 2), invl], -- 7
        [(9, 1), invl, invl, invl], -- 8
        [(8, 1), (1, 2), (10, 2), invl], -- 9
        [(1, 2), (3, 2), (9, 2), (11, 2)], -- A/10
        [(3, 2), (5, 2), (10, 2), (12, 2)], -- B/11
        [(5, 2), (7, 2), (11, 2), (13, 2)], -- C/12
        [(7, 2), (12, 2), (14, 1), invl], -- D/13
        [(13, 1), invl, invl, invl] -- E/14
    ]

let part1_n_nodes = 15i64

let part1_chambers: [][]i64 = [
    [0, 1], -- Amber can move to nodes 0, 1
    [2, 3], -- Bronze can move to nodes 2, 3
    [4, 5], -- Copper can move to nodes 4, 5
    [6, 7]  -- Desert can move to nodes 6, 7
]

let amphipod_move_wt: []i64 = [1, 10, 100, 1000]

-- | Returns for each edge (move mask, total weight)
let compute_valid_moves (start_node: i64): [part1_n_nodes](i64, i64) =
    let start_mask = 1 << start_node
    let iter (mask, src, wt) =
        map
            (\(dst, wt') ->
                if dst >= 0 && mask & (1 << dst) == 0 then -- If the move has not previously been seen
                    -- Move there
                    (mask | (1 << dst), dst, wt + wt')
                else
                    (-1, -1, -1))
            part1_edges[src]
    let start = (start_mask, start_node, 0)
    let (_, moves) =
        loop (current, moves) = ([start], []) while length current > 0 do
            let current' =
                map iter current
                |> flatten
                |> filter (\(_, node, _) -> node != -1)
            in (current', current ++ moves)
    -- We only want to keep the sortest path to each dst, so do a histogram on dst and keep the lowest weight
    in
        reduce_by_index
            (replicate part1_n_nodes (i64.highest, i64.highest))
            (\(a_mask, a_wt) (b_mask, b_wt) ->
                if a_wt < b_wt then
                    (a_mask, a_wt)
                else
                    (b_mask, b_wt))
            (i64.highest, i64.highest)
            (map (.1) moves)
            (map (\(mask, _, wt) -> (mask, wt)) moves)
        -- Clear source bit
        |> map (\(mask, wt) -> (mask & !start_mask, wt))

let part1_valid_moves: [part1_n_nodes][4][9](i64, i64, i64) = -- [src][amphipod][k](dst, mask, weight)
    let [n][m] chambers: [n][m]i64 = part1_chambers
    let k = part1_n_nodes - (n - 1) * m
    let f start =
        let moves = compute_valid_moves start
        in tabulate 4
            (\amphipod ->
                -- Can only move if either (1) the dst is the hall or (2) dst is its own chamber
                let moves' =
                    iota part1_n_nodes
                    |> filter
                        (\dst ->
                            let dst_in_hall = all (!=dst) (flatten chambers)
                            let dst_in_chamber = any (==dst) chambers[amphipod]
                            in dst_in_hall || dst_in_chamber)
                    |> map (\i -> (i, moves[i].0, moves[i].1 * amphipod_move_wt[amphipod]))
                in moves' :> [k](i64, i64, i64))
    in (tabulate part1_n_nodes f) :> [part1_n_nodes][4][9](i64, i64, i64)

let expand (s: state.t) =
    let empty_mask = (!(state.occupy_mask s)) & ((1 << part1_n_nodes) - 1)
    let can_do_move (mask: i64) =
        empty_mask & mask == mask
    let do_move i dst wt s =
        let s' = state.move i dst s
        in state.add_score wt s'
    let hall_mask = 0b0111_11110000_0000i64
    let node_amphipods =
        scatter
            (replicate part1_n_nodes (-1i64))
            (state.decode s)
            [0, 0, 1, 1, 2, 2, 3, 3]
    let may_move_to_chamber =
        tabulate 4
            (\amphipod ->
                all
                    (\i -> node_amphipods[i] == amphipod || node_amphipods[i] == -1)
                    part1_chambers[amphipod])
    in tabulate 8
        (\i ->
            let amphipod = i / 2
            let src = state.get i s
            let in_hall = hall_mask & (1 << src) != 0
            let moves = part1_valid_moves[src, amphipod]
            let mmtc = in_hall && may_move_to_chamber[amphipod]
            let is_first = state.is_first_move i s
            --  let performed_moves = 1 + state.get_moves i s
            --  let s' = state.set_moves i performed_moves s
            in map
                (\(dst, mask, wt) ->
                    -- If currently not in a hall, we may move to either the hall or a chamber
                    -- If currently in a hall, we may only move to a chamber.
                    -- Both the above only apply if the chamber is empty or only contains amphipods
                    -- of the right type
                    let dst_in_chamber = hall_mask & (1 << dst) == 0
                    in if dst == src then
                        state.invalid
                    -- If the destination is in a chamber:
                    -- - The chamber needs to be empty
                    -- - The path must be clear
                    else if dst_in_chamber && mmtc && can_do_move mask then
                        do_move i dst wt s
                    -- If the destination is not in the chamber:
                    -- - We must be already in a chamber.
                    -- - The path must be clear
                    -- - This must be the first move of the amphipod
                    else if !dst_in_chamber && !in_hall && is_first && can_do_move mask then
                        do_move i dst wt s
                    else
                        state.invalid)
                moves)

let dedup [n] (states: [n]state.t) =
    let sorted =
        radix_sort
            64
            (\bit s -> i64.get_bit bit s)
            states
    in
        iota n
        |> map (\i -> if i == 0 then false else sorted[i] == sorted[i - 1])
        |> map (\x -> !x)
        |> zip sorted
        |> filter (\(_, d) -> d)
        |> map (.0)

let dedup2 [n] (states: [n](state.t, i32)) =
    let sorted =
        radix_sort
            64
            (\bit (s, _) -> 1 - i64.get_bit bit s)
            states
    in
        iota n
        |> map (\i -> if i == 0 then false else sorted[i].0 == sorted[i - 1].0)
        |> map (\x -> !x)
        |> zip sorted
        |> filter (\(_, d) -> d)
        |> map (.0)

entry part1 (input: []u8) =
    let example = state.encode [2, 4, 1, 3, 0, 6, 5, 7]
    let (states, score) =
        loop (states, score) = ([example], 20_000) while (#[trace(states)] length states) > 0 do
            let score' =
                states
                |> map (\s -> if state.is_target s then state.get_score s else i64.highest)
                |> i64.minimum
            let states' =
                states
                |> map expand
                |> flatten_3d
                |> filter (\s -> s != state.invalid && state.get_score s < score)
                |> dedup
            in (states', #[trace(score)] (i64.min score score'))
    in (length states, score)
