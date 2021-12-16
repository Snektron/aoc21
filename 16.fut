import "util/aoc"

-- ==
-- entry: part1
-- input @ datasets/16.in output { 895i64 }

-- ==
-- entry: part2
-- input @ datasets/16.in output { 1148595959144i64 }

type packet = {
    ver: u8,
    type_id: u8,
    -- The length of the packet in bits, not counting sub packets
    len: i64,
     -- if type_id == 4, this holds the int value
     -- else, this holds the total length in bits (positive) or in fields (negative)
    data: i64
}

let parse (input: []u8) =
    let bits =
        input
        |> map (\c -> match c
            case '0' -> [0, 0, 0, 0u8]
            case '1' -> [0, 0, 0, 1]
            case '2' -> [0, 0, 1, 0]
            case '3' -> [0, 0, 1, 1]
            case '4' -> [0, 1, 0, 0]
            case '5' -> [0, 1, 0, 1]
            case '6' -> [0, 1, 1, 0]
            case '7' -> [0, 1, 1, 1]
            case '8' -> [1, 0, 0, 0]
            case '9' -> [1, 0, 0, 1]
            case 'A' -> [1, 0, 1, 0]
            case 'B' -> [1, 0, 1, 1]
            case 'C' -> [1, 1, 0, 0]
            case 'D' -> [1, 1, 0, 1]
            case 'E' -> [1, 1, 1, 0]
            case  _  -> [1, 1, 1, 1])
        |> flatten
    let get_int offset len =
        loop value = 0i64 for i < len do
            value * 2 + i64.u8 bits[offset + i]
    let get_var_int offset =
        let (value, offset) =
            loop (value, offset) = (0i64, offset) while (bits[offset]) == 1 do
                (value * 16 + get_int (offset + 1) 4, offset + 5)
        in (value * 16 + get_int (offset + 1) 4, offset + 5)
    let (packets, _) =
        loop (packets, offset) = ([], 0i64) while length bits - offset >= 11 do
            let start = offset
            let ver = u8.i64 (get_int offset 3)
            let type_id = u8.i64 (get_int (offset + 3) 3)
            let (data, offset) = if type_id == 4 then
                get_var_int (offset + 6)
            else
                let length_type_id = bits[offset + 6]
                let offset = offset + 7
                in if length_type_id == 0 then
                    (get_int offset 15, offset + 15)
                else
                    (-(get_int offset 11), offset + 11)
            let len = offset - start
            in (packets ++ [{ver, type_id, len, data}], offset)
    in packets

entry part1 [n] (input: [n]u8) =
    parse input[:n - 1]
    |> map (.ver)
    |> map i64.u8
    |> i64.sum

let resolve_parents_linear [n] (packets: [n]packet) (offsets: [n]i64) =
    let (stack, parents) =
        loop (stack, parents) = ([(-1i64, -1i64)], replicate n 0i64) for i < n do
            let (parent, top) = last stack
            -- Pop a single field counter, if present
            let stack = if top == -1 then
                init stack
            else if top < 0 then
                let n = length stack - 1
                in stack with [n] = (parent, top + 1)
            else
                stack
            -- Pop as many bit offsets as required.
            let stack = loop stack = stack while length stack > 0 && (last stack).1 == offsets[i] + packets[i].len do
                init stack
            let stack = if packets[i].type_id == 4 then
                stack
            else if packets[i].data < 0 then
                stack ++ [(i, packets[i].data)]
            else
                stack ++ [(i, packets[i].data + offsets[i] + packets[i].len)]
            in (stack, parents with [i] = parent)
    in assert (length stack == 0) parents

let evaluate [n] (packets: [n]packet) (parents: [n]i64) =
    let num_children =
        reduce_by_index
            (replicate n 0i64)
            (+)
            0
            parents
            (replicate n 1i64)
    let step (packets: [n]packet) (parents: [n]i64) (values: [n]i64) (is_resolved: [n]bool) =
        -- Mask of parents which are going to be resolved this iteration
        let to_resolve =
            reduce_by_index
                (replicate n 0i64)
                (+)
                0
                parents
                (map i64.bool is_resolved)
            |> map2 (==) num_children
        let is = map (\parent -> if parent >= 0 && to_resolve[parent] && !is_resolved[parent] then parent else -1) parents
        let ops = map (\i -> if i < 0 then 99 else packets[i].type_id) is
        let vs =
            zip3
            (iota n)
            (ops)
            values
        let apply op a b =
            match op
            case 0 -> a + b
            case 1 -> a * b
            case 2 -> i64.min a b
            case 3 -> i64.max a b
            case 5 -> i64.bool (a > b)
            case 6 -> i64.bool (a < b)
            case _ -> assert (op == 7) (i64.bool (a == b))
        let f (a_i, a_op, a_v) (b_i, b_op, b_v) =
            if a_i < 0 then
                (b_i, b_op, b_v)
            else if b_i < 0 then
                (a_i, a_op, a_v)
            else if a_i < b_i then
                (a_i, a_op, apply a_op a_v b_v)
            else
                (a_i, a_op, apply a_op b_v a_v)
        let values =
            reduce_by_index
                (zip3
                    (replicate n (-1))
                    (map (.type_id) packets)
                    (copy values))
                f
                (-1, 0, 0)
                is
                vs
            |> map (.2)
        in (values, to_resolve)

    let is_resolved =
        packets
        |> map (.type_id)
        |> map (== 4)
    let values = map (\p -> if p.type_id == 4 then p.data else 0) packets
    let (values, _) =
        loop (values, is_resolved) = (values, is_resolved) while !is_resolved[0] do
            step packets parents values is_resolved
    in values[0]

entry part2 [n] (input: [n]u8) =
    let packets = parse input[:n - 1]
    let offsets =
        packets
        |> map (.len)
        |> scan (+) 0
        |> rotate (-1)
        |> map2 (\i x -> if i == 0 then 0 else x) (indices packets)
    in resolve_parents_linear packets offsets
        |> evaluate packets
