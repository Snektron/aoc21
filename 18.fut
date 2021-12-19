import "util/aoc"

-- ==
-- entry: part1
-- input @ datasets/18.example.in output { 4140i32 }
-- input @ datasets/18.in output { 3884i32 }

-- ==
-- entry: part2
-- input @ datasets/18.example.in output { 3993i32 }
-- input @ datasets/18.in output { 4595i32 }

let parse (input: []u8) =
    let input = filter (!= ',') input
    in
        input
        |> map
            (\c ->
                match c
                case '[' -> 1i32
                case ']' -> -1
                case _ -> 0)
        |> scan (+) 0
        |> map (\d -> d - 1)
        |> zip input
        |> filter (\(c, _) -> (c >= '0' && c <= '9') || c == '\n')
        |> map (\(c, d) -> if c >= '0' then (i8.u8 c - '0', d) else (-1i8, d))
        |> unzip

let is_pair [n] (index: i64) (depths: [n]i32) =
    index + 1 < n && depths[index] == depths[index + 1]

let insert [n] 't (index: i64) (x: t) (xs: [n]t) =
    map (\i -> if i < index then xs[i] else if i == index then x else xs[i - 1]) (iota (n + 1))

let delete [n] 't (index: i64) (xs: [n]t) =
    map (\i -> if i < index then xs[i] else xs[i + 1]) (iota (n - 1))

let delete2 [n] 't 'u (index: i64) (xs: [n]t) (ys: [n]u) =
    zip xs ys
    |> delete index
    |> unzip

let update [n] 't (i: i64) (f: t -> t) (xs: *[n]t) =
    if i >= 0 && i < n then
        xs with [i] = copy (f xs[i])
    else
        xs

let snail_explode [n] (digits: *[n]i8) (depths: *[n]i32) =
    let i =
        iota n
        |> find_index (\i -> depths[i] == -1 || (depths[i] >= 4 && is_pair i depths))
    in if i == -1 || depths[i] == -1 then
        (false, digits, depths)
    else
        let left = digits[i]
        let right = digits[i + 1]
        let digits = update (i - 1) (+left) digits
        let digits = if digits[i + 2] != -1 then
            update (i + 2) (+right) digits
        else
            digits
        let depths = depths with [i] = depths[i] - 1
        let digits = digits with [i] = 0
        let (digits, depths) =
            zip digits depths
            |> delete (i + 1)
            |> unzip
        in (true, digits, depths)

let snail_split [n] (digits: *[n]i8) (depths: *[n]i32) =
    let i =
        iota n
        |> find_index (\i -> depths[i] == -1 || digits[i] >= 10)
    in if i == -1 || digits[i] == -1 then
        (false, digits, depths)
    else
        let left = digits[i] / 2
        let right = digits[i] - left
        let digits = digits with [i] = left
        let depths = depths with [i] = depths[i] + 1
        let (digits, depths) =
            zip digits depths
            |> insert (i + 1) (right, depths[i])
            |> unzip
        in (true, digits, depths)

let snail_reduce [n] (digits: *[n]i8) (depths: *[n]i32) =
    let (digits, depths, _) =
        loop (digits, depths, continue) = (digits, depths, true) while continue do
            let (exploded, digits, depths) = snail_explode (copy digits) (copy depths)
            in if exploded then
                (digits, depths, true)
            else
                let (splitted, digits, depths) = snail_split (copy digits) (copy depths)
                in if splitted then
                    (digits, depths, true)
                else
                    (digits, depths, false)
    in (digits, depths)

let snail_reduce_all [n] (digits: *[n]i8) (depths: *[n]i32) =
    let (digits, depths, _) =
        loop (digits, depths, continue) = (digits, depths, true) while continue do
            let index = find_index (== -1) (init depths)
            in if index < 0 then
                (digits, depths, false)
            else
                let (digits, depths) =
                    zip digits depths
                    |> delete index
                    |> unzip
                let index = find_index (== -1) depths
                let depths =
                    map2 (\d i -> if i < index then d + 1 else d) depths (indices depths)
                let (digits, depths) = snail_reduce (copy digits) (copy depths)
                in (digits, depths, true)
    in (digits, depths)

let magnitude [n] (digits: [n]i8) (depths: [n]i32) =
    let powers_of_three = [1, 3, 9, 27, 81]
    let m = n - 1
    let digits = digits[:m]
    let depths = depths[:m]
    let height = i32.maximum depths
    let reverse_depths = map (height-) depths
    let paths =
        reverse_depths
        |> map (1<<)
        |> scan (+) 0
        |> rotate (-1)
        |> map2
            (\i x -> if i == 0 then 0 else x)
            (iota m)
        |> map2
            (flip (>>))
            reverse_depths
        |> map i32.popc
        |> map2
            (\depth twos -> powers_of_three[depth + 1 - twos] << twos)
            depths
        |> map2 (*) (map i32.i8 digits)
        |> i32.sum
    in paths

entry part1 (input: []u8) =
    let (digits, depths) = parse input
    let (digits, depths) = snail_reduce_all (copy digits) (copy depths)
    in magnitude digits depths

entry part2 (input: []u8) =
    let (digits, depths) = parse input
    let offsets =
        depths
        |> map (== -1)
        |> rotate (-1)
        |> map2 (\i f -> if f then i else -1) (indices depths)
        |> filter (>=0)
    let n = length offsets
    let m = length digits
    let lengths =
        iota n
        |> map (\i -> (if i == n - 1 then m else offsets[i + 1]) - offsets[i] - 1)
    in
        tabulate_2d n n
            (\a b ->
                let off_a = offsets[a]
                let len_a = lengths[a]
                let off_b = offsets[b]
                let len_b = lengths[b] + 1 -- Include -1 terminator
                let k = len_a + len_b
                let digits' = concat_to k digits[off_a:off_a + len_a] digits[off_b:off_b + len_b]
                let depths' =
                    concat_to k depths[off_a:off_a + len_a] depths[off_b:off_b + len_b]
                    |> map (\x -> if x >= 0 then x + 1 else x)
                let (digits', depths') = snail_reduce digits' depths'
                let mag = magnitude digits' depths'
                in mag)
        |> flatten
        |> i32.maximum
