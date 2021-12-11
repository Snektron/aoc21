import "util/aoc"


-- ==
-- entry: part1
-- input @ datasets/11.example.in output { 1656i32 }
-- input @ datasets/11.in output { 1741i32 }

-- ==
-- entry: part2
-- input @ datasets/11.example.in output { 195i32 }
-- input @ datasets/11.in output { 440i32 }

let parse (input: []u8) =
    split_regular_lines input
    |> map (map (\c -> c - '0'))
    |> map (map i32.u8)

let convolve [n] [m] (oc: [n][m]i32) =
    let f i j = if i < 0 || i >= n || j < 0 || j >= m then 0 else i32.bool (oc[i, j] > 9)
    in tabulate_2d
        n
        m
        (\i j ->
            let x = oc[i, j]
            in if x == 0 || x > 9 then 0 else
                  f (i - 1) (j - 1)
                + f (i - 1) j
                + f (i - 1) (j + 1)
                + f i (j - 1)
                + x
                + f i (j + 1)
                + f (i + 1) (j - 1)
                + f (i + 1) j
                + f (i + 1) (j + 1))

let flash [n] [m] (oc: [n][m]i32) =
    oc
    |> map (map (+1))
    |> iterate_while
        (\oc -> flatten oc |> any (>9))
        convolve

entry part1 (input: []u8) =
    (iterate
        100
        (\(oc, total) ->
            let oc = flash oc
            let count = oc |> flatten |> map (==0) |> map i32.bool |> i32.sum
            in (oc, count + total))
        (parse input, 0)).1

entry part2 (input: []u8) =
    let oc = parse input
    let (x, _) =
        loop (i, oc) = (0i32, oc) while i >= 0 do
            let oc = flash oc
            let same = oc |> flatten |> map (== oc[0, 0]) |> reduce (&&) true
            in if same then (-i - 1, oc) else (i + 1, oc)
    in -x
