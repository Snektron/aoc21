import "util/aoc"

-- ==
-- entry: part1
-- input @ datasets/17.example.in output { 45i32 }
-- input @ datasets/17.in output { 3570i32 }

-- ==
-- entry: part2
-- input @ datasets/17.example.in output { 112i64 }
-- input @ datasets/17.in output { 1919i64 }

let parse [n] (input: [n]u8) =
    split_fields input "targe ar: x=.,y\n" 4 (0, i32.i64 n)
    |> map (parse_int input)

let is_in_target (target: [4]i32) (x: i32) (y: i32) =
    x >= target[0] && x <= target[1] && y >= target[2] && y <= target[3]

let simulate (target: [4]i32) (vx: i32) (vy: i32) =
    let maxit = 200
    let (_, _, _, _, i, maxy) =
        loop (x, y, vx, vy, i, maxy) = (0i32, 0i32, vx, vy, 0, 0) while !(is_in_target target x y) && i < maxit do
            let x = x + vx
            let y = y + vy
            let vx = vx - i32.sgn vx
            let vy = vy - 1
            let maxy = i32.max y maxy
            in (x, y, vx, vy, i + 1, maxy)
    in if i == maxit then -1 else maxy

entry part1 (input: []u8) =
    let target = parse input
    in
        tabulate_2d
            (i64.i32 target[1] + 1)
            100
            (\vx vy -> simulate target (i32.i64 vx) (i32.i64 vy))
        |> flatten
        |> i32.maximum

entry part2 (input: []u8) =
    let target = parse input
    in
        tabulate_2d
            (i64.i32 target[1] + 1)
            200
            (\vx vy -> simulate target (i32.i64 vx) (i32.i64 vy - 100))
        |> flatten
        |> filter (>=0)
        |> length
