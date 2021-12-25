import "util/aoc"

let parse (input: []u8) =
    input
    |> split_regular_lines

let move_east [n][m] (state: [n][m]u8) =
    tabulate_2d n m
        (\i j ->
            let prev = (j - 1 + m) % m
            let next = (j + 1) % m
            in if state[i, j] == '.' && state[i, prev] == '>' then
                '>'
            else if state[i, j] == '>' && state[i, next] == '.' then
                '.'
            else
                state[i, j])

let move_south [n][m] (state: [n][m]u8) =
    tabulate_2d n m
        (\i j ->
            let prev = (i - 1 + n) % n
            let next = (i + 1) % n
            in if state[i, j] == '.' && state[prev, j] == 'v' then
                'v'
            else if state[i, j] == 'v' && state[next, j] == '.' then
                '.'
            else
                state[i, j])

let step = move_east >-> move_south

entry part1 (input: []u8) =
    let initial = parse input
    let (_, i) =
        loop (state, i) = (initial, 0i64) while i >= 0 do
            let state' = step state
            let equal = all2 (all2 (==)) state state'
            let i' = if equal then -i - 1 else i + 1
            in (state', i')
    in -i
