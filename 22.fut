import "util/aoc"
import "util/sorts/radix_sort"

-- ==
-- entry: part1
-- compiled input @ datasets/22.example.in output { 474140i64 }
-- compiled input @ datasets/22.in output { 620241i64 }

-- ==
-- entry: part2
-- compiled input @ datasets/22.example.in output { 2758514936282235i64 }
-- compiled input @ datasets/22.in output { 1284561759639324i64 }

type aabb = {
    on: bool,
    min: [3]i64,
    max: [3]i64
}

type item = {
    box: aabb,
    i: i64
}

module aabb = {
    let intersection (a: aabb) (b: aabb): aabb =
        {
            on = b.on,
            min = map2 i64.max a.min b.min,
            max = map2 i64.min a.max b.max
        }

    let is_valid ({on = _, min, max}: aabb): bool =
        all2 (<) min max

    let dims ({on = _, min, max}: aabb): [3]i64 =
        map2 (-) max min

    let volume (box: aabb): i64 =
        dims box |> reduce (*) 1
}

let parse (input: []u8) =
    split_lines input
    |> map (split_fields input " xyz=,." 7)
    |> map (\fields -> (fields[0].1 == 2, (map (parse_int input >-> i64.i32) fields[1:]) :> [6]i64))
    |> map
        (\(on, coords) -> {
            on,
            min = [coords[0], coords[2], coords[4]],
            max = [coords[1] + 1, coords[3] + 1, coords[5] + 1]
        })

let intersections [n][m] (boxes: [n]aabb) (items: [m]item) =
    tabulate_2d m n (\i j -> (i, j))
    |> flatten
    |> filter (\(i, j) -> j > items[i].i) -- crude
    |> map
        (\(i, j) ->
            let box = aabb.intersection items[i].box boxes[j]
            in {box = box, i = j})
    |> filter (\item -> aabb.is_valid item.box)

let compute_volume (boxes: []aabb) =
    let items = map2 (\box i -> {box, i}) boxes (indices boxes)
    let (_, volume, _) =
        loop (items, volume, i) = (items, 0, 1) while length items > 0 do
        let volume' =
            items
            |> map (.box)
            |> map aabb.volume
            |> i64.sum
        in (intersections boxes items, volume + volume' * i, -i)
    in volume

let solve (boxes: []aabb) =
    let total_volume = compute_volume boxes
    let off_volume =
        map2 (\box i -> {box, i}) boxes (indices boxes)
        |> filter (\{box, i = _} -> !box.on)
        |> map
            (\(item: item) ->
                let negative_volume =
                    intersections boxes [item]
                    |> map (.box)
                    |> compute_volume
                in (aabb.volume item.box) - negative_volume)
        |> i64.sum
    in total_volume - off_volume

entry part1 (input: []u8) =
    parse input
    |> filter (\{on = _, max, min} -> all (<= 50) min && all (>= -50) max)
    |> solve

entry part2 (input: []u8) =
    parse input
    |> solve
