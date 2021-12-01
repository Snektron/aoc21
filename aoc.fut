let in_windows [n] 't (ws: i64) (as: [n]t): [][ws]t =
    tabulate (n - ws + 1) (\i -> as[i:i + ws] :> [ws]t)

let in_pairs [n] 't (as: [n]t): [](t, t) =
    tabulate (n - 1) (\i -> (as[i], as[i + 1]))

let in_triplets [n] 't (as: [n]t): [](t, t, t) =
    tabulate (n - 2) (\i -> (as[i], as[i + 1], as[i + 2]))
