let crossover a b =
  let rec mem x = function
    | [] -> false
    | y :: ys -> y = x || mem x ys
  in
  let rec loop l acc =
    match l with
    | [] -> List.rev acc
    | x :: xs ->
        if mem x b && not (mem x acc) then loop xs (x :: acc) else loop xs acc
  in
  loop a []
