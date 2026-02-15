type radar = float array * string

let eu_dist a b =
  let n = Array.length a in
  let acc = ref 0. in
  for i = 0 to n - 1 do
    let d = a.(i) -. b.(i) in
    acc := !acc +. (d *. d)
  done;
  sqrt !acc

let take k lst =
  let rec loop i acc = function
    | [] -> List.rev acc
    | _ when i <= 0 -> List.rev acc
    | x :: xs -> loop (i - 1) (x :: acc) xs
  in
  loop k [] lst

let k_nn training k (point, _) =
  match training with
  | [] -> ""
  | _ ->
      let with_dist =
        List.map (fun (v, c) -> (eu_dist point v, c)) training
        |> List.sort (fun (d1, _) (d2, _) -> Stdlib.compare d1 d2)
      in
      let nearest = take (max 1 k) with_dist in
      let rec count cls = function
        | [] -> 0
        | (_, c) :: xs -> (if c = cls then 1 else 0) + count cls xs
      in
      let rec classes acc = function
        | [] -> acc
        | (_, c) :: xs ->
            if List.mem c acc then classes acc xs else classes (c :: acc) xs
      in
      let uniq = classes [] nearest in
      let rec best best_c best_n = function
        | [] -> best_c
        | c :: xs ->
            let n = count c nearest in
            if n > best_n then best c n xs else best best_c best_n xs
      in
      match uniq with
      | [] -> ""
      | c :: xs -> best c (count c nearest) xs
