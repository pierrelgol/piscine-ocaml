let gray n =
  let rec append a b =
    match a with
    | [] -> b
    | x :: xs -> x :: append xs b
  in
  let rec build k =
    if k <= 0 then [ "" ]
    else
      let prev = build (k - 1) in
      let rec pref p = function
        | [] -> []
        | x :: xs -> (p ^ x) :: pref p xs
      in
      let left = pref "0" prev in
      let right = pref "1" (List.rev prev) in
      append left right
  in
  let seq = build n in
  print_endline (String.concat " " seq)
