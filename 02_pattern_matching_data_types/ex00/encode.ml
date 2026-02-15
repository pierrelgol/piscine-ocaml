let encode lst =
  let rec loop current count rest acc =
    match (current, rest) with
    | None, [] -> List.rev acc
    | Some v, [] -> List.rev ((count, v) :: acc)
    | None, x :: xs -> loop (Some x) 1 xs acc
    | Some v, x :: xs when x = v -> loop (Some v) (count + 1) xs acc
    | Some v, x :: xs -> loop (Some x) 1 xs ((count, v) :: acc)
  in
  loop None 0 lst []
