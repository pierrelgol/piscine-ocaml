module Try = struct
  type 'a t = Success of 'a | Failure of exn

  let return x = Success x

  let bind m f =
    match m with
    | Failure e -> Failure e
    | Success x ->
        (try f x with e -> Failure e)

  let recover m f =
    match m with Success _ -> m | Failure e -> (try f e with e2 -> Failure e2)

  let filter m pred =
    match m with
    | Failure _ -> m
    | Success x -> if pred x then m else Failure (Failure "Try.filter")

  let flatten mm =
    match mm with
    | Failure e -> Failure e
    | Success (Failure e) -> Failure e
    | Success (Success x) -> Success x
end

let () =
  let open Try in
  let a = return 21 in
  let b = bind a (fun x -> return (x * 2)) in
  let c = filter b (fun x -> x = 42) in
  let d = bind c (fun _ -> raise (Failure "boom")) in
  let e = recover d (fun _ -> return 0) in
  let f = flatten (return e) in
  match f with Success x -> Printf.printf "Try result: %d\n" x | Failure _ -> print_endline "failure"
