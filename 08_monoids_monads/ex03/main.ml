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
    | Success x -> if pred x then m else Failure (Failure "Try.filter predicate failed")

  let flatten mm =
    match mm with
    | Failure e -> Failure e
    | Success (Failure e) -> Failure e
    | Success (Success x) -> Success x
end

let print_try_int label value =
  match value with
  | Try.Success x -> Printf.printf "%s = Success %d\n" label x
  | Try.Failure e -> Printf.printf "%s = Failure %s\n" label (Printexc.to_string e)

let () =
  let value = Try.return 21 in
  let bind_success = Try.bind value (fun x -> Try.return (x * 2)) in
  let bind_failure =
    Try.bind (Try.Failure (Failure "already failed")) (fun x -> Try.return (x * 2))
  in
  let bind_raised =
    Try.bind bind_success (fun _ -> raise (Failure "boom during bind"))
  in
  let recover_success = Try.recover bind_success (fun _ -> Try.return 0) in
  let recover_failure = Try.recover bind_raised (fun _ -> Try.return 0) in
  let filter_success = Try.filter bind_success (fun x -> x = 42) in
  let filter_failure = Try.filter bind_success (fun x -> x < 0) in
  let flatten_success = Try.flatten (Try.return recover_failure) in
  let flatten_nested_failure =
    Try.flatten (Try.return (Try.Failure (Failure "nested failure")))
  in
  let flatten_failure = Try.flatten (Try.Failure (Failure "outer failure")) in
  print_endline "Try tests:";
  print_try_int "return 21" value;
  print_try_int "bind success" bind_success;
  print_try_int "bind on failure" bind_failure;
  print_try_int "bind raising exception" bind_raised;
  print_try_int "recover on success" recover_success;
  print_try_int "recover on failure" recover_failure;
  print_try_int "filter success" filter_success;
  print_try_int "filter failure" filter_failure;
  print_try_int "flatten Success (Success x)" flatten_success;
  print_try_int "flatten Success (Failure e)" flatten_nested_failure;
  print_try_int "flatten Failure e" flatten_failure
