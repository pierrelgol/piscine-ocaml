module Set = struct
  type 'a t = 'a list

  let normalize s = List.sort_uniq Stdlib.compare s

  let return x = [ x ]

  let bind s f =
    let rec flat acc = function [] -> acc | x :: xs -> flat (acc @ f x) xs in
    normalize (flat [] s)

  let union a b = normalize (a @ b)
  let inter a b = List.filter (fun x -> List.mem x b) (normalize a) |> normalize
  let diff a b = List.filter (fun x -> not (List.mem x b)) (normalize a) |> normalize
  let filter s p = List.filter p (normalize s)
  let foreach s f = List.iter f (normalize s)
  let for_all s p = List.for_all p (normalize s)
  let exists s p = List.exists p (normalize s)
end

let () =
  let open Set in
  let a = union (return 1) [ 2; 3; 3 ] in
  let b = [ 2; 4 ] in
  let c = inter a b in
  let d = diff a b in
  let e = bind a (fun x -> [ x; x * 10 ]) in
  foreach e (fun x -> Printf.printf "%d " x);
  print_newline ();
  Printf.printf "inter size=%d diff size=%d\n" (List.length c) (List.length d);
  Printf.printf "all positive=%b any > 20=%b\n" (for_all e (fun x -> x > 0))
    (exists e (fun x -> x > 20));
  ignore (filter e (fun x -> x mod 2 = 0))
