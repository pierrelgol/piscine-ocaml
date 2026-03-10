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

let print_int_set label set =
  let rec print_elements = function
    | [] -> ()
    | [ x ] -> Printf.printf "%d" x
    | x :: xs ->
        Printf.printf "%d; " x;
        print_elements xs
  in
  Printf.printf "%s = [" label;
  print_elements (List.sort_uniq Stdlib.compare set);
  print_endline "]"

let () =
  let singleton = Set.return 1 in
  let a = Set.union singleton [ 2; 3; 3 ] in
  let b = [ 2; 4; 4 ] in
  let union_set = Set.union a b in
  let inter_set = Set.inter a b in
  let diff_set = Set.diff a b in
  let bind_set = Set.bind a (fun x -> [ x; x * 10; x ]) in
  let even_set = Set.filter bind_set (fun x -> x mod 2 = 0) in
  print_endline "Set tests:";
  print_int_set "return 1" singleton;
  print_int_set "a" a;
  print_int_set "b" b;
  print_int_set "union a b" union_set;
  print_int_set "inter a b" inter_set;
  print_int_set "diff a b" diff_set;
  print_int_set "bind a (x and x * 10)" bind_set;
  print_int_set "filter even" even_set;
  Printf.printf "foreach bind_set = ";
  Set.foreach bind_set (fun x -> Printf.printf "%d " x);
  print_newline ();
  Printf.printf "for_all positive = %b\n" (Set.for_all bind_set (fun x -> x > 0));
  Printf.printf "exists > 20 = %b\n" (Set.exists bind_set (fun x -> x > 20));
  Printf.printf "exists < 0 = %b\n" (Set.exists bind_set (fun x -> x < 0));
  Printf.printf "for_all even_set even = %b\n" (Set.for_all even_set (fun x -> x mod 2 = 0));
  print_int_set "inter disjoint" (Set.inter a [ 99; 100 ]);
  print_int_set "diff empty" (Set.diff [] a)
