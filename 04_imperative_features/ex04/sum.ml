let sum a b = a +. b

let () =
  Printf.printf "sum 1.5 2.5 = %.1f\n" (sum 1.5 2.5);
  Printf.printf "sum 0.1 0.2 = %.1f\n" (sum 0.1 0.2)
