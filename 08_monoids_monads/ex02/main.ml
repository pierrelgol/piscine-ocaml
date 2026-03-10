module type MONOID = sig
  type element

  val zero1 : element
  val zero2 : element
  val mul : element -> element -> element
  val add : element -> element -> element
  val div : element -> element -> element
  val sub : element -> element -> element
end

module INT : MONOID with type element = int = struct
  type element = int

  let zero1 = 0
  let zero2 = 1
  let mul = ( * )
  let add = ( + )
  let div = ( / )
  let sub = ( - )
end

module FLOAT : MONOID with type element = float = struct
  type element = float

  let zero1 = 0.
  let zero2 = 1.
  let mul = ( *. )
  let add = ( +. )
  let div = ( /. )
  let sub = ( -. )
end

module Calc (M : MONOID) = struct
  let add = M.add
  let sub = M.sub
  let mul = M.mul
  let div = M.div

  let rec power x n = if n <= 0 then M.zero2 else M.mul x (power x (n - 1))

  let rec fact x =
    if Stdlib.compare x M.zero1 <= 0 then M.zero2
    else M.mul x (fact (M.sub x M.zero2))
end

module Calc_int = Calc (INT)
module Calc_float = Calc (FLOAT)

let print_int_case label value =
  Printf.printf "%s = %d\n" label value

let print_float_case label value =
  Printf.printf "%s = %.2f\n" label value

let () =
  print_endline "Calc_int tests:";
  print_int_case "zero1" INT.zero1;
  print_int_case "zero2" INT.zero2;
  print_int_case "add 20 22" (Calc_int.add 20 22);
  print_int_case "sub 20 22" (Calc_int.sub 20 22);
  print_int_case "mul 6 7" (Calc_int.mul 6 7);
  print_int_case "div 84 2" (Calc_int.div 84 2);
  print_int_case "power 3 0" (Calc_int.power 3 0);
  print_int_case "power 3 1" (Calc_int.power 3 1);
  print_int_case "power 3 3" (Calc_int.power 3 3);
  print_int_case "fact 0" (Calc_int.fact 0);
  print_int_case "fact 1" (Calc_int.fact 1);
  print_int_case "fact 5" (Calc_int.fact 5);
  print_endline "Calc_float tests:";
  print_float_case "zero1" FLOAT.zero1;
  print_float_case "zero2" FLOAT.zero2;
  print_float_case "add 20.50 21.50" (Calc_float.add 20.50 21.50);
  print_float_case "sub 20.50 21.50" (Calc_float.sub 20.50 21.50);
  print_float_case "mul 6.00 7.00" (Calc_float.mul 6.00 7.00);
  print_float_case "div 84.00 2.00" (Calc_float.div 84.00 2.00);
  print_float_case "power 3.00 0" (Calc_float.power 3.00 0);
  print_float_case "power 3.00 1" (Calc_float.power 3.00 1);
  print_float_case "power 3.00 3" (Calc_float.power 3.00 3);
  print_float_case "fact 0.00" (Calc_float.fact 0.00);
  print_float_case "fact 1.00" (Calc_float.fact 1.00);
  print_float_case "fact 5.00" (Calc_float.fact 5.00)
