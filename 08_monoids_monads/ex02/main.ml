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

let () =
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
  print_endline (string_of_int (Calc_int.fact 5));
  print_endline (string_of_float (Calc_float.fact 5.0))
