module type FRACTIONNAL_BITS = sig
  val bits : int
end

module type FIXED = sig
  type t

  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool
  val eqs : t -> t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type MAKE = functor (B : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE = functor (B : FRACTIONNAL_BITS) -> struct
  type t = int

  let scale = 1 lsl B.bits
  let of_float f = int_of_float (f *. float_of_int scale)
  let of_int n = n * scale
  let to_float x = float_of_int x /. float_of_int scale
  let to_int x = x / scale
  let to_string x = string_of_float (to_float x)
  let zero = 0
  let one = of_int 1
  let succ x = x + 1
  let pred x = x - 1
  let min a b = if a <= b then a else b
  let max a b = if a >= b then a else b
  let gth a b = a > b
  let lth a b = a < b
  let gte a b = a >= b
  let lte a b = a <= b
  let eqp a b = a == b
  let eqs a b = a = b
  let add a b = a + b
  let sub a b = a - b
  let mul a b = (a * b) / scale
  let div a b = (a * scale) / b

  let foreach a b f =
    let rec loop x =
      if x > b then ()
      else (
        f x;
        loop (succ x)
      )
    in
    loop a
end

module Fixed4 : FIXED = Make (struct
  let bits = 4
end)

module Fixed8 : FIXED = Make (struct
  let bits = 8
end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach Fixed4.zero Fixed4.one (fun f -> print_endline (Fixed4.to_string f));
  print_endline ("Fixed4.max test: " ^ Fixed4.to_string (Fixed4.max Fixed4.zero Fixed4.one));
  print_endline ("Fixed8.mul test: " ^ Fixed8.to_string (Fixed8.mul x8 y8));
  print_endline ("Fixed8.div test: " ^ Fixed8.to_string (Fixed8.div y8 x8))
