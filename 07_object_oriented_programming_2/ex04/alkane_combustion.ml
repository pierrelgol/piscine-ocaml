let gcd a b =
  let rec loop x y = if y = 0 then x else loop y (x mod y) in
  loop (abs a) (abs b)

class alkane_combustion (alkanes : Molecule.alkane list) =
  object (self)
    inherit Reaction.reaction

    val reactives : (Molecule.molecule * int) list =
      List.map (fun a -> ((a :> Molecule.molecule), 1)) alkanes

    method private totals =
      List.fold_left
        (fun (c, h) a -> (c + a#carbon_count, h + a#hydrogen_count))
        (0, 0) alkanes

    method private balanced_coeffs =
      let c, h = self#totals in
      let water = h / 2 in
      let oxy_num = 2 * c + water in
      if oxy_num mod 2 = 0 then (1, oxy_num / 2, c, water)
      else (2, oxy_num, 2 * c, 2 * water)

    method is_balanced =
      let (_, o2c, co2c, h2oc) = self#balanced_coeffs in
      let (c, h) = self#totals in
      let left_c = c in
      let left_h = h in
      let left_o = 2 * o2c in
      let right_c = co2c in
      let right_h = 2 * h2oc in
      let right_o = (2 * co2c) + h2oc in
      left_c = right_c && left_h = right_h && left_o = right_o

    method get_start =
      if not self#is_balanced then raise (Failure "Unbalanced reaction");
      let (mul, o2c, _, _) = self#balanced_coeffs in
      let scaled = List.map (fun (m, n) -> (m, n * mul)) reactives in
      scaled @ [ ((new Molecule.oxygen :> Molecule.molecule), o2c) ]

    method get_result =
      if not self#is_balanced then raise (Failure "Unbalanced reaction");
      let (_, _, co2c, h2oc) = self#balanced_coeffs in
      [ ((new Molecule.carbon_dioxide :> Molecule.molecule), co2c); ((new Molecule.water :> Molecule.molecule), h2oc) ]

    method balance = (self :> Reaction.reaction)
  end
