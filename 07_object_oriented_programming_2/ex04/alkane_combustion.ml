let rec gcd left right =
  if right = 0 then abs left else gcd right (left mod right)

let gcd_list values =
  List.fold_left
    (fun current value -> if current = 0 then value else gcd current value)
    0 values

let combine_alkanes (alkanes : Molecule.alkane list) =
  let rec add alkane grouped =
    match grouped with
    | [] -> [ (alkane, 1) ]
    | (existing, amount) :: tail when existing#equals (alkane :> Molecule.molecule) ->
        (existing, amount + 1) :: tail
    | head :: tail -> head :: add alkane tail
  in
  List.fold_left (fun grouped alkane -> add alkane grouped) [] alkanes

let totals grouped_alkanes =
  List.fold_left
    (fun (carbon_total, hydrogen_total) (alkane, amount) ->
      ( carbon_total + (amount * alkane#carbon_count),
        hydrogen_total + (amount * alkane#hydrogen_count) ))
    (0, 0) grouped_alkanes

let balanced_state grouped_alkanes =
  let carbon_total, hydrogen_total = totals grouped_alkanes in
  let water_amount = hydrogen_total / 2 in
  let oxygen_atoms = (2 * carbon_total) + water_amount in
  let multiplier = if oxygen_atoms mod 2 = 0 then 1 else 2 in
  let oxygen_amount = if multiplier = 1 then oxygen_atoms / 2 else oxygen_atoms in
  let scaled_alkanes =
    List.map
      (fun (alkane, amount) ->
        ((alkane :> Molecule.molecule), amount * multiplier))
      grouped_alkanes
  in
  let scaled_result =
    [
      ((new Molecule.carbon_dioxide :> Molecule.molecule), carbon_total * multiplier);
      ((new Molecule.water :> Molecule.molecule), water_amount * multiplier);
    ]
  in
  let all_coefficients =
    oxygen_amount
    :: List.map snd scaled_result
    @ List.map snd scaled_alkanes
  in
  let divisor = gcd_list all_coefficients in
  let normalize side =
    List.map (fun (molecule, amount) -> (molecule, amount / divisor)) side
  in
  ( normalize
      (scaled_alkanes @ [ ((new Molecule.oxygen :> Molecule.molecule), oxygen_amount) ]),
    normalize scaled_result )

class alkane_combustion
  ?(state :
      ((Molecule.molecule * int) list * (Molecule.molecule * int) list) option =
      None)
  (alkanes : Molecule.alkane list) =
  let grouped_alkanes = combine_alkanes alkanes in
  object (self)
    inherit
      Reaction.reaction
        (List.map (fun (alkane, amount) -> ((alkane :> Molecule.molecule), amount)) grouped_alkanes)
        []

    method private state = state
    method is_balanced = match self#state with Some _ -> true | None -> false

    method get_start =
      match self#state with
      | Some (start, _) -> start
      | None -> raise (Failure "Reaction is not balanced")

    method get_result =
      match self#state with
      | Some (_, result) -> result
      | None -> raise (Failure "Reaction is not balanced")

    method balance =
      let balanced_start, balanced_result = balanced_state grouped_alkanes in
      (new alkane_combustion ~state:(Some (balanced_start, balanced_result)) alkanes
        :> Reaction.reaction)
  end
