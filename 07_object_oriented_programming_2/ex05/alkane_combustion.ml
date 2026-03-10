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

let complete_state grouped_alkanes =
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
      let balanced_start, balanced_result = complete_state grouped_alkanes in
      (new alkane_combustion ~state:(Some (balanced_start, balanced_result)) alkanes
        :> Reaction.reaction)

    method get_incomplete_results : (int * (Molecule.molecule * int) list) list =
      let carbon_total, hydrogen_total = totals grouped_alkanes in
      let water_amount = hydrogen_total / 2 in
      let complete_start, _ = complete_state grouped_alkanes in
      let complete_oxygen_amount =
        List.fold_left
          (fun current (molecule, amount) ->
            if molecule#formula = "O2" then amount else current)
          0 complete_start
      in
      let raw_start oxygen_amount =
        (List.map
           (fun (alkane, amount) -> ((alkane :> Molecule.molecule), amount))
           grouped_alkanes)
        @ [ ((new Molecule.oxygen :> Molecule.molecule), oxygen_amount) ]
      in
      let rec scan carbon_dioxide_amount carbon_monoxide_amount acc =
        if carbon_dioxide_amount < 0 then acc
        else if carbon_monoxide_amount > carbon_total then
          scan (carbon_dioxide_amount - 1) 0 acc
        else
          let soot_amount =
            carbon_total - carbon_dioxide_amount - carbon_monoxide_amount
          in
          if soot_amount < 0 then
            scan (carbon_dioxide_amount - 1) 0 acc
          else
            let oxygen_atoms =
              (2 * carbon_dioxide_amount) + carbon_monoxide_amount + water_amount
            in
            if oxygen_atoms mod 2 <> 0 then
              scan carbon_dioxide_amount (carbon_monoxide_amount + 1) acc
            else
              let oxygen_amount = oxygen_atoms / 2 in
              let products =
                [
                  ((new Molecule.carbon_dioxide :> Molecule.molecule), carbon_dioxide_amount);
                  ((new Molecule.carbon_monoxide :> Molecule.molecule), carbon_monoxide_amount);
                  ((new Molecule.carbon :> Molecule.molecule), soot_amount);
                  ((new Molecule.water :> Molecule.molecule), water_amount);
                ]
                |> List.filter (fun (_, amount) -> amount > 0)
              in
              let next_acc =
                if oxygen_amount < complete_oxygen_amount
                   && (carbon_monoxide_amount > 0 || soot_amount > 0)
                   && self#protected_atom_counts (raw_start oxygen_amount)
                      = self#protected_atom_counts products
                then (oxygen_amount, products) :: acc
                else acc
              in
              scan carbon_dioxide_amount (carbon_monoxide_amount + 1) next_acc
      in
      scan carbon_total 0 []
      |> List.sort (fun (left, _) (right, _) -> compare left right)
  end
