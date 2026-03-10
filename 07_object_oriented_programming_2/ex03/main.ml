class sample_reaction ?balanced_version s r =
  object (self)
    inherit Reaction.reaction s r
    method get_start =
      if not self#is_balanced then failwith "Reaction is not balanced";
      s

    method get_result =
      if not self#is_balanced then failwith "Reaction is not balanced";
      r

    method balance =
      match balanced_version with
      | Some reaction -> reaction
      | None -> (self :> Reaction.reaction)
    method is_balanced = self#protected_is_balanced
  end

let pp_side side =
  String.concat " + "
    (List.map
       (fun (molecule, coefficient) ->
         if coefficient = 1 then molecule#formula
         else string_of_int coefficient ^ molecule#formula)
       side)

let print_reaction label reaction =
  print_endline ("[" ^ label ^ "]");
  print_endline ("balanced: " ^ string_of_bool reaction#is_balanced);
  (try print_endline ("start : " ^ pp_side reaction#get_start)
   with Failure message -> print_endline ("start : " ^ message));
  (try print_endline ("result: " ^ pp_side reaction#get_result)
   with Failure message -> print_endline ("result: " ^ message));
  print_endline ""

let () =
  let balanced =
    new sample_reaction
      [
        ((new Molecule.methane :> Molecule.molecule), 1);
        ((new Molecule.oxygen :> Molecule.molecule), 2);
      ]
      [
        ((new Molecule.carbon_dioxide :> Molecule.molecule), 1);
        ((new Molecule.water :> Molecule.molecule), 2);
      ]
  in
  let unbalanced =
    new sample_reaction
      ~balanced_version:(balanced :> Reaction.reaction)
      [
        ((new Molecule.methane :> Molecule.molecule), 1);
        ((new Molecule.oxygen :> Molecule.molecule), 1);
      ]
      [
        ((new Molecule.carbon_dioxide :> Molecule.molecule), 1);
        ((new Molecule.water :> Molecule.molecule), 1);
      ]
  in
  let neutral =
    new sample_reaction
      [ ((new Molecule.water :> Molecule.molecule), 2) ]
      [ ((new Molecule.water :> Molecule.molecule), 2) ]
  in
  ignore balanced#balance;
  print_reaction "methane combustion" balanced;
  print_reaction "intentionally unbalanced reaction" unbalanced;
  print_reaction
    "balanced version of the previous reaction"
    (unbalanced#balance);
  print_reaction "identity reaction" neutral
