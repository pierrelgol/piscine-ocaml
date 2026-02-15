class sample_reaction s r =
  object
    inherit Reaction.reaction s r
    method get_start = s
    method get_result = r
    method balance = (self :> Reaction.reaction)
    method is_balanced = true
  end

let () =
  let s = [ ((new Molecule.water :> Molecule.molecule), 2) ] in
  let r = [ ((new Molecule.water :> Molecule.molecule), 2) ] in
  let rx = new sample_reaction s r in
  ignore rx#balance;
  print_endline (string_of_bool rx#is_balanced)
