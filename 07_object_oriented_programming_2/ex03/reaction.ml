class virtual reaction (start : (Molecule.molecule * int) list) (result : (Molecule.molecule * int) list) =
  object
    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool

    method protected_start = start
    method protected_result = result

    method to_string side =
      String.concat " + "
        (List.map
           (fun (m, n) -> if n = 1 then m#formula else string_of_int n ^ m#formula)
           side)
  end
