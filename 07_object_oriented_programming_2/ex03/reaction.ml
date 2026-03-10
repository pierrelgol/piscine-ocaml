class virtual reaction (start : (Molecule.molecule * int) list) (result : (Molecule.molecule * int) list) =
  object (self)
    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool

    method protected_start = start
    method protected_result = result

    method to_string (side : (Molecule.molecule * int) list) =
      String.concat " + "
        (List.map
           (fun (m, n) -> if n = 1 then m#formula else string_of_int n ^ m#formula)
           side)

    method protected_atom_counts (side : (Molecule.molecule * int) list) =
      let add_atom counts atom amount =
        let rec loop remaining acc =
          match remaining with
          | [] -> List.rev ((atom#symbol, amount) :: acc)
          | (symbol, total) :: tail when symbol = atom#symbol ->
              List.rev_append acc ((symbol, total + amount) :: tail)
          | head :: tail -> loop tail (head :: acc)
        in
        loop counts []
      in
      let counts =
        List.fold_left
          (fun counts (molecule, coefficient) ->
            List.fold_left
              (fun inner_counts atom ->
                add_atom inner_counts atom coefficient)
              counts molecule#atoms)
          [] side
      in
      List.sort (fun (left, _) (right, _) -> String.compare left right) counts

    method protected_is_balanced =
      self#protected_atom_counts start = self#protected_atom_counts result
  end
