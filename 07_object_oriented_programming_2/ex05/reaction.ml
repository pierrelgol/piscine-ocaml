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
           (fun (molecule, coefficient) ->
             if coefficient = 1 then molecule#formula
             else string_of_int coefficient ^ molecule#formula)
           side)

    method protected_atom_counts (side : (Molecule.molecule * int) list) =
      let add symbol amount counts =
        let rec loop remaining acc =
          match remaining with
          | [] -> List.rev ((symbol, amount) :: acc)
          | (current, total) :: tail when current = symbol ->
              List.rev_append acc ((current, total + amount) :: tail)
          | head :: tail -> loop tail (head :: acc)
        in
        loop counts []
      in
      let counts =
        List.fold_left
          (fun counts (molecule, coefficient) ->
            List.fold_left
              (fun inner_counts atom ->
                add atom#symbol coefficient inner_counts)
              counts molecule#atoms)
          [] side
      in
      List.sort (fun (left, _) (right, _) -> String.compare left right) counts

    method protected_is_balanced =
      self#protected_atom_counts start = self#protected_atom_counts result
  end
