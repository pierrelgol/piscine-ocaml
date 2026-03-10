let pp side =
  String.concat " + "
    (List.map (fun (m, c) -> if c = 1 then m#formula else string_of_int c ^ m#formula) side)

let alkane_start alkanes oxygen_amount =
  (List.map (fun alkane -> ((alkane :> Molecule.molecule), 1)) alkanes)
  @ [ ((new Molecule.oxygen :> Molecule.molecule), oxygen_amount) ]

let atom_counts side =
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
          (fun inner_counts atom -> add atom#symbol coefficient inner_counts)
          counts molecule#atoms)
      [] side
  in
  List.sort (fun (left, _) (right, _) -> String.compare left right) counts

let print_complete label alkanes reaction =
  let balanced = reaction#balance in
  print_endline ("[" ^ label ^ "]");
  print_endline (pp balanced#get_start ^ " -> " ^ pp balanced#get_result);
  print_endline "incomplete outcomes:";
  List.iter
    (fun (oxygen_amount, products) ->
      print_endline
        ( "  "
        ^ string_of_int oxygen_amount
        ^ "O2 -> "
        ^ pp products
        ^ " balanced="
        ^ string_of_bool
            (atom_counts (alkane_start alkanes oxygen_amount) = atom_counts products) ))
    reaction#get_incomplete_results;
  print_endline ""

let () =
  let ethane_input = [ new Molecule.ethane ] in
  let propane_input = [ new Molecule.propane ] in
  let mixed_input = [ new Molecule.ethane; new Molecule.propane ] in
  let ethane = new Alkane_combustion.alkane_combustion ethane_input in
  let propane = new Alkane_combustion.alkane_combustion propane_input in
  let mixed =
    new Alkane_combustion.alkane_combustion mixed_input
  in
  print_complete "ethane" ethane_input ethane;
  print_complete "propane" propane_input propane;
  print_complete "ethane + propane" mixed_input mixed
