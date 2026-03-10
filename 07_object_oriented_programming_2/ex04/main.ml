let pp side =
  String.concat " + "
    (List.map (fun (m, c) -> if c = 1 then m#formula else string_of_int c ^ m#formula) side)

let print_before_balance label reaction =
  print_endline ("[" ^ label ^ "]");
  print_endline ("balanced: " ^ string_of_bool reaction#is_balanced);
  (try
     ignore reaction#get_start;
     print_endline "unexpectedly returned a balanced start side"
   with Failure message -> print_endline ("get_start: " ^ message));
  print_endline ""

let print_after_balance label reaction =
  print_endline ("[" ^ label ^ " balanced]");
  print_endline ("balanced: " ^ string_of_bool reaction#is_balanced);
  print_endline (pp reaction#get_start ^ " -> " ^ pp reaction#get_result);
  print_endline ""

let () =
  let methane = new Alkane_combustion.alkane_combustion [ new Molecule.methane ] in
  let mixed =
    new Alkane_combustion.alkane_combustion
      [ new Molecule.methane; new Molecule.ethane; new Molecule.propane ]
  in
  let duplicated =
    new Alkane_combustion.alkane_combustion
      [ new Molecule.ethane; new Molecule.ethane; new Molecule.octane ]
  in
  print_before_balance "single alkane" methane;
  print_before_balance "multiple different alkanes" mixed;
  print_before_balance "duplicate alkanes are merged before balancing" duplicated;
  print_after_balance "single alkane" methane#balance;
  print_after_balance "multiple different alkanes" mixed#balance;
  print_after_balance
    "duplicate alkanes are merged before balancing" duplicated#balance
