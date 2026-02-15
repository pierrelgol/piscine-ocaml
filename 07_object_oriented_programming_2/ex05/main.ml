let pp side =
  String.concat " + "
    (List.map (fun (m, c) -> if c = 1 then m#formula else string_of_int c ^ m#formula) side)

let () =
  let r = new Alkane_combustion.alkane_combustion [ new Molecule.ethane; new Molecule.propane ] in
  print_endline (pp r#get_start ^ " -> " ^ pp r#get_result);
  List.iter
    (fun (o2, prods) -> print_endline (string_of_int o2 ^ " O2 -> " ^ pp prods))
    r#get_incomplete_results
