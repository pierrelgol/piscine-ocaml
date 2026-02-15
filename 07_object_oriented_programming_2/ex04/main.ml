let pp side =
  String.concat " + "
    (List.map (fun (m, c) -> if c = 1 then m#formula else string_of_int c ^ m#formula) side)

let () =
  let r = new Alkane_combustion.alkane_combustion [ new Molecule.methane; new Molecule.ethane ] in
  ignore r#balance;
  if r#is_balanced then (
    print_endline (pp r#get_start ^ " -> " ^ pp r#get_result)
  )
