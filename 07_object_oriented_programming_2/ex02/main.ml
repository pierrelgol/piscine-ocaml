let () =
  let a1 = new Alkane.methane in
  let a2 = new Alkane.ethane in
  let a3 = new Alkane.octane in
  print_endline a1#to_string;
  print_endline a2#to_string;
  print_endline a3#to_string;
  print_endline (string_of_bool (a1#equals (new Alkane.alkane 1 :> Molecule.molecule)))
