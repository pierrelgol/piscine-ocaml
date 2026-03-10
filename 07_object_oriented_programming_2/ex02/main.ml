let print_alkane molecule =
  print_endline (molecule#to_string ^ " formula=" ^ molecule#formula)

let print_comparison label left right =
  print_endline (label ^ ": " ^ string_of_bool (left#equals right))

let print_invalid n =
  try
    ignore (new Alkane.alkane n);
    print_endline ("alkane " ^ string_of_int n ^ ": accepted unexpectedly")
  with Invalid_argument message ->
    print_endline ("alkane " ^ string_of_int n ^ ": " ^ message)

let () =
  let alkanes : Molecule.molecule list =
    [
      (new Alkane.methane :> Molecule.molecule);
      (new Alkane.ethane :> Molecule.molecule);
      (new Alkane.alkane 3 :> Molecule.molecule);
      (new Alkane.alkane 4 :> Molecule.molecule);
      (new Alkane.octane :> Molecule.molecule);
      (new Alkane.alkane 12 :> Molecule.molecule);
    ]
  in
  print_endline "[alkanes]";
  List.iter print_alkane alkanes;
  print_endline "";
  print_endline "[equality]";
  print_comparison "methane = alkane 1" (new Alkane.methane) (new Alkane.alkane 1);
  print_comparison "ethane = octane" (new Alkane.ethane) (new Alkane.octane);
  print_endline "";
  print_endline "[invalid inputs]";
  print_invalid 0;
  print_invalid 13
