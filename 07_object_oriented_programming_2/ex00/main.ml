let print_atom atom = print_endline atom#to_string

let print_comparison label left right =
  print_endline
    (label ^ ": " ^ string_of_bool (left#equals right))

let () =
  let atoms : Atom.atom list =
    [
      (new Atom.hydrogen :> Atom.atom);
      (new Atom.carbon :> Atom.atom);
      (new Atom.oxygen :> Atom.atom);
      (new Atom.nitrogen :> Atom.atom);
      (new Atom.chlorine :> Atom.atom);
      (new Atom.sodium :> Atom.atom);
    ]
  in
  print_endline "[atoms]";
  List.iter print_atom atoms;
  print_endline "";
  print_endline "[comparisons]";
  print_comparison "hydrogen = hydrogen" (new Atom.hydrogen) (new Atom.hydrogen);
  print_comparison "hydrogen = carbon" (new Atom.hydrogen) (new Atom.carbon);
  print_comparison "oxygen = oxygen" (new Atom.oxygen) (new Atom.oxygen)
