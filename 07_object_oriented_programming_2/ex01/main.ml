class glucose =
  object
    inherit Molecule.molecule "Glucose"
      [
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
      ]
  end

class tnt =
  object
    inherit Molecule.molecule "TNT"
      [
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.carbon :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.nitrogen :> Atom.atom);
        (new Atom.nitrogen :> Atom.atom);
        (new Atom.nitrogen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
      ]
  end

let print_molecule molecule =
  print_endline (molecule#to_string ^ " formula=" ^ molecule#formula)

let print_comparison label left right =
  print_endline (label ^ ": " ^ string_of_bool (left#equals right))

let () =
  let molecules : Molecule.molecule list =
    [
      (new Molecule.water :> Molecule.molecule);
      (new Molecule.carbon_dioxide :> Molecule.molecule);
      (new Molecule.methane :> Molecule.molecule);
      (new Molecule.ammonia :> Molecule.molecule);
      (new Molecule.sodium_chloride :> Molecule.molecule);
      (new glucose :> Molecule.molecule);
      (new tnt :> Molecule.molecule);
    ]
  in
  print_endline "[molecules]";
  List.iter print_molecule molecules;
  print_endline "";
  print_endline "[equality]";
  print_comparison "water = water" (new Molecule.water) (new Molecule.water);
  print_comparison "water = carbon dioxide" (new Molecule.water) (new Molecule.carbon_dioxide);
  print_comparison "methane = methane" (new Molecule.methane) (new Molecule.methane)
