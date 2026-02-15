let () =
  let atoms : Atom.atom list =
    [ (new Atom.hydrogen :> Atom.atom); (new Atom.carbon :> Atom.atom); (new Atom.oxygen :> Atom.atom); (new Atom.nitrogen :> Atom.atom); (new Atom.chlorine :> Atom.atom); (new Atom.sodium :> Atom.atom) ]
  in
  List.iter (fun a -> print_endline a#to_string) atoms
