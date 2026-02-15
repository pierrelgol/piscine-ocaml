let () =
  let ms : Molecule.molecule list =
    [ (new Molecule.water :> Molecule.molecule); (new Molecule.carbon_dioxide :> Molecule.molecule); (new Molecule.methane :> Molecule.molecule); (new Molecule.ammonia :> Molecule.molecule); (new Molecule.sodium_chloride :> Molecule.molecule) ]
  in
  List.iter (fun m -> print_endline m#to_string) ms
