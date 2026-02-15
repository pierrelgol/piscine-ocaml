let rec repeat_atom n mk acc = if n <= 0 then acc else repeat_atom (n - 1) mk ((mk ()) :: acc)

class alkane n =
  let n = if n < 1 || n > 12 then invalid_arg "alkane n must be in [1..12]" else n in
  let name_of_n =
    match n with
    | 1 -> "methane"
    | 2 -> "ethane"
    | 3 -> "propane"
    | 4 -> "butane"
    | 5 -> "pentane"
    | 6 -> "hexane"
    | 7 -> "heptane"
    | 8 -> "octane"
    | 9 -> "nonane"
    | 10 -> "decane"
    | 11 -> "undecane"
    | _ -> "dodecane"
  in
  let carbons = repeat_atom n (fun () -> (new Atom.carbon :> Atom.atom)) [] in
  let hydrogens = repeat_atom (2 * n + 2) (fun () -> (new Atom.hydrogen :> Atom.atom)) [] in
  object
    inherit Molecule.molecule name_of_n (carbons @ hydrogens)
  end

class methane = object inherit alkane 1 end
class ethane = object inherit alkane 2 end
class octane = object inherit alkane 8 end
