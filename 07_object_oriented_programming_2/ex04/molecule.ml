let rec repeat_atom n make_atom acc =
  if n <= 0 then acc else repeat_atom (n - 1) make_atom ((make_atom ()) :: acc)

class virtual molecule name (atoms : Atom.atom list) =
  object (self)
    method name = name
    method atoms = atoms

    method private sorted_symbols =
      let symbols = List.map (fun atom -> atom#symbol) atoms in
      let unique = List.sort_uniq String.compare symbols in
      let has symbol = List.exists (( = ) symbol) unique in
      let others =
        List.filter (fun symbol -> symbol <> "C" && symbol <> "H") unique
        |> List.sort String.compare
      in
      (if has "C" then [ "C" ] else [])
      @ (if has "H" then [ "H" ] else [])
      @ others

    method formula =
      let count symbol =
        List.fold_left
          (fun total atom -> if atom#symbol = symbol then total + 1 else total)
          0 atoms
      in
      String.concat ""
        (List.map
           (fun symbol ->
             let amount = count symbol in
             if amount = 1 then symbol else symbol ^ string_of_int amount)
           self#sorted_symbols)

    method to_string = self#name ^ "(" ^ self#formula ^ ")"
    method equals (other : molecule) = self#formula = other#formula
  end

class water =
  object
    inherit molecule "Water"
      [
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
      ]
  end

class carbon_dioxide =
  object
    inherit molecule "Carbon dioxide"
      [
        (new Atom.carbon :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
        (new Atom.oxygen :> Atom.atom);
      ]
  end

class oxygen =
  object
    inherit molecule "Oxygen"
      [ (new Atom.oxygen :> Atom.atom); (new Atom.oxygen :> Atom.atom) ]
  end

class alkane n =
  let n =
    if n < 1 || n > 12 then invalid_arg "alkane n must be in [1..12]" else n
  in
  let name =
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
  let hydrogens =
    repeat_atom (2 * n + 2) (fun () -> (new Atom.hydrogen :> Atom.atom)) []
  in
  object
    inherit molecule name (carbons @ hydrogens)
    method carbon_count = n
    method hydrogen_count = 2 * n + 2
  end

class methane = object inherit alkane 1 end
class ethane = object inherit alkane 2 end
class propane = object inherit alkane 3 end
class octane = object inherit alkane 8 end
