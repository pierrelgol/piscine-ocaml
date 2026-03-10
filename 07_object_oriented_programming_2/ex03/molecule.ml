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

class methane =
  object
    inherit molecule "Methane"
      [
        (new Atom.carbon :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
      ]
  end

class oxygen =
  object
    inherit molecule "Oxygen"
      [ (new Atom.oxygen :> Atom.atom); (new Atom.oxygen :> Atom.atom) ]
  end

class ammonia =
  object
    inherit molecule "Ammonia"
      [
        (new Atom.nitrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
        (new Atom.hydrogen :> Atom.atom);
      ]
  end
