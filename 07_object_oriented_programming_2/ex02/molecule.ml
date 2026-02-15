class virtual molecule name (atoms : Atom.atom list) =
  object (self)
    method name = name

    method private sorted_symbols =
      let symbols = List.map (fun a -> a#symbol) atoms in
      let uniq = List.sort_uniq String.compare symbols in
      let has s = List.exists (( = ) s) uniq in
      let others = List.filter (fun s -> s <> "C" && s <> "H") uniq |> List.sort String.compare in
      (if has "C" then [ "C" ] else []) @ (if has "H" then [ "H" ] else []) @ others

    method formula =
      let count s = List.fold_left (fun acc a -> if a#symbol = s then acc + 1 else acc) 0 atoms in
      String.concat ""
        (List.map
           (fun s ->
             let n = count s in
             if n <= 1 then s else s ^ string_of_int n)
           self#sorted_symbols)

    method to_string = self#name ^ "(" ^ self#formula ^ ")"
    method equals (other : molecule) = self#formula = other#formula
  end
