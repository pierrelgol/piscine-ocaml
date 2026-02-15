class virtual molecule (name : string) (formula : string) =
  object
    method name = name
    method formula = formula
    method equals (other : molecule) = formula = other#formula
  end

class water = object inherit molecule "Water" "H2O" end
class carbon_dioxide = object inherit molecule "CarbonDioxide" "CO2" end
class oxygen = object inherit molecule "Oxygen" "O2" end

class alkane n =
  object
    inherit molecule
      (match n with
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
      | _ -> "dodecane")
      ("C" ^ string_of_int n ^ "H" ^ string_of_int (2 * n + 2))

    method carbon_count = n
    method hydrogen_count = 2 * n + 2
  end

class methane = object inherit alkane 1 end
class ethane = object inherit alkane 2 end
class octane = object inherit alkane 8 end
