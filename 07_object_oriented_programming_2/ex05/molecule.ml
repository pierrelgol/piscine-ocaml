class virtual molecule name formula =
  object
    method name = name
    method formula = formula
  end

class water = object inherit molecule "Water" "H2O" end
class carbon_dioxide = object inherit molecule "CarbonDioxide" "CO2" end
class carbon_monoxide = object inherit molecule "CarbonMonoxide" "CO" end
class carbon = object inherit molecule "Carbon" "C" end
class oxygen = object inherit molecule "Oxygen" "O2" end

class alkane n =
  object
    inherit molecule "alkane" ("C" ^ string_of_int n ^ "H" ^ string_of_int (2 * n + 2))
    method carbon_count = n
    method hydrogen_count = 2 * n + 2
  end

class methane = object inherit alkane 1 end
class ethane = object inherit alkane 2 end
class propane = object inherit alkane 3 end
