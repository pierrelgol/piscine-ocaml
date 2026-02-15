class virtual molecule name formula =
  object
    method name = name
    method formula = formula
    method equals (other : molecule) = formula = other#formula
  end

class water = object inherit molecule "Water" "H2O" end
class carbon_dioxide = object inherit molecule "CarbonDioxide" "CO2" end
