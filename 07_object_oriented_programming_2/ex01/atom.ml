class virtual atom name symbol atomic_number =
  object
    method name : string = name
    method symbol : string = symbol
    method atomic_number : int = atomic_number
  end

class hydrogen = object inherit atom "Hydrogen" "H" 1 end
class carbon = object inherit atom "Carbon" "C" 6 end
class oxygen = object inherit atom "Oxygen" "O" 8 end
class nitrogen = object inherit atom "Nitrogen" "N" 7 end
class chlorine = object inherit atom "Chlorine" "Cl" 17 end
class sodium = object inherit atom "Sodium" "Na" 11 end
