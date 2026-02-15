class virtual atom name symbol atomic_number =
  object
    method name : string = name
    method symbol : string = symbol
    method atomic_number : int = atomic_number
  end

class hydrogen = object inherit atom "Hydrogen" "H" 1 end
class carbon = object inherit atom "Carbon" "C" 6 end
