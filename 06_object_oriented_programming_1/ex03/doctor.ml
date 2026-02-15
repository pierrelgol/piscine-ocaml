class doctor name age sidekick =
  object
    val _name : string = name
    val mutable _age : int = age
    val _sidekick : People.people = sidekick
    val mutable _hp : int = 100

    method to_string =
      "Doctor(name=" ^ _name ^ ", age=" ^ string_of_int _age ^ ", hp="
      ^ string_of_int _hp ^ ", sidekick=" ^ _sidekick#to_string ^ ")"

    method talk = print_endline "Hi! I'm the Doctor!"

    method die =
      _hp <- 0;
      print_endline "The Doctor has fallen..."

    method is_alive = _hp > 0
  end
