class people name =
  object
    val _name : string = name
    val mutable _hp : int = 100

    method to_string =
      "People(name=" ^ _name ^ ", hp=" ^ string_of_int _hp ^ ")"

    method talk = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")

    method die =
      _hp <- 0;
      print_endline "Aaaarghh!"

    method is_alive = _hp > 0

    initializer print_endline ("[people] created: " ^ _name)
  end
