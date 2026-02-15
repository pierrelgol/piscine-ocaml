class doctor name age sidekick =
  object (self)
    val _name : string = name
    val mutable _age : int = age
    val _sidekick : People.people = sidekick
    val mutable _hp : int = 100

    method to_string =
      "Doctor(name=" ^ _name ^ ", age=" ^ string_of_int _age ^ ", hp="
      ^ string_of_int _hp ^ ", sidekick=" ^ _sidekick#to_string ^ ")"

    method talk = print_endline "Hi! I'm the Doctor!"

    method travel_in_time start arrival =
      _age <- _age + (arrival - start);
      print_endline "  _");
      print_endline " /_\\";
      print_endline "|TARDIS|"

    method use_sonic_screwdriver =
      print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

    method private regenerate = _hp <- 100
    method heal = self#regenerate

    initializer print_endline ("[doctor] created: " ^ _name)
  end
