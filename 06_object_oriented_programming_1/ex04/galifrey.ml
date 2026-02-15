class galifrey doctors daleks people =
  object (self)
    val mutable _doctors : Doctor.doctor list = doctors
    val mutable _daleks : Dalek.dalek list = daleks
    val mutable _people : People.people list = people

    method doctors = _doctors
    method daleks = _daleks
    method people = _people

    method private first_alive_person lst =
      let rec loop = function
        | [] -> None
        | x :: xs -> if x#is_alive then Some x else loop xs
      in
      loop lst

    method private first_alive_dalek lst =
      let rec loop = function
        | [] -> None
        | x :: xs -> if x#is_alive then Some x else loop xs
      in
      loop lst

    method private daleks_attack =
      let rec loop = function
        | [] -> ()
        | d :: ds ->
            if d#is_alive then
              match self#first_alive_person _people with
              | None -> ()
              | Some p -> d#exterminate p;
            loop ds
      in
      loop _daleks

    method private doctors_attack =
      let rec loop = function
        | [] -> ()
        | d :: ds ->
            if d#is_alive then
              match self#first_alive_dalek _daleks with
              | None -> ()
              | Some enemy -> enemy#die;
            loop ds
      in
      loop _doctors

    method do_time_war =
      print_endline "Time War begins!";
      List.iter (fun d -> if d#is_alive then d#talk) _doctors;
      List.iter (fun d -> if d#is_alive then d#talk) _daleks;
      self#daleks_attack;
      self#doctors_attack;
      print_endline "Time War ends."
  end
