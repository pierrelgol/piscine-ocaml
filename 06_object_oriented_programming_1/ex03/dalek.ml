let random_suffix () =
  match Random.int 6 with
  | 0 -> "Sec"
  | 1 -> "Caan"
  | 2 -> "Jast"
  | 3 -> "Thay"
  | 4 -> "Khan"
  | _ -> "Xor"

class dalek =
  object
    val _name : string = "Dalek" ^ random_suffix ()
    val mutable _hp : int = 100
    val mutable _shield : bool = true

    method to_string =
      "Dalek(name=" ^ _name ^ ", hp=" ^ string_of_int _hp ^ ", shield="
      ^ string_of_bool _shield ^ ")"

    method talk =
      match Random.int 4 with
      | 0 -> print_endline "Explain! Explain!"
      | 1 -> print_endline "Exterminate! Exterminate!"
      | 2 -> print_endline "I obey!"
      | _ -> print_endline "You are the Doctor! You are the enemy of the Daleks!"

    method exterminate (p : People.people) =
      _shield <- not _shield;
      p#die

    method die =
      _hp <- 0;
      print_endline "Emergency Temporal Shift!"

    method is_alive = _hp > 0
  end
