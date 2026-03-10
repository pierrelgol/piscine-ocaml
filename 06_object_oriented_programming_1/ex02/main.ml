let print_section title =
  print_endline "";
  print_endline ("== " ^ title ^ " ==")

let () =
  Random.init 42;
  let human = new People.people "Rory Williams" in
  let doctor = new Doctor.doctor "Who" 1000 human in
  let dalek = new Dalek.dalek in

  print_section "Initial state";
  print_endline human#to_string;
  print_endline doctor#to_string;
  print_endline dalek#to_string;

  print_section "Doctor actions";
  doctor#talk;
  doctor#use_sonic_screwdriver;
  doctor#travel_in_time 1941 2024;
  print_endline doctor#to_string;

  print_section "Dalek voice lines";
  dalek#talk;
  dalek#talk;

  print_section "Extermination";
  print_endline "Dalek before attack:";
  print_endline dalek#to_string;
  dalek#exterminate human;
  print_endline "Human after attack:";
  print_endline human#to_string;
  print_endline ("Human alive: " ^ string_of_bool human#is_alive);
  print_endline "Dalek after attack (shield should have toggled):";
  print_endline dalek#to_string;

  print_section "Deaths";
  dalek#die;
  doctor#die;
  print_endline dalek#to_string;
  print_endline doctor#to_string
