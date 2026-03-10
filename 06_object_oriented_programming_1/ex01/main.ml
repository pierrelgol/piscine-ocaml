let print_section title =
  print_endline "";
  print_endline ("== " ^ title ^ " ==")

let () =
  print_section "Creating the cast";
  let rose = new People.people "Rose Tyler" in
  let doctor = new Doctor.doctor "Who" 900 rose in

  print_section "Initial state";
  print_endline rose#to_string;
  print_endline doctor#to_string;

  print_section "Doctor basics";
  doctor#talk;
  doctor#use_sonic_screwdriver;

  print_section "Time travel";
  print_endline "Before travel:";
  print_endline doctor#to_string;
  doctor#travel_in_time 2005 2024;
  print_endline "After travelling from 2005 to 2024:";
  print_endline doctor#to_string;

  print_section "Regeneration hook";
  doctor#heal;
  print_endline doctor#to_string
