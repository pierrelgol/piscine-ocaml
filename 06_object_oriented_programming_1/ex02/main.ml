let () =
  Random.self_init ();
  let human = new People.people "Rory" in
  let doctor = new Doctor.doctor "Who" 1000 human in
  let dalek = new Dalek.dalek in

  print_endline human#to_string;
  print_endline doctor#to_string;
  print_endline dalek#to_string;

  doctor#talk;
  doctor#use_sonic_screwdriver;
  dalek#talk;
  dalek#exterminate human;
  if not human#is_alive then print_endline "Human down!";
  dalek#die;
  doctor#die
