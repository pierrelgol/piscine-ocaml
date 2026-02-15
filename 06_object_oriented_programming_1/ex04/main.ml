let () =
  Random.self_init ();
  let p1 = new People.people "Clara" in
  let p2 = new People.people "Bill" in
  let doc1 = new Doctor.doctor "Who" 1200 p1 in
  let doc2 = new Doctor.doctor "Master-Doctor" 800 p2 in
  let d1 = new Dalek.dalek in
  let d2 = new Dalek.dalek in
  let d3 = new Dalek.dalek in
  let war = new Galifrey.galifrey [ doc1; doc2 ] [ d1; d2; d3 ] [ p1; p2 ] in
  war#do_time_war;
  print_endline "Survivors:";
  List.iter (fun p -> print_endline p#to_string) war#people;
  List.iter (fun d -> print_endline d#to_string) war#doctors;
  List.iter (fun d -> print_endline d#to_string) war#daleks
