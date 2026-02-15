let () =
  Random.self_init ();
  let p1 = new People.people "Martha" in
  let p2 = new People.people "Donna" in
  let d = new Doctor.doctor "Who" 1000 p1 in
  let k1 = new Dalek.dalek in
  let k2 = new Dalek.dalek in

  let people_army = new Army.army in
  people_army#add p1;
  people_army#add p2;
  people_army#delete;

  let doctor_army = new Army.army in
  doctor_army#add d;

  let dalek_army = new Army.army in
  dalek_army#add k1;
  dalek_army#add k2;
  dalek_army#delete;

  print_endline ("People army size: " ^ string_of_int (List.length people_army#members));
  print_endline ("Doctor army size: " ^ string_of_int (List.length doctor_army#members));
  print_endline ("Dalek army size: " ^ string_of_int (List.length dalek_army#members))
