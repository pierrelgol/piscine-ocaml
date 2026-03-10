let print_section title =
  print_endline "";
  print_endline ("== " ^ title ^ " ==")

let print_army label to_string members =
  print_endline
    (label ^ " size: " ^ string_of_int (List.length members));
  List.iter (fun member -> print_endline ("  " ^ to_string member)) members

let () =
  Random.init 84;
  let p1 = new People.people "Martha Jones" in
  let p2 = new People.people "Donna Noble" in
  let p3 = new People.people "Wilfred Mott" in
  let doctor = new Doctor.doctor "Who" 1000 p1 in
  let k1 = new Dalek.dalek in
  let k2 = new Dalek.dalek in

  print_section "Doctor carry-over behaviour";
  doctor#talk;
  doctor#use_sonic_screwdriver;
  print_endline "Before travel:";
  print_endline doctor#to_string;
  doctor#travel_in_time 1913 2024;
  print_endline "After travel:";
  print_endline doctor#to_string;

  print_section "People army";
  let people_army = new Army.army in
  people_army#delete;
  print_army "People army after delete on empty" (fun p -> p#to_string)
    people_army#members;
  people_army#add p1;
  people_army#add p2;
  people_army#add p3;
  print_army "People army after additions" (fun p -> p#to_string)
    people_army#members;
  people_army#delete;
  print_army "People army after one delete" (fun p -> p#to_string)
    people_army#members;

  print_section "Doctor army";
  let doctor_army = new Army.army in
  doctor_army#add doctor;
  print_army "Doctor army after addition" (fun d -> d#to_string)
    doctor_army#members;
  doctor_army#delete;
  print_army "Doctor army after delete" (fun d -> d#to_string)
    doctor_army#members;

  print_section "Dalek army";
  let dalek_army = new Army.army in
  dalek_army#add k1;
  dalek_army#add k2;
  print_army "Dalek army after additions" (fun d -> d#to_string)
    dalek_army#members;
  dalek_army#delete;
  print_army "Dalek army after one delete" (fun d -> d#to_string)
    dalek_army#members
