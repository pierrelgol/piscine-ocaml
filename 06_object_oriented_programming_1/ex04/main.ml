let print_section title =
  print_endline "";
  print_endline ("== " ^ title ^ " ==")

let print_people label people =
  print_endline label;
  List.iter
    (fun p ->
      print_endline
        ("  " ^ p#to_string ^ " alive=" ^ string_of_bool p#is_alive))
    people

let print_doctors label doctors =
  print_endline label;
  List.iter
    (fun d ->
      print_endline
        ("  " ^ d#to_string ^ " alive=" ^ string_of_bool d#is_alive))
    doctors

let print_daleks label daleks =
  print_endline label;
  List.iter
    (fun d ->
      print_endline
        ("  " ^ d#to_string ^ " alive=" ^ string_of_bool d#is_alive))
    daleks

let () =
  Random.init 126;
  let p1 = new People.people "Clara Oswald" in
  let p2 = new People.people "Bill Potts" in
  let p3 = new People.people "Captain Jack" in
  let doc1 = new Doctor.doctor "Who" 1200 p1 in
  let doc2 = new Doctor.doctor "War Doctor" 800 p2 in
  let d1 = new Dalek.dalek in
  let d2 = new Dalek.dalek in
  let d3 = new Dalek.dalek in
  let war =
    new Galifrey.galifrey [ doc1; doc2 ] [ d1; d2; d3 ] [ p1; p2; p3 ]
  in

  print_section "Doctor carry-over behaviour";
  doc1#talk;
  doc1#use_sonic_screwdriver;
  print_endline "Before travel:";
  print_endline doc1#to_string;
  doc1#travel_in_time 2013 2024;
  print_endline "After travel:";
  print_endline doc1#to_string;

  print_section "Initial roster";
  print_people "People:" war#people;
  print_doctors "Doctors:" war#doctors;
  print_daleks "Daleks:" war#daleks;

  print_section "Time war";
  war#do_time_war;

  print_section "Final roster";
  print_people "People:" war#people;
  print_doctors "Doctors:" war#doctors;
  print_daleks "Daleks:" war#daleks
