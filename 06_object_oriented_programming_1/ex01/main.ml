let () =
  let rose = new People.people "Rose" in
  let doc = new Doctor.doctor "Who" 900 rose in
  print_endline doc#to_string;
  doc#talk;
  doc#use_sonic_screwdriver;
  doc#travel_in_time 2005 2024;
  doc#heal;
  print_endline doc#to_string
