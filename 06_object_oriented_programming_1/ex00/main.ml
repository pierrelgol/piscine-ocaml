let print_section title =
  print_endline "";
  print_endline ("== " ^ title ^ " ==")

let () =
  print_section "Creating companions";
  let amy = new People.people "Amy Pond" in
  let river = new People.people "River Song" in

  print_section "Inspecting state";
  print_endline amy#to_string;
  print_endline river#to_string;

  print_section "Talking";
  amy#talk;
  river#talk;

  print_section "Dying";
  amy#die;
  river#die
