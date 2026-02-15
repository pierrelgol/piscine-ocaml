let () =
  let amy = new People.people "Amy Pond" in
  print_endline amy#to_string;
  amy#talk;
  amy#die
