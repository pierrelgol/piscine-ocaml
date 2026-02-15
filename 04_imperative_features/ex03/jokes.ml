let read_jokes path =
  let ch = open_in path in
  let rec loop acc =
    try
      let line = input_line ch in
      if String.length line = 0 then loop acc else loop (line :: acc)
    with End_of_file ->
      close_in ch;
      List.rev acc
  in
  loop []

let () =
  if Array.length Sys.argv <> 2 then ()
  else
    let jokes_list = read_jokes Sys.argv.(1) in
    let jokes = Array.of_list jokes_list in
    if Array.length jokes = 0 then ()
    else (
      Random.self_init ();
      let i = Random.int (Array.length jokes) in
      print_endline jokes.(i)
    )
