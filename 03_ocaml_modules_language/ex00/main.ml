let () =
  List.iter
    (fun c ->
      print_endline (Color.toString c ^ " -> " ^ Color.toStringVerbose c))
    Color.all
