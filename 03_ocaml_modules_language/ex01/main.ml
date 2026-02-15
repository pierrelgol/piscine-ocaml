let () =
  List.iter
    (fun v ->
      Printf.printf "%s (%s) -> %d\n" (Value.toString v) (Value.toStringVerbose v)
        (Value.toInt v))
    Value.all
