let my_sleep () = Unix.sleep 1

let () =
  if Array.length Sys.argv <> 2 then ()
  else
    try
      let n = int_of_string Sys.argv.(1) in
      if n > 0 then for _ = 1 to n do ignore (my_sleep ()) done
    with _ -> ()
