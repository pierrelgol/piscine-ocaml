let repeat_x n =
  if n < 0 then "Error"
  else
    let rec build acc i =
      if i = n then acc else build (acc ^ "x") (i + 1)
    in
    build "" 0
