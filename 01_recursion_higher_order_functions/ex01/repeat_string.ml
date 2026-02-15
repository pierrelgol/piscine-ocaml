let repeat_string ?(str = "x") n =
  if n < 0 then "Error"
  else
    let rec build acc i =
      if i = n then acc else build (acc ^ str) (i + 1)
    in
    build "" 0
