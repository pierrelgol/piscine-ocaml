let iter f x n =
  if n < 0 then -1
  else
    let rec loop acc i = if i = 0 then acc else loop (f acc) (i - 1) in
    loop x n
