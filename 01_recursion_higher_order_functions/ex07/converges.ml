let converges f x n =
  let rec loop current i =
    if current = f current then true
    else if i = 0 then false
    else loop (f current) (i - 1)
  in
  if n < 0 then false else loop x n
