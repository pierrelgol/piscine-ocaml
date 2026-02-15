let sequence n =
  let rec encode s i current count acc =
    if i = String.length s then
      acc ^ string_of_int count ^ String.make 1 current
    else
      let c = String.get s i in
      if c = current then encode s (i + 1) current (count + 1) acc
      else
        encode s (i + 1) c 1 (acc ^ string_of_int count ^ String.make 1 current)
  in
  let next_term s =
    if s = "" then "" else encode s 1 (String.get s 0) 1 ""
  in
  if n <= 0 then ""
  else
    let rec loop i term = if i = n then term else loop (i + 1) (next_term term) in
    loop 1 "1"
