let ft_string_all pred str =
  let rec loop i =
    if i >= String.length str then true
    else if pred (String.get str i) then loop (i + 1)
    else false
  in
  loop 0
