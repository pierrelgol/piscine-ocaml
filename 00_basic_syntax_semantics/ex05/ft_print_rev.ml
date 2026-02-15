let ft_print_rev str =
  let rec loop i =
    if i >= 0 then (
      print_char (String.get str i);
      loop (i - 1)
    )
  in
  loop (String.length str - 1);
  print_char '\n'
