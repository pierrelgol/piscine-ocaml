let ft_print_alphabet () =
  let rec loop c =
    if c <= 'z' then (
      print_char c;
      loop (char_of_int (int_of_char c + 1))
    )
  in
  loop 'a';
  print_char '\n'
