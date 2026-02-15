let ft_countdown n =
  let rec loop x =
    print_int x;
    print_char '\n';
    if x > 0 then loop (x - 1)
  in
  if n < 0 then loop 0 else loop n
