let ft_print_comb () =
  for i = 0 to 7 do
    for j = i + 1 to 8 do
      for k = j + 1 to 9 do
        print_int i;
        print_int j;
        print_int k;
        if not (i = 7 && j = 8 && k = 9) then print_string ", "
      done
    done
  done;
  print_string "\n"
