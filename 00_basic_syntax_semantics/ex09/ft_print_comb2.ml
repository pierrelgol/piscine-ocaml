let print_two_digits n =
  print_char (char_of_int (int_of_char '0' + (n / 10)));
  print_char (char_of_int (int_of_char '0' + (n mod 10)))

let ft_print_comb2 () =
  for a = 0 to 98 do
    for b = a + 1 to 99 do
      print_two_digits a;
      print_char ' ';
      print_two_digits b;
      if not (a = 98 && b = 99) then (
        print_char ',';
        print_char ' '
      )
    done
  done;
  print_char '\n'
