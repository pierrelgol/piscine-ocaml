let ft_rot_n n str =
  let shift base c =
    let offset = int_of_char c - int_of_char base in
    char_of_int (int_of_char base + ((offset + (n mod 26)) mod 26))
  in
  String.map
    (fun c ->
      if c >= 'a' && c <= 'z' then shift 'a' c
      else if c >= 'A' && c <= 'Z' then shift 'A' c
      else c)
    str
