let ft_is_palindrome str =
  let rec loop i j =
    if i >= j then true
    else if String.get str i = String.get str j then loop (i + 1) (j - 1)
    else false
  in
  loop 0 (String.length str - 1)
