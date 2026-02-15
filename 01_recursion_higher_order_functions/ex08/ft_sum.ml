let ft_sum f low high =
  if high < low then nan
  else
    let rec loop i acc =
      if i > high then acc else loop (i + 1) (acc +. f i)
    in
    loop low 0.
