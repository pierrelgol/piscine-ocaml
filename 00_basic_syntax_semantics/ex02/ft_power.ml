let rec ft_power base exp =
  if exp = 0 then 1 else base * ft_power base (exp - 1)
