let eu_dist a b =
  let n = Array.length a in
  let acc = ref 0. in
  for i = 0 to n - 1 do
    let d = a.(i) -. b.(i) in
    acc := !acc +. (d *. d)
  done;
  sqrt !acc
