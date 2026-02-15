let leibniz_pi delta =
  if delta < 0. then -1
  else
    let pi_ref = 4. *. atan 1. in
    let absf x = if x < 0. then -.x else x in
    let rec loop i sum =
      let pi_est = 4. *. sum in
      if absf (pi_est -. pi_ref) <= delta then i
      else
        let sign = if i mod 2 = 0 then 1. else -1. in
        let denom = float_of_int (2 * i + 1) in
        loop (i + 1) (sum +. (sign /. denom))
    in
    loop 0 0.
