type radar = float array * string

let eu_dist a b =
  let n = Array.length a in
  let acc = ref 0. in
  for i = 0 to n - 1 do
    let d = a.(i) -. b.(i) in
    acc := !acc +. (d *. d)
  done;
  sqrt !acc

let one_nn training (point, _) =
  match training with
  | [] -> ""
  | (v0, c0) :: xs ->
      let rec loop best_d best_c = function
        | [] -> best_c
        | (v, c) :: tl ->
            let d = eu_dist point v in
            if d < best_d then loop d c tl else loop best_d best_c tl
      in
      loop (eu_dist point v0) c0 xs
