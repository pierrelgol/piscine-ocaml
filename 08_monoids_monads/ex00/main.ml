module Watchtower = struct
  type hour = int

  let zero = 0

  let normalize h =
    let m = h mod 12 in
    if m < 0 then m + 12 else m

  let add a b = normalize (a + b)
  let sub a b = normalize (a - b)
end

let () =
  let open Watchtower in
  Printf.printf "add 10 5 = %d\n" (add 10 5);
  Printf.printf "sub 3 5 = %d\n" (sub 3 5);
  Printf.printf "add zero 8 = %d\n" (add zero 8)
