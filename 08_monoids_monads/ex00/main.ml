module Watchtower = struct
  type hour = int

  let zero = 0

  let normalize h =
    let m = h mod 12 in
    if m < 0 then m + 12 else m

  let add a b = normalize (a + b)
  let sub a b = normalize (a - b)
end

let print_hour_case label value =
  Printf.printf "%s = %d\n" label value

let () =
  print_endline "Watchtower tests:";
  print_hour_case "zero" Watchtower.zero;
  print_hour_case "add zero 8" (Watchtower.add Watchtower.zero 8);
  print_hour_case "add 8 zero" (Watchtower.add 8 Watchtower.zero);
  print_hour_case "add 10 5" (Watchtower.add 10 5);
  print_hour_case "add 11 2" (Watchtower.add 11 2);
  print_hour_case "add 12 12" (Watchtower.add 12 12);
  print_hour_case "add 25 24" (Watchtower.add 25 24);
  print_hour_case "add (-3) 5" (Watchtower.add (-3) 5);
  print_hour_case "sub 3 5" (Watchtower.sub 3 5);
  print_hour_case "sub 0 1" (Watchtower.sub 0 1);
  print_hour_case "sub 12 12" (Watchtower.sub 12 12);
  print_hour_case "sub 24 7" (Watchtower.sub 24 7);
  print_hour_case "sub (-2) 5" (Watchtower.sub (-2) 5)
