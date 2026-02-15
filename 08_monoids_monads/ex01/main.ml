module App = struct
  type project = string * string * int

  let zero = ("", "", 0)

  let combine (n1, _, g1) (n2, _, g2) =
    let grade = (g1 + g2) / 2 in
    let status = if grade > 80 then "succeed" else "failed" in
    (n1 ^ n2, status, grade)

  let fail (name, _, _) = (name, "failed", 0)
  let success (name, _, _) = (name, "succeed", 80)
end

let print_proj (n, s, g) = Printf.printf "project=%s status=%s grade=%d\n" n s g

let () =
  let p1 = ("d08_ex01_", "succeed", 90) in
  let p2 = ("final", "failed", 70) in
  print_proj (App.combine p1 p2);
  print_proj (App.fail p1);
  print_proj (App.success p2);
  print_proj App.zero
