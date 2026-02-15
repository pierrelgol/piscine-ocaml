let jokes =
  [|
    "Why do programmers prefer dark mode? Because light attracts bugs.";
    "There are 10 kinds of people: those who understand binary and those who don't.";
    "I changed my password to 'incorrect' so the computer reminds me.";
    "A SQL query walks into a bar, walks up to two tables and asks: can I join you?";
    "My code doesn't have bugs, it just develops random features.";
  |]

let () =
  Random.self_init ();
  let i = Random.int (Array.length jokes) in
  print_endline jokes.(i)
