let () =
  let card = Card.newCard Card.Value.As Card.Color.Spade in
  print_endline (Card.toString card);
  print_endline (Card.toStringVerbose card);
  let best = Card.best Card.all in
  print_endline ("Best card value is " ^ Card.Value.toString (Card.getValue best))
