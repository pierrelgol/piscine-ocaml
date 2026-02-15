let () =
  Random.self_init ();
  let deck = Deck.newDeck () in
  let (card, rest) = Deck.drawCard deck in
  print_endline ("Drawn: " ^ Deck.Card.toStringVerbose card);
  print_endline ("Remaining cards: " ^ string_of_int (List.length (Deck.toStringList rest)))
