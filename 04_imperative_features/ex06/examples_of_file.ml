let split_comma s =
  let rec loop i j acc =
    if j = String.length s then List.rev ((String.sub s i (j - i)) :: acc)
    else if s.[j] = ',' then loop (j + 1) (j + 1) ((String.sub s i (j - i)) :: acc)
    else loop i (j + 1) acc
  in
  if s = "" then [] else loop 0 0 []

let examples_of_file path =
  let ch = open_in path in
  let parse_line line =
    match List.rev (split_comma line) with
    | [] -> ([||], "")
    | cls :: rev_feats ->
        let feats = Array.of_list (List.rev_map float_of_string rev_feats) in
        (feats, cls)
  in
  let rec loop acc =
    try
      let line = input_line ch in
      if String.length line = 0 then loop acc else loop (parse_line line :: acc)
    with End_of_file ->
      close_in ch;
      List.rev acc
  in
  loop []
