type nucleobase = A | T | C | G | U | None

type nucleotide = string * string * nucleobase

type helix = nucleotide list

type rna = nucleobase list

type aminoacid =
  | Stop
  | Ala
  | Arg
  | Asn
  | Asp
  | Cys
  | Gln
  | Glu
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val

type protein = aminoacid list

let nucleobase_of_char = function
  | 'A' | 'a' -> A
  | 'T' | 't' -> T
  | 'C' | 'c' -> C
  | 'G' | 'g' -> G
  | _ -> None

let string_of_nucleobase = function
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | U -> "U"
  | None -> "N"

let complementary_base = function
  | A -> T
  | T -> A
  | C -> G
  | G -> C
  | U -> A
  | None -> None

let complementary_rna_base = function
  | A -> U
  | T -> A
  | C -> G
  | G -> C
  | U -> A
  | None -> None

let generate_helix s =
  let rec loop i acc =
    if i < 0 then acc
    else
      let b = nucleobase_of_char (String.get s i) in
      loop (i - 1) (("phosphate", "deoxyribose", b) :: acc)
  in
  loop (String.length s - 1) []

let helix_to_string h =
  let rec loop hx acc =
    match hx with
    | [] -> acc
    | (_, _, b) :: xs -> loop xs (acc ^ string_of_nucleobase b)
  in
  loop h ""

let complementary_helix h =
  let rec loop hx acc =
    match hx with
    | [] -> List.rev acc
    | (_, _, b) :: xs ->
        loop xs (("phosphate", "deoxyribose", complementary_base b) :: acc)
  in
  loop h []

let generate_rna h =
  let rec loop hx acc =
    match hx with
    | [] -> List.rev acc
    | (_, _, b) :: xs -> loop xs (complementary_rna_base b :: acc)
  in
  loop h []

let generate_bases_triplets r =
  let rec loop rx acc =
    match rx with
    | a :: b :: c :: xs -> loop xs ((a, b, c) :: acc)
    | _ -> List.rev acc
  in
  loop r []

let aminoacid_of_triplet = function
  | (U, A, A) | (U, A, G) | (U, G, A) -> Stop
  | (G, C, A) | (G, C, C) | (G, C, G) | (G, C, U) -> Ala
  | (A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U) -> Arg
  | (A, A, C) | (A, A, U) -> Asn
  | (G, A, C) | (G, A, U) -> Asp
  | (U, G, C) | (U, G, U) -> Cys
  | (C, A, A) | (C, A, G) -> Gln
  | (G, A, A) | (G, A, G) -> Glu
  | (G, G, A) | (G, G, C) | (G, G, G) | (G, G, U) -> Gly
  | (C, A, C) | (C, A, U) -> His
  | (A, U, A) | (A, U, C) | (A, U, U) -> Ile
  | (C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G) -> Leu
  | (A, A, A) | (A, A, G) -> Lys
  | (A, U, G) -> Met
  | (U, U, C) | (U, U, U) -> Phe
  | (C, C, C) | (C, C, A) | (C, C, G) | (C, C, U) -> Pro
  | (U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C) -> Ser
  | (A, C, A) | (A, C, C) | (A, C, G) | (A, C, U) -> Thr
  | (U, G, G) -> Trp
  | (U, A, C) | (U, A, U) -> Tyr
  | (G, U, A) | (G, U, C) | (G, U, G) | (G, U, U) -> Val
  | _ -> Stop

let decode_arn r =
  let rec loop t acc =
    match t with
    | [] -> List.rev acc
    | x :: xs ->
        let aa = aminoacid_of_triplet x in
        if aa = Stop then List.rev acc else loop xs (aa :: acc)
  in
  loop (generate_bases_triplets r) []

let string_of_protein p =
  let name = function
    | Stop -> "Stop"
    | Ala -> "Ala"
    | Arg -> "Arg"
    | Asn -> "Asn"
    | Asp -> "Asp"
    | Cys -> "Cys"
    | Gln -> "Gln"
    | Glu -> "Glu"
    | Gly -> "Gly"
    | His -> "His"
    | Ile -> "Ile"
    | Leu -> "Leu"
    | Lys -> "Lys"
    | Met -> "Met"
    | Phe -> "Phe"
    | Pro -> "Pro"
    | Ser -> "Ser"
    | Thr -> "Thr"
    | Trp -> "Trp"
    | Tyr -> "Tyr"
    | Val -> "Val"
  in
  String.concat "-" (List.map name p)

let life s =
  let h = generate_helix s in
  let hc = complementary_helix h in
  let r = generate_rna h in
  let p = decode_arn r in
  print_endline ("Input DNA : " ^ s);
  print_endline ("Helix     : " ^ helix_to_string h);
  print_endline ("Complement: " ^ helix_to_string hc);
  print_endline
    ("RNA       : " ^ String.concat "" (List.map string_of_nucleobase r));
  print_endline ("Protein   : " ^ string_of_protein p)
