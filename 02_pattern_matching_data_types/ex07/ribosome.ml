type nucleobase = A | T | C | G | U | None

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

let string_of_protein p =
  let string_of_aminoacid = function
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
  String.concat "-" (List.map string_of_aminoacid p)

let generate_bases_triplets rna =
  let rec loop r acc =
    match r with
    | a :: b :: c :: xs -> loop xs ((a, b, c) :: acc)
    | _ -> List.rev acc
  in
  loop rna []

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

let decode_arn rna =
  let triplets = generate_bases_triplets rna in
  let rec loop ts acc =
    match ts with
    | [] -> List.rev acc
    | t :: xs ->
        let aa = aminoacid_of_triplet t in
        if aa = Stop then List.rev acc else loop xs (aa :: acc)
  in
  loop triplets []
