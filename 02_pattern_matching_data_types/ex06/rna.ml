type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

type nucleotide = phosphate * deoxyribose * nucleobase

type helix = nucleotide list

type rna = nucleobase list

let complementary_base = function
  | A -> U
  | T -> A
  | C -> G
  | G -> C
  | U -> A
  | None -> None

let generate_rna h =
  let rec loop hx acc =
    match hx with
    | [] -> List.rev acc
    | (_, _, b) :: xs -> loop xs (complementary_base b :: acc)
  in
  loop h []
