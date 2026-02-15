type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = phosphate * deoxyribose * nucleobase

type helix = nucleotide list

let random_base () =
  match Random.int 4 with 0 -> A | 1 -> T | 2 -> C | _ -> G

let make_nucleotide base = ("phosphate", "deoxyribose", base)

let generate_helix n =
  let rec loop i acc =
    if i <= 0 then acc else loop (i - 1) (make_nucleotide (random_base ()) :: acc)
  in
  if n <= 0 then [] else loop n []

let char_of_base = function A -> "A" | T -> "T" | C -> "C" | G -> "G" | None -> "N"

let helix_to_string h =
  let rec loop hx acc =
    match hx with
    | [] -> acc
    | (_, _, b) :: xs -> loop xs (acc ^ char_of_base b)
  in
  loop h ""

let complement = function A -> T | T -> A | C -> G | G -> C | None -> None

let complementary_helix h =
  let rec loop hx acc =
    match hx with
    | [] -> List.rev acc
    | (_, _, b) :: xs -> loop xs (make_nucleotide (complement b) :: acc)
  in
  loop h []
