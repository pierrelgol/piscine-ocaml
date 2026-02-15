type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide c =
  let base =
    match c with
    | 'A' | 'a' -> A
    | 'T' | 't' -> T
    | 'C' | 'c' -> C
    | 'G' | 'g' -> G
    | _ -> None
  in
  ("phosphate", "deoxyribose", base)
