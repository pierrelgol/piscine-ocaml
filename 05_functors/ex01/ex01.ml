module StringHashed = struct
  type t = string

  let equal a b = a = b

  let hash s =
    let rec loop i acc =
      if i = String.length s then acc
      else
        let code = Char.code (String.get s i) in
        loop (i + 1) (((acc lsl 5) + acc) + code)
    in
    loop 0 5381
end

module StringHashtbl = Hashtbl.Make (StringHashed)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k, v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
