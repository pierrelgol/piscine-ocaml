type 'a ft_ref = { mutable contents : 'a }

let return v = { contents = v }
let get r = r.contents
let set r v = r.contents <- v
let bind r f = f r.contents
