class ['a] army =
  object
    val mutable members : 'a list = []

    method add x = members <- x :: members

    method delete =
      match members with
      | [] -> ()
      | _ :: xs -> members <- xs

    method members = members
  end
