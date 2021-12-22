(* Create a list containing all integers within a given range. (easy) *)

(* If first argument is greater than second, produce a list in decreasing order. *)

let range i k =
  let rec aux count s =
    if count = s then [ s ] else count :: aux (count + 1) s
  in
  if i > k then List.rev (aux k i) else aux i k
