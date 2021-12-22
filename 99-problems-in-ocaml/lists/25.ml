let rec extract index = function
  | [] ->
      raise Not_found
  | x :: rest ->
      if index = 0 then (x, rest)
      else
        let el, rlst = extract (index - 1) rest in
        (el, x :: rlst)

(* Generate a random permutation of the elements of a list. (easy) *)

let rec permutation = function
    | [] -> []
    | x::rest as lst ->
            let (el, newlst) = extract (Random.int (List.length lst)) lst
            in el::permutation newlst
