(* Extract a given number of randomly selected elements from a list. (medium) *)

let rec extract index = function
  | [] ->
      raise Not_found
  | x :: rest ->
      if index = 0 then (x, rest)
      else
        let el, rlst = extract (index - 1) rest in
        (el, x :: rlst)

let rec rand_select lst = function
  | 0 ->
      []
  | n ->
      let el, newlst =
        try extract (Random.int (List.length lst)) lst
        with _ -> failwith "Out_of_bounds"
      in
      el :: rand_select newlst (n - 1)
