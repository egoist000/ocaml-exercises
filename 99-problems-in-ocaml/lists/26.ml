(* Generate the combinations of K distinct objects chosen from the N elements of a list. (medium) *)

let rec extract n lst =
  if n <= 0 then [[]]
  else
    match lst with
    | [] ->
        []
    | x :: rest ->
        let l1 = List.map (function l -> x :: l) (extract (n - 1) rest) in
        let l2 = extract n rest in
        l1 @ l2
