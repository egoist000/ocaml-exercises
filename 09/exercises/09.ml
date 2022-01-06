type 'a ntree = Tr of 'a * 'a ntree list

(* ramo_di_primi: int ntree -> int *)
(* ramo_di_primi tree = una foglia n tale che il ramo dalla radice alla foglia
 * sia composto solo da nodi con numeri primi *)

(* is_prime: int -> bool *)
(* is_prime n = true se n è primo, false altrimenti *)
let is_prime n =
  let rec aux = function 1 -> true | k -> n mod k <> 0 && aux (k - 1) in
  n > 1 && aux (n / 2)

let rec ramo_di_primi = function
  | Tr (x, []) ->
      if is_prime x then x else raise Not_found
  | Tr (x, subtrees) ->
      if is_prime x then iter subtrees else raise Not_found

(* iter: 'int ntree list -> int *)
(* iter treelst = cerca un ramo valido tra tutti i sottoalberi di x
 * e riporta la foglia, Not_found se nessun sottoalbero ritorna un ramo valido *)
and iter = function
  | [] ->
      raise Not_found
  | t1 :: trest -> (
    try ramo_di_primi t1 with Not_found -> iter trest )
