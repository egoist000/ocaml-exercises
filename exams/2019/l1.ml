(* Rappresentazione di un albero binario *)
type 'a tree = Empty | Tr of ('a * 'a tree * 'a tree)

(* n_ramo_bin: int -> int tree -> int list *)
(* n_ramo_bin n tree = una lista che rappresenta un ramo dalla radice ad 
 * una foglia in cui la somma di tali nodi è uguale a n *)

let n_ramo_bin n tree =
  (* aux: int -> int tree -> int list *)
  (* aux sum tree = una lista che rappresenta un ramo dalla radice del sottoalbero 
   * fino ad una foglia che contenga nodi la cui somma a partire da sum 
   * sia pari ad n *)
  let rec aux sum = function
    | Empty ->
        raise Not_found
    | Tr (l, Empty, Empty) ->
        if sum + l = n then [l] else raise Not_found
    | Tr (node, l1, l2) ->
        let new_sum = node + sum in
        if new_sum >= n then raise Not_found
        else node :: (try aux new_sum l1 with Not_found -> aux new_sum l2)
  in
  aux 0 tree
