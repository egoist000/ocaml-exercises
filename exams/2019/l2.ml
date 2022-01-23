(* Rappresentazione di un albero n-ario *)

type 'a ntree = Tr of 'a * 'a ntree list

(* n_ramo: int -> int ntree -> int list *)
(* n_ramo n ntree = una lista che rappresenta un ramo dalla radice ad una foglia
 * che contenga nodi la cui somma è n *)

let n_ramo n ntree =
  (* aux: int -> int ntree -> int list *)
  (* aux sum ntree -> una lista che rappresenta il cammino dalla radice 
   * del sottoalbero fino ad una sua foglia che contenga nodi la cui somma a 
   * partire da sum sia pari ad n, altrimenti solleva un'eccezione *)
  let rec aux sum = function
    | Tr (leaf, []) ->
        if sum + leaf = n then [leaf] else raise Not_found
    | Tr (node, sub_trees) ->
        let new_sum = node + sum in
        if new_sum >= n then raise Not_found
        else node :: iter_trees new_sum sub_trees
  (* iter_trees: int -> int ntree -> int list *)
  (* iter_trees sum subtrees = ricerca nei sottoalberi dell'albero 
   * della chiamata ricorsiva una foglia la cui somma a partire da sum sia pari
   * ad n, altrimenti solleva un' eccezione *)
  and iter_trees sum = function
    | [] ->
        raise Not_found
    | s :: srest -> (
      try aux sum s with Not_found -> iter_trees sum srest )
  in
  aux 0 ntree

(** Albero di test **)

let albero =
  Tr
    ( 5
    , [ Tr (7, [Tr (3, []); Tr (9, []); Tr (12, [])])
      ; Tr (1, [Tr (4, []); Tr (5, []); Tr (10, [])])
      ; Tr (3, [Tr (9, []); Tr (7, []); Tr (10, [])]) ] )
