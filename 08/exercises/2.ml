(* Definizione di albero *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* albero di esempio *)
let albero = Tr("A", Tr("H", Tr("X", Tr("Y", Empty, Empty), Empty), Empty),
                Tr("O", Tr("I", Empty, Tr("M", Empty, Tr("T", Empty, Empty))), 
                Empty))

(* reflect 'a tree -> 'a tree *)
(* reflect t = un albero specchiato *)

let rec reflect = function
    | Empty -> Empty
    | Tr(x, l, r) -> Tr(x, reflect r, reflect l)

(* fulltree int -> int tree *)
(* fulltree n = un albero di altezza n dove la radice è etichettata con 1 
 * e i figli da 2k e 2k + 1 dove k è l'etichetta del padre *)

let fulltree n =
    let rec aux k h =
        if h >= n then Empty
        else let double_k = 2 * k
        in Tr(k, aux double_k (h + 1), aux (double_k + 1) (h + 1))
    in aux 1 0

(* balanced. 'a tree -> bool *)
(* balanced t = true se l'albero è bilanciato, false altrimenti
 * un albero è bilanciato quando per ogni nodo n l'altezza dei sottoalberi
 * destro e sinistro differisce di al massimo di 1 *)

let rec t_height = function
    | Empty -> 0
    | Tr(_, l, r) ->
            1 + max(t_height l) (t_height r)

let rec balanced = function
    | Empty -> true
    | Tr(_, l, r) ->
            balanced l && balanced r && abs(t_height l - t_height r) <= 1
           
let rec balanced2 t = 
    let rec aux = function
        | Empty -> 0
        | Tr(_, l, r) ->
                let ll = aux l
                and rl = aux r
                in if abs(ll - rl) <= 1
                then 1 + max ll rl
                else failwith "Not_Balanced"
    in try let _ = aux t in true
    with _ -> false

(* preorder: 'a tree -> 'a list *)
(* visita in preordine dell'albero *)

let rec preorder = function
    | Empty -> []
    | Tr(x, l, r) ->
            x::(preorder l) @ (preorder r)

(* postorder: 'a tree -> 'a list *)
(* visita in postordine dell'albero *)

let rec postorder = function
    | Empty -> []
    | Tr(x, l, r) ->
            (postorder l) @ (postorder r) @ [x]

(* inorder: 'a tree -> 'a list *)
(* visita simmetrica dell'albero *)

let rec inorder = function
    | Empty -> []
    | Tr(x, l, r) ->
            (inorder l) @ (x::inorder r)

(* balpreorder: 'a list -> 'a tree *)
(* balpreorder lst = un albero etichettato dagli elementi di lst tale che
 * preorder (balorder lst) = lst *)

let rec take n = function
    | [] -> []
    | x::rest ->
            if n <= 0 then []
            else x::take (n - 1) rest

let rec drop n = function
    | [] -> []
    | x::rest ->
            if n <= 0 then x::rest
            else drop (n - 1) rest

let rec balpreorder = function
    | [] -> Empty
    | x::rest ->
            let m = (List.length rest) / 2 
            in Tr(x, balpreorder (take m rest), balpreorder (drop m rest))

let rec balinorder lst =
    match lst with
    | [] -> Empty
    | x::rest ->
            let m = (List.length lst) / 2
            in Tr(List.nth lst m, balinorder (take m lst), 
                balinorder (drop m rest))
