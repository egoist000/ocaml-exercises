(* find ('a -> bool) -> 'a list -> 'a *)
(* find p lst = primo elemento nella lista che soddisfa p *)

let rec find p = function
    | [] -> raise Not_found
    | x::rest ->
            if p x then x
            else find p rest

let find_applicata lst = find (function x -> x * x > 30) lst

(* takewhile: ('a -> bool) 'a list -> 'a list *)
(* takewhile p lst = la piu lunga parte di lst formata da tutti elementi che 
 * soddisfano p *)

let takewhile p lst = 
    let rec aux tmp maxl = function
        | [] -> maxl
        | x::rest ->
                if p x then aux (x::tmp) maxl rest
                else if List.length tmp > List.length maxl
                then aux [] tmp rest
                else aux [] maxl rest
    in aux [] [] lst

(* piu lunga parte iniziale *)

let rec takewhile' p = function
    | [] -> []
    | x::rest ->
            if p x then x::takewhile p rest
            else []

(* dropwhile ('a -> bool) -> 'a list -> 'a list *)
(* dropwhile p lst = una lista ottenuta eliminando i primi elementi di lst 
 * fino a che soddisfano il predicato p *)

let rec dropwhile p = function
    | [] -> []
    | x::rest ->
            if p x then dropwhile p rest
            else x::rest

(* partition: ('a -> bool) -> 'a list -> ('a list * 'a list) *)
(* partition p lst = (yes, no) dove yes contiene tutti gli elementi che 
 * soddisfano p e no tutti gli elementi che non soddisfano p *)

let rec partition p = function
    | [] -> ([], [])
    | x::rest ->
            let prest = partition p rest
            in if p x then (x::fst prest, snd prest)
            else (fst prest, x::snd prest)

(* pairwith: 'a -> 'b list -> ('a * 'b) list *)
(* pairwith y xlst = [(y, x1)..(y, xn)] *)

let pairwith y xlst = List.map (function x -> (y, x)) xlst

(* verifica_matrice: int -> int list list -> bool *)
(* verifica_matrice n matrix = true se la matrice contiene almeno una riga
 * in cui gli elementi sono tutti minori di n, false altrimenti *)

let rec verifica_matrice n matrix =
    List.exists (List.for_all (function x -> x < n)) matrix

(* setdiff: 'a list -> 'a list -> 'a list *)
(* setdiff set1 set2 = differenza insiemistica tra set1 e set2 *)

let setdiff set1 set2 = List.filter (function e -> not(List.mem e set2)) set1

(* subset: 'a list -> 'a list -> bool *)
(* subset set1 set2 = true se set1 rappresenta un sottoinsieme di set2 *)

let subset set1 set2 = List.for_all (function e -> List.mem e set2) set1

(* duplica: int list -> int list *)
(* duplica lst = una lista dove tutti gli elementi sono raddoppiati *)

let duplica lst = List.map (function n -> n * 2) lst

(* mapcons: ('a * 'b list) list -> 'b -> ('a * 'b list) list *)
(* mapcons lst x = applicata a una lista di coppie ritorna una lista di 
 * coppie dove per ognuna di esse x è stato messo in testa al secondo elemento
 * di lst *)

let mapcons lst x = List.map (function (e, lst) -> (e, x::lst)) lst

(* tutte_liste_con: int -> 'a -> 'a -> 'a list list *)
(* tutte_liste_con n x y = una lista contenente tutte le possibili liste di 
 * lunghezza n contenenti soltanto i valori x e y *)

let rec tutte_liste_con n x y =
    if n <= 0 then [[]]
    else
        let tlc = tutte_liste_con (n - 1) x y in 
        List.map (function lst -> x::lst) tlc @
        List.map (function lst -> y::lst) tlc

