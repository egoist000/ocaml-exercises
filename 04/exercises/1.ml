(* length: a' list -> int *)
(* length lst = la lunghezza della lista lst *)

let rec length = function
    | [] -> 0
    | x::rest -> 1 + length rest

(* versione iterativa *)

let length' lst =
    (* loop: int -> a' list -> int *)
    (* loop len lst = la linghezza della lista a partire da len *)
    let rec loop len = function
        | [] -> len
        | x::rest -> loop (len + 1) rest
    in loop 0 lst

(* sumof: int list -> int *)
(* sumof lst = la somma di tutti gli interi nella lista *)

let rec sumof = function
    | [] -> 0
    | x::rest -> x + sumof rest

(* versione iterativa *)

let sumof' lst =
    (* loop: int -> int list -> int *)
    (* loop s lst = la somma degli elementi di lst + s *)
    let rec loop s = function
        | [] -> s
        | x::rest -> loop (s + x) rest
    in loop 0 lst

exception EmptyList

(* maxlst: a' list -> a' *)
(* maxlst lst = il massimo elemento in una lista *)

let maxlst lst =
    let rec aux maxc = function
        | [] -> maxc
        | x::rest -> aux (max x maxc) rest
    in try aux (List.hd lst) lst with
    | _ -> raise EmptyList

(* drop: int -> a' list -> a' list *)
(* drop n lst = una lista che si ottiene da lst 
 * rimuovendone i primi n elementi *)

let rec drop n = function
    | [] -> []
    | x::rest -> 
            if n <= 0 then x::rest
            else drop (n - 1) rest

(* reverse: a' list -> a' list *)
(* reverse lst = la lista lst con l'ordine degli elementi inverso *)

let rec reverse = function
    | [] -> []
    | x::rest -> (reverse rest) @ [x]

(* versione iterativa *)

let reverse' lst =
    let rec loop tlist = function
        | [] -> tlist
        | x::rest -> loop ([x] @ tlist) rest
    in loop [] lst

exception OutOfBounds

(* nth: int -> a' list -> a' *)
(* nth n lst = n-esimo elemento di lst *)

let nth n lst =
    let rec aux i = function
        | [] -> raise OutOfBounds
        | x::rest -> 
                if n = i then x
                else aux (i + 1) rest
    in aux 0 lst

(* remove: a' -> a' list -> a' list *)
(* remove x lst = rimuove tutte le occorrenze di x in lst *)

let rec remove x = function
    | [] -> []
    | e::rest -> 
            if x = e then remove x rest
            else [e] @ remove x rest

(* versione iterativa *)

let remove x lst =
    let rec loop tlist = function 
        | [] -> tlist
        | e::rest ->
                if x = e then loop tlist rest
                else loop ([e] @ tlist) rest
    in loop [] lst


