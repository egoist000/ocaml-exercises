(* copy: int -> a' -> a' list *)
(* copy n x = una lista di lunghezza n con tutti elementi uguali a x *)

let rec copy n x =
    match n with
    | 0 -> []
    | _ -> (if n < 0 then []
            else x::copy (n - 1) x)

let copy' n x =
    (* aux: a' list -> int -> a' list *)
    (* aux lst i = una lista contenente i volte x *)
    let rec aux lst i =
        if i <= 0 then lst
        else aux (x::lst) (i - 1)
    in aux [] n

(* nondec: int list -> bool *)
(* nondec lst = true se tutti gli elementi della lista 
 * sono in ordine non decrescente, false altrimenti *)

let rec nondec = function
    | [] -> true
    | _::[] -> true
    | x::y::rest -> 
            if x > y then false
            else nondec (y::rest)

(* pairwith: a' -> b' list -> (a' * b') list *)
(* pairwith y xs = una lista di coppie (y, x1)..(y, xn) *)

let rec pairwith y = function
    | [] -> []
    | x::rest -> (y, x)::pairwith y rest

let pairwith' y xs =
    (* aux: a' list -> (a' * b') list *)
    (* aux lst xs = [] @ (y, x) @ xs *)
    let rec aux lst = function
        | [] -> lst
        | x::rest -> aux (lst @ [(y, x)]) rest
    in aux [] xs

(* duplica: a' list -> a' list *)
(* duplica lst = una lista contenente ogni elemento di lst duplicato *)

let rec duplica = function
    | [] -> []
    | x::rest -> x::x::duplica rest

let duplica' lista=
    let rec aux lst = function
        | [] -> lst
        | x::rest -> aux (lst @ [x; x]) rest
    in aux [] lista

(* enumera: a' list -> (int * a') list *)
(* enumera lst = una lista di coppie (0, x1)..(k, xk) dove x sono gli elementi
 * di lst *)

let enumera lst =
    let rec aux i = function
        | [] -> []
        | x::rest -> (i, x)::aux (i + 1) rest
    in aux 0 lst

(* position: a' -> a' list -> int *)
(* position x lst = la posizione della prima occorrenza di x in lst.
 * se l'elemento non è presente solleva un eccezione *)

let position x lst =
    let rec loop i = function
        | [] -> raise Not_found
        | e::rest -> 
                if x = e then i
                else loop (i + 1) rest
    in loop 0 lst

let rec position' x = function
    | [] -> raise Not_found
    | e::rest -> 
            if x = e then 1
            else 1 + position' x rest

(* alternate: a' list -> a' list *)
(* alternate lst = una lista che riporta gli elementi di lst 
 * con indice dispari *)

let alternate lst = 
    let rec loop i = function
        | [] -> []
        | x::rest -> 
                if i mod 2 <> 0 then x::loop (i + 1) rest
                else loop (i + 1) rest
    in loop 0 lst

exception Err

let maxlst lst =
    let rec aux m = function
        | [] -> m
        | x::rest -> aux (max m x) rest
    in try aux (List.hd lst) lst with
    | _ -> raise Err

let rec min_dei_max = function
    | [] -> raise Err
    | [lst] -> maxlst lst
    | lst::rest -> min (maxlst lst) (min_dei_max rest)

let min_dei_max' lst =
    let rec loop mlst = function
        | [] -> mlst
        | x::rest -> loop (min mlst (maxlst x)) rest
    in 
    match lst with
    | [] -> raise Err
    | x::rest -> loop (maxlst x) rest

let rec drop n = function
    | [] -> []
    | x::rest -> 
            if n <= 0 then x::rest
            else drop (n - 1) rest;;

let rec take n = function
    | [] -> []
    | x::rest -> 
            if n <= 0 then []
            else x::take (n - 1) rest;;

let split2 lst =
    let rec aux l = function
        | [] -> l
        | x::rest -> aux (l + 1) rest
    in
    match lst with
    | [] -> ([], [])
    | [x] -> ([x], [])
    | lista ->
            let len = aux 0 lista
            in (take (len / 2) lista, drop (len / 2) lista)
