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


