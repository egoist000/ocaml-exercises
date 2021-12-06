(* sumbetween: int -> int -> int *)
(* sumbetween n m = somma degli interi tra n e m (estremi inclusi) *)

let rec sumbetween n m =
    if n > m then 0
    else n + sumbetween (n + 1) m

(* sumto: int -> int *)
(* sumto n = somma degli interi compresi tra 0 e n (n >= 0) *)

let rec sumto n = 
    match n with
    | 0 -> n
    | num -> num + sumto (num - 1)

(* versione smart *)

let sumto' n = sumbetween 0 n

(* power: int -> int -> int *)
(* power n k = k-esima potenza di n *)

let rec power n = function
    | 0 -> 1
    | k -> n * power n (k - 1)

(* fib: int -> int *)
(* fib n = n-esimo numero di fibonacci *)

let rec fib = function
    | 0 -> 1
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

(* maxstring: string -> char *)
(* maxstring s = massimo carattere in s secondo codice ASCII *)

exception EmptyString

let maxstring s =
    let rec loop c_max i =
        try loop (max s.[i] c_max) (i + 1)
        with _ -> c_max
    in try loop s.[0] 0
    with _ -> raise EmptyString
        
