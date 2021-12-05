let is_numeric c =
    c >= '0' && c <= '9'

(* conta_digits: string -> int *)
(* conta_digits s = numero di caratteri numerici in s *)

let conta_digits s =
    let rec loop i =
        try if is_numeric s.[i] then 1 + loop (i + 1)
        else loop (i + 1)
        with _ -> 0
    in loop 0

(* Vediamo ora la versione iterativa *)

let conta_digits_it s =
    (* loop: int -> int -> int *)
    (* loop res i = il numero di caratteri numerici a partire da i *)
    let rec loop res i =
        try if is_numeric s.[i] then loop (res + 1) (i + 1)
        else loop res (i + 1)
        with _ -> res
    in loop 0 0

(* result rappresenta il risultato parziale inizializzato a 0 *)
