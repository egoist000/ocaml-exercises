(* Sottoproblema: determinare se un carattere è numerico *)

(* numeric: char -> bool *)
let numeric c =
    c >= '0' && c <= '9'

(* conta_digits: string -> int *)
let conta_digits s =
    let max_index = (String.length s) - 1 in
    let rec loop i =
        if i >= max_index then 0
        else if numeric s.[i] then 1 + loop(i + 1)
        else loop(i + 1)
    in loop 0

let conta_digits_exn s = 
    let rec loop i = 
        try if numeric s.[i] then 1 + loop(i + 1)
        else loop(i + 1)
        with _ -> 0
    in loop 0
