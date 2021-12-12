(* explode: string -> char list *)
(* explode str = trasforma una stringa in una lista di caratteri che la 
 * rappresentano *)

let explode str =
    (* loop: char list -> int -> char list *)
    (* loop lst i = una lista contenente i caratteri di str a partire da i
     * :: lst *)
    let rec loop lst i = 
        try loop (str.[i]::lst) (i + 1)
        with _ -> List.rev lst
    in loop [] 0

let explode' str =
    (* aux: int -> char list *)
    (* aux i = una lista contenente i caratteri di str a partire da i *)
    let rec aux i =
        try str.[i]::aux (i + 1)
        with _ -> []
    in aux 0

(* implode: char list -> string *)
(* implode lst = una stringa ottenuta da lst concatenando tutti 
 * i suoi elementi *)

let rec implode = function
    | [] -> ""
    | c::rest ->
            (Char.escaped c) ^ implode rest

let implode' clst =
    (* loop: string -> char list -> string *)
    (* loop str clst = una stringa costruita a partire da str ^ caratteri 
     * della lista clist *)
    let rec loop str = function
        | [] -> str
        | c::rest ->
                loop (str ^ Char.escaped c) rest
    in loop "" clst
