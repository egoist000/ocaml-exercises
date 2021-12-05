(* Problema: leggere una sequenza di interi terminata da un punto
 * e riportare la somma *)

exception BadInput

(* somma: unit -> int *)
let rec somma () = 
    let s = read_line ()
    in try if s = "." then 0
    else (int_of_string s) + somma ()
    with _ -> raise BadInput

(* versione iterativa *)

let somma' () =
    (* aux int -> int *)
    (* aux res = res + il valore della somma degli interi letti *)
    let rec aux res = 
        let s = read_line ()
        in try if s = "." then res
        else aux (res + (int_of_string s))
        with _ -> raise BadInput
    in aux 0

(* numero e somma degli interi letti da tastiera *)

(* numero_somma: unit -> int * int *)
(* numero_somma () = riporta numero e somma degli interi letti *)

let rec numero_somma () =
    let s = read_line () in
    if s = "."
    then (0, 0)
    else 
        let (tot, somma) = numero_somma ()
        in (tot + 1, somma + (int_of_string s))

let rec numero_somma' () =
    try let n = int_of_string(read_line ())
        in let (tot, somma) = numero_somma' ()
        in (tot + 1, somma + n)
    with _ -> (0, 0)

let numero_somma_it () = 
    let rec aux tot somma =
        try
            aux (tot + 1) (somma + (int_of_string(read_line ())))
        with _ -> (tot, somma)
    in aux 0 0
