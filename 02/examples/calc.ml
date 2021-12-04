(* Calcolatrice per interi non negativi *)
(* somma, differenza, moltiplicazione e divisione *)

exception BadOperation
exception BadInt

let numeric c =
    c >= '0' && c <= '9'

(* substring: string -> int -> int -> string *)
(* substring s j k = sottostringa di s che va dalla posizione j a k *)

let substring s j k =
    String.sub s j ((k - j) + 1)

(* primo_non_numerico: string -> int *)
(* primo_non_numerico s = posizione del primo carattere non numerico in s. 
 * Errore se non esiste *)

let primo_non_numerico s =
    (* loop: int -> int *)
    (* indice del primo carattere non numerico in s a partire da i *)
    let rec loop i =
        if not (numeric s.[i]) then i
        else loop(i + 1)
    in try loop 0
    with _ -> raise BadOperation

(* split_string: string -> int * char * int *)

let split_string s =
    let i = primo_non_numerico s
    in try (int_of_string (substring s 0 (i - 1)), 
        s.[i], 
        int_of_string (substring s (i + 1) ((String.length s) - 1)))
    with _ -> raise BadInt

(* evaluate: string -> int *)
(* evaluate s = valore numerico dell'espressione s.
 * Errore se s non rappresenta un espressione aritmetica semplice *)

let evaluate s =
    let (n, op, m) = split_string s
    in match op with
    | '+' -> n + m
    | '-' -> n - m
    | '*' -> n * m
    | '/' -> n / m
    | _ -> raise BadOperation
