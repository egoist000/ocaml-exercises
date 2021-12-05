(* Problema: dato un intero m stampare tutti gli interi tra 0 e m *)
(* Sottoproblema: stampare gli interi compresi tra n e m (estremi inclusi) *)

(* ciclo: int -> int -> unit *)
(* ciclo n m = stampa gli interi tra n e m (estremi inclusi) *)

let rec ciclo n m =
    if n > m then ()
    else (print_int n; print_newline (); ciclo (n + 1) m)

(* stampa: int -> unit *)
(* stampa m stampa gli interi da 0 a m *)

let stampa = ciclo 0

(* Note: ciclo è 'Tail recursive' alla fine della chiamata ricorsiva 
 * la funzione non deve calcolare nient'altro e l'esecuzione termina *)
(* Viene implementata un iterazione *)


(* Un esempio di una funzione non tail recursive *)

let rec fact = function
    | 0 -> 1
    | n -> n * fact (n - 1)

(* In questo caso non viene implementata un'iterazione,
 * per ottenere il risultato è necessario aspettare la fine di tutte le 
 * chiamate ricorsive, e infine calcolare i rispettivi valori *)

(* fact versione tail recursive *)

let fact' n = 
    let rec aux f = function
        | 0 -> f
        | n -> aux (f * n) (n - 1) (* Iterazione successiva *)
    in aux 1 n
