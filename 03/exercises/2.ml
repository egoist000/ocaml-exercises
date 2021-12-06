(* A *)
(* read_max: unit -> int *)
(* read_max () = il massimo della sequenza di interi letti da tastiera *)

exception NaN

let read_max () =
    (* loop: int -> int *)
    (* loop curr_max = il massimo tra gli eventuali numeri letti da tastiera 
     * e il massimo dell'iterazione corrente *)
    let rec loop curr_max =
        try let num = read_int ()
            in loop (max curr_max num)
        with _ -> curr_max
    in try
        let n1 = read_int ()
        in loop n1
        with _ -> raise NaN

(* B *)
(* read_max_min: () -> int * int *)
(* read_max_min () = la coppia (massimo, minimo) degli interi letti da tastiera
 * separati da ENTER e terminati da stringa non rappresentante un intero *)

let read_max_min () =
    let rec loop curr_max curr_min =
        try let num = read_int ()
            in loop (max curr_max num) (min curr_min num)
        with _ -> (curr_max, curr_min)
    in try
        let n1 = read_int ();
        in loop n1 n1
    with _ -> raise NaN

(* C *)
(* tutti_minori: int -> bool *)
(* tutti_minori n = true se tutti i numeri letti da tastiera sono minori di n
 * false altrimenti *)

let rec tutti_minori n =
    try let num = read_int ()
        in num < n && tutti_minori n
    with _ -> true

(* D *)
(* occorre: int -> bool *)
(* occorre n = true se il numero occorre nella sequenza, false altrimenti *)

let rec occorre n =
    try let num = read_int ()
        in n = num || occorre n
    with _ -> false

(* E *)
(* num_di_stringhe: unit -> int *)
(* num_di_stringhe () = legge una sequenza di stringhe non vuote separate da 
 * ENTER e terminata dalla stringa vuota e ritorna il numero di stringhe *)

let rec num_di_stringhe () =
    let line = read_line ()
    in if line = "" then 0
    else 1 + num_di_stringhe ()

(* F *)
(* stringa_max: unit -> string *)
(* stringa_max () = ritorna la stringa piu lunga della sequenza *)

let stringa_max () =
    (* loop: string -> string *)
    (* loop max_s = la stringa più grande della lista, grande almeno max_s *)
    let rec loop max_s =
        let line = read_line () 
        in if line = "" then max_s
        (* In caso di stringhe di stessa lunghezza si preferisce l'ultima *)
        else loop (max line max_s)
    in loop ""
