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

