(* Problema: attraversamento di un labirinto "quadrato", da una casella
 * di entrata a una casella di uscita, senza passare per caselle
 * che contengono un mostro. 
 * Ci si puo' spostare in orizzontale e in diagonale, ma solo verso
 * destra. La casella di ingresso e' nella colonna piu' a sinistra. *)

(** Rappresentazione del labirinto **)

(*   0  1  2  3  4
   0       
   1         
   2             
   3       
   4          
*)

(* Rappresentazione delle caselle contententi mostri *)
let mostri = [(0,2);(1,1);(1,3);(2,3);(3,0);(4,2)]

let in_labirinto dim (r, c) = 
    r >= 0 && c >= 0 && r < dim && c < dim

    (** ricerca percorso versione 1 **)

    (* path1: int -> (int * int) list -> int * int -> int * int 
     *  -> (int * int) list *)
    (* path1 dim mostri ingresso uscita = path, dove path è il cammino
     * nella matrice di dimensione dim che va dall'ingresso all'uscita senza
 * passare per le caselle con il mostro *)

exception NotFound

let path1 dim mostri ingresso uscita =
    let rec cerca_da ((r, c) as casella) =
        if not (in_labirinto dim casella) || List.mem casella mostri 
        then raise NotFound
        else
            if casella = uscita
            then [casella]
            else
                casella::
                    try cerca_da (r, c + 1) 
                    with NotFound ->
                        try cerca_da (r + 1, c + 1) 
                        with NotFound ->
                            cerca_da (r - 1, c + 1)
    in cerca_da ingresso

let rec filter_vicini dim = function
    | [] -> []
    | casella::rest ->
            if in_labirinto dim casella
            then casella::filter_vicini dim rest
            else filter_vicini dim rest

let vicini dim (r, c) =
    if in_labirinto dim (r, c)
    then filter_vicini dim [(r, c + 1); (r + 1, c + 1); (r - 1,  c + 1)]
    else raise NotFound

let path2 dim mostri ingresso uscita =
    let rec cerca_da ((r, c) as casella) =
        if not (in_labirinto dim casella) || List.mem casella mostri
        then raise NotFound
        else
            if casella = uscita
            then [casella]
            else
                casella::cerca_da_lista (vicini dim casella)
            and cerca_da_lista = function
                | [] -> raise NotFound
                | c::rest ->
                        try cerca_da c
                        with NotFound -> cerca_da_lista rest
    in cerca_da ingresso

