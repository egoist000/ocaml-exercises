type direzione = Su | Giu | Destra | Sinistra
type posizione = int * int * direzione
type azione = Gira | Avanti of int

(* gira : direzione -> direzione *)
let gira = function
    | Su -> Destra
    | Giu -> Sinistra
    | Destra -> Giu
    | Sinistra -> Su

(* avanti : posizione -> int -> posizione *)
let avanti (x,y,dir) n =
    match dir with
    | Su -> (x,y+n,dir)
    | Giu -> (x,y-n,dir)
    | Destra -> (x+n,y,dir)
    | Sinistra -> (x-n,y,dir)

(* sposta : posizione -> azione -> posizione *)
let sposta (x,y,dir) act =
    match act with
    | Gira -> (x,y,gira dir)
    (* le coordinate non cambiano,
    la direzione gira di 90 gradi in senso orario *)
    | Avanti n -> avanti (x,y,dir) n

(* esegui: posizione -> azione list -> posizione *)

let rec esegui p = function
    | [] -> p
    | x::rest -> esegui (sposta p x) rest

