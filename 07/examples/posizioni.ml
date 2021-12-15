
(* Data la posizione (punto del piano (x, y ) e una direzione)
 * dell’oggetto e un’azione di spostamento o di cambiamento di
 * direzione, calcolare la nuova posizione dell’oggetto. *)

(* direzione *)

type direzione = Su | Giu | Destra | Sinistra

(* posizione come coppia di interi (x, y) *)

type posizione = int * int * direzione

(* selettori del tipo posizione *)

let xcoord (x, _, _) = x

let ycoord (_, y, _) = y

let dir (_, _, d) = d

(* azioni da considerare:
 * Girare di 90° in senso orario 
 * andare avanti di n passi (n intero) *)

(* costruttore funzionale -> Avanti *)

type azione = Gira | Avanti of int 
(* of int -> tipo degli oggetti a cui il costruttore si applica *)

let gira = function
    | Su -> Destra
    | Giu -> Sinistra
    | Destra -> Giu
    | Sinistra -> Su

let avanti (x, y, dir) n =
    match dir with
    | Su -> (x, y + n, dir)
    | Giu -> (x, y - n, dir)
    | Destra -> (x + n, y, dir)
    | Sinistra -> (x - n, y, dir)

let sposta (x, y, dir) act =
    match act with
    | Gira -> (x, y, gira dir)
    | Avanti n -> avanti (x, y, dir) n
