(* esempio di dichiarazione di un nuovo tipo *)

type seme = Bastoni | Coppe | Denari | Spade
type valore = Asso | Due | Tre | Quattro | Cinque | Sei | Sette | Fante |
              Cavallo | Re


let settebello = (Sette, Denari)

let briscola = [(Asso,Denari);(Due, Spade);(Tre,Bastoni)]

(* valore : valore -> int *)
let valore = function
    | Asso -> 1
    | Due -> 2
    | Tre -> 3
    | Quattro -> 4
    | Cinque -> 5
    | Sei -> 6
    | Sette -> 7
    | Fante -> 8
    | Cavallo -> 9
    | Re -> 10
