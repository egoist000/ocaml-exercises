(* data: int * string -> bool *)
(* data (d, m) = ritorna true se la coppia giorno d e mese m rappresenta una 
 * data coretta, false altrimenti *)
(* Si assume che l'anno non sia bisestile *)

let data (d, m) =
    d >= 1 &&
    match m with
    | "novembre" | "aprile" | "giugno" | "settembre" -> d <= 30
    | "febbraio" -> d <= 28
    | "gennaio" | "marzo" | "maggio" | "luglio" | "agosto" | "ottobre" -> d <= 31
    | _ -> false
