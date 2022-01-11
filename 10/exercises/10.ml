(** qui sotto la soluzione dell'esercizio 3 del Gruppo 7 *)

(** Ridenomino la funzione successori in veryHardSucc, per non
    fare confusione con la funzione successori generale per i grafi *)

type chiave = Aperta | Chiusa

type cassaforte = chiave list

exception Fail

(* gira: chiave -> chiave, gira una chiave *)

let gira = function Aperta -> Chiusa | Chiusa -> Aperta

(* giraPrima: cassaforte -> cassaforte, riporta la configurazione
   che si ottiene girando la prima chiave *)

let giraPrima = function x :: rest -> gira x :: rest | _ -> raise Fail

(* giraDopoChiusa: cassaforte -> cassaforte,  riporta la configurazione
   che si ottiene girando la chiave che segue la prima chiusa.
   Fallisce se non è possibile eseguire l’operazione *)

let rec giraDopoChiusa = function
  | Chiusa :: x :: rest ->
      Chiusa :: gira x :: rest
  | Aperta :: rest ->
      Aperta :: giraDopoChiusa rest
  | _ ->
      raise Fail

(* veryHardSucc: cassaforte -> cassaforte list *)
(* la funzione chiamata successori nell'es 3 del gruppo 7 *)

let veryHardSucc config =
  giraPrima config :: (try [giraDopoChiusa config] with Fail -> [])

(* nodi: int -> cassaforte list *)
(* nodi n = tutte le possibili configurazioni di una cassaforte con n chiavi *)

let rec nodi n =
  if n <= 0 then [[]]
  else
    let n = nodi (n - 1) in
    List.map (function lst -> Aperta :: lst) n
    @ List.map (function lst -> Chiusa :: lst) n

let archi n =
  let nodes = nodi n in
  List.map (function c -> (c, veryHardSucc c)) nodes

let rec start n = if n <= 0 then [] else Chiusa :: start (n - 1)

let rec aperta = function
  | [] ->
      true
  | c :: crest ->
      c = Aperta && aperta crest

let apri n =
  let grafo = archi n in
  let rec from_node c visited =
    if List.mem c visited then raise Not_found
    else if aperta c then [c]
    else c :: from_list (c :: visited) (List.assoc c grafo)
  and from_list visited = function
    | [] ->
        raise Not_found
    | n :: nrest -> (
      try from_node n visited with _ -> from_list visited nrest )
  in
  from_node (start n) []
