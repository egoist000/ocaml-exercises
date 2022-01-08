let rec vicini nodo = function
  | [] ->
      []
  | (x, y) :: rest ->
      if x = nodo then y :: vicini nodo rest
      else if y = nodo then x :: vicini nodo rest
      else vicini nodo rest

(*
 * Problema: attraversamento di un labirinto, da una casella di entrata a una
 * casella di uscita, senza passare per caselle che contengono un
 * mostro, e raccogliendo tutti gli oggetti che si trovano nelle altre
 * caselle (se ce ne sono)
 *)

(* un labirinto è rappresentato mediante un grafo non orientato *)
(* rappresento un grafo mediante una lista di archi *)
type 'a graph = ('a * 'a) list

let grafo =
  [ (1, 2)
  ; (1, 3)
  ; (1, 4)
  ; (2, 3)
  ; (2, 4)
  ; (2, 7)
  ; (3, 5)
  ; (4, 6)
  ; (4, 7)
  ; (5, 7)
  ; (6, 7) ]

(* I nodi possono contenere degli oggetti *)
type content = Mostro | Obj of string

(* Rappresentiamo il contenuto di tutte le caselle del labirinto *)
type 'a contents = ('a * content list) list

let contents =
  [ (1, [Obj "oro"])
  ; (2, [Mostro])
  ; (4, [Obj "computer"; Obj "penna"])
  ; (7, [Mostro; Obj "libro"]) ]

(* un labirinto e' un: 'a graph * 'a contents *)
type 'a labirinto = 'a graph * 'a contents

let labirinto = (grafo, contents)

(* Funzione di supporto, trovare il contenuto di una casella *)
(* content: 'a -> 'a contents -> 'b list *)

let content x contents = try List.assoc x contents with _ -> raise Not_found

(* verificare se una casella ha un mostro *)
(* has_monster: 'a -> 'a contents -> bool *)

let has_monster x contents = List.mem Mostro (content x contents)

(* primo passo: adattiamo l'algoritmo di ricerca di cammino tenendo
   in considerazione la presenza dei mostri, ma ignorando la raccolta
   degli oggetti *)
(* path: 'a labirinto -> 'a -> 'a -> 'a list *)
let path ((grafo, contenuti) : 'a labirinto) ingresso uscita =
  let rec cerca_da visited casella =
    if List.mem casella visited || has_monster casella contenuti then
      raise Not_found
    else if casella = uscita then [casella]
    else casella :: cerca_da_una_tra (casella :: visited) (vicini casella grafo)
  and cerca_da_una_tra visited = function
    | [] ->
        raise Not_found
    | x :: rest -> (
      try cerca_da visited x with Not_found -> cerca_da_una_tra visited rest )
  in
  cerca_da [] ingresso

(* Ora vogliamo in uscita il cammino e la lista degli oggetti raccolti*)

let path ((grafo, contenuti) : 'a labirinto) ingresso uscita =
  let rec cerca_da visited casella =
    if List.mem casella visited || has_monster casella contenuti then
      raise Not_found
    else if casella = uscita then ([casella], content casella contenuti)
    else
      let cammino, oggetti =
        cerca_da_una_tra (casella :: visited) (vicini casella grafo)
      in
      (casella :: cammino, content casella contenuti @ oggetti)
  and cerca_da_una_tra visited = function
    | [] ->
        raise Not_found
    | x :: rest -> (
      try cerca_da visited x with Not_found -> cerca_da_una_tra visited rest )
  in
  cerca_da [] ingresso
