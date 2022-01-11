type 'a graph = ('a * 'a) list

let successori n g = List.map snd (List.filter (function c, _ -> c = n) g)

(* connessi_in_glist: 'a graph list -> 'a -> 'a -> bool *)
(* connessi_in_glist glist b c = determina se in qualche grafo in glist 
 * esiste un cammino da B a C o viceversa *)

let esiste_cammino g start goal =
  let rec search visited = function
    | [] ->
        false
    | x :: rest ->
        if List.mem x visited then search visited rest
        else x = goal || search (x :: visited) (successori x g @ rest)
  in
  search [start] (successori start g)

let connessi_in_glist glist b c =
  b <> c
  && List.exists
       (function g -> esiste_cammino g b c || esiste_cammino g c b)
       glist
