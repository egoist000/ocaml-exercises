type 'a ntree = Tr of 'a * 'a ntree list

type col = Rosso | Giallo | Verde | Blu

type 'a col_assoc = (col * 'a list) list

let rec get_col e = function
  | [] ->
      failwith "Error"
  | (color, lst) :: rest ->
      if List.mem e lst then color else get_col e rest

(* ramo_colorato: 'a -> 'a col_assoc -> 'a ntree -> 'a list *)
(* ramo_colorato k assoc_lst tree = ramo a colori alterni dalla radice a una
 * foglia etichettada con k, se non esiste si solleverà un'eccezione *)

let rec ramo_colorato k assoc_lst = function
  | Tr (x, []) ->
      if k = x then [x] else failwith "path"
  | Tr (x, subtrees) ->
      let cur_color = get_col x assoc_lst in
      itera cur_color k assoc_lst subtrees

and itera c k assoc_lst = function
  | [] ->
      failwith "path"
  | (Tr (x, _) as t) :: trest ->
      if get_col x assoc_lst <> c then ramo_colorato k assoc_lst t
      else itera c k assoc_lst trest
