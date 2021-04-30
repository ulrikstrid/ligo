open Types

let bind_lmap (l:_ label_map) =
  let open Trace in
  let open LMap in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux l (ok empty)

let bind_fold_lmap f init (lmap:_ LMap.t) =
  let open Trace in
  let aux k v prev =
    prev >>? fun prev' ->
    f prev' k v
  in
  LMap.fold aux lmap init

let bind_map_lmap f map = bind_lmap (LMap.map f map)
let bind_map_lmap_t f map = bind_lmap (
  LMap.map 
    (fun ({associated_type;_}) -> 
      f associated_type)
    map)
let bind_map_lmapi f map = bind_lmap (LMap.mapi f map)

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map (fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all (fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let get_pair m =
  match (LMap.find_opt (Label "0") m , LMap.find_opt (Label "1") m) with
  | Some {associated_type=e1;_}, Some {associated_type=e2;_} -> Some (e1,e2)
  | _ -> None

let tuple_of_record (m: _ LMap.t) =
  let aux i = 
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind (fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux
