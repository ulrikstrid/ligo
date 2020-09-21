open Errors
module AST = Ast_typed
module Append_tree = Tree.Append

open Trace
open Mini_c

module LMap = AST.Types.LMap

let list_of_lmap    m = List.rev @@ LMap.fold (fun _ v prev -> v :: prev) m []
let kv_list_of_lmap m = List.rev @@ LMap.fold (fun k v prev -> (k, v) :: prev) m []
let list_of_map m = List.rev @@ Map.String.fold (fun _ v prev -> v :: prev) m []
let kv_list_of_map m = List.rev @@ Map.String.fold (fun k v prev -> (k, v) :: prev) m []
let lmap_of_kv_list lst =
  let open LMap in
  List.fold_left (fun prev (k, v) -> add k v prev) empty lst
let map_of_kv_list lst =
  let open Map.String in
  List.fold_left (fun prev (k, v) -> add k v prev) empty lst

let extract_constructor (v : value) (tree : _ Append_tree.t') : (string * value * AST.type_expression , spilling_error) result =
  let open Append_tree in
  let rec aux tv : (string * value * AST.type_expression , spilling_error) result=
    match tv with
    | Leaf (Label k, t), v -> ok (k, v, t)
    | Node {a}, D_left v -> aux (a, v)
    | Node {b}, D_right v -> aux (b, v)
    | _ -> fail @@ corner_case ~loc:__LOC__ "bad constructor path"
  in
  let%bind (s, v, t) = aux (tree, v) in
  ok (s, v, t)