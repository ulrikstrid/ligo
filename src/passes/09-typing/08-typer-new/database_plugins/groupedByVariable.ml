open Ast_typed.Types
open UnionFind

(* map from (unionfind) variables to constraints containing them *)
type 'typeVariable t = ('typeVariable, constraints) ReprMap.t
let create_state ~cmp =
  let merge cs1 cs2 = let _ = failwith "assert (Compare.constraints cs1 cs2 = 0);" in ignore cs2; cs1 in
  ReprMap.create ~cmp ~merge
let add_constraint _ = failwith "todo"
let remove_constraint _ = failwith "todo"
let merge_aliases =
  fun updater state -> updater.map state

let get_constraints_related_to : 'type_variable -> 'type_variable t -> constraints =
  fun variable state ->
  (* get the class of the variable *)
  match ReprMap.find_opt variable state with
    Some l -> l
  | None -> {
      constructor = [] ;
      poly        = [] ;
      tc          = [] ;
      row         = [] ;
    }
