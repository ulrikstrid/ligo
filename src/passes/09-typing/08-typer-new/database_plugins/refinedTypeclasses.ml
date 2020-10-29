open Ast_typed.Types

type 'typeVariable t = (constraint_identifier, refined_typeclass) PolyMap.t
let create_state ~cmp:_ =
  PolyMap.create ~cmp:Ast_typed.Compare.constraint_identifier
let add_constraint _ = failwith "todo"
let remove_constraint _ = failwith "todo"
let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun _merge_keys state -> state

let find_opt : constraint_identifier -> 'typeVariable t -> refined_typeclass option = PolyMap.find_opt
let values : 'typeVariable t -> refined_typeclass list = fun m -> List.map snd @@ PolyMap.bindings m
