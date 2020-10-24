open Ast_typed.Types
open UnionFind

type 'typeVariable t = ('typeVariable, constraint_identifier_set) ReprMap.t
let create_state ~cmp =
  let merge c1 c2 = assert (Ast_typed.Compare.c_constructor_simpl c1 c2 = 0); c1 in
  ReprMap.create ~cmp ~merge
let add_constraint _ = failwith "todo"
let remove_constraint _ = failwith "todo"
let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun merge_keys state -> merge_keys.map state

