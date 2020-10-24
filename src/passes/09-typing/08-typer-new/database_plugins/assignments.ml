open Ast_typed.Types
open UnionFind

(* Haskell doesn't have easy-to-use type-level functions or types as
   fields of records, so we're bending its syntax here.

   type "Assignments.t" typeVariable = map typeVariable
   c_constructor_simpl data Assignments :: Plugin "Assignments.t" *)

type 'typeVariable t = ('typeVariable, c_constructor_simpl) ReprMap.t
let create_state ~cmp =
  let merge c1 c2 = assert (Ast_typed.Compare.c_constructor_simpl c1 c2 = 0); c1 in
  ReprMap.create ~cmp ~merge
let add_constraint _ = failwith "todo"
let remove_constraint _ = failwith "todo"
let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun merge_keys state -> merge_keys.map state
