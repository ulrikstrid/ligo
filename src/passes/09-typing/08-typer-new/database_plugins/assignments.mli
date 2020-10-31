open Ast_typed.Types
include Plugin with type 'typeVariable t = ('typeVariable, c_constructor_simpl) UnionFind.ReprMap.t
val find_opt : 'type_variable -> 'type_variable t -> c_constructor_simpl option
