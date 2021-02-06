open Ast_typed.Types
include Plugin
type 'type_variable inc = < typeclasses_constraining : 'type_variable t >
val get_typeclasses_constraining : 'type_variable -> <'type_variable inc;..> -> c_typeclass_simpl PolySet.t
val get_typeclasses_constraining_list : 'type_variable -> <'type_variable inc;..> -> c_typeclass_simpl list

val get_state_for_tests : 'type_variable t -> ('type_variable, c_typeclass_simpl PolySet.t) UnionFind.ReprMap.t
