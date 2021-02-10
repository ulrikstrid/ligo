open Ast_typed.Types
include Solver_types.INDEXER_PLUGIN
type ('type_variable, 'a) state = < typeclasses_constraining : 'type_variable t ; .. > as 'a
val get_typeclasses_constraining : 'type_variable -> 'type_variable t -> c_typeclass_simpl PolySet.t
val get_typeclasses_constraining_list : 'type_variable -> 'type_variable t -> c_typeclass_simpl list

val get_state_for_tests : 'type_variable t -> ('type_variable, c_typeclass_simpl PolySet.t) UnionFind.ReprMap.t
