open Ast_typed.Types
include Solver_types.INDEXER_PLUGIN
val find_opt : constraint_identifier -> 'type_variable t -> c_typeclass_simpl option
val get_state_for_tests : _ t -> (constraint_identifier, c_typeclass_simpl) PolyMap.t
