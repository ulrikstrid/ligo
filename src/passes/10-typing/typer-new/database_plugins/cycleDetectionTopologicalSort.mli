open Ast_typed.Types
include Plugin
type 'type_variable inc = < cycle_detection_topological_sort : 'type_variable t >
val get_state_for_tests : 'type_variable t -> unit
