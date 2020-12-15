open Ast_typed.Types
include Plugin
val find_opt : constraint_identifier -> 'typeVariable t -> refined_typeclass option
val find : c_typeclass_simpl -> 'typeVariable t -> refined_typeclass
(* TODO: this shouldn't be exposed, just a WIP while refactoring. *)
val values : 'typeVariable t -> refined_typeclass list

type 'typeVariable t_for_tests = {
  forwards: (constraint_identifier, refined_typeclass) PolyMap.t ;
  backwards: (constraint_identifier, constraint_identifier) PolyMap.t (* maybe not needed anymore now that c_typeclass_simpl has an "original" field ? *)
}
val get_state_for_tests : 'tv t -> 'tv t_for_tests
