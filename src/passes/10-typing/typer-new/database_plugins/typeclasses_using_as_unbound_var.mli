open Solver_types

module M : functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
sig
  open Type_variable_abstraction.Types
  include INDEXER_PLUGIN_TYPE(Type_variable)(Type_variable_abstraction).S
  type ('type_variable, 'a) state = < typeclasses_using_as_unbound_var : 'type_variable t ; .. > as 'a
  val get_typeclasses_using_as_unbound_var : 'type_variable -> 'type_variable t -> c_typeclass_simpl MultiSet.t
  val get_typeclasses_using_as_unbound_var_list : 'type_variable -> 'type_variable t -> c_typeclass_simpl list

  val get_state_for_tests : 'type_variable t -> ('type_variable, c_typeclass_simpl MultiSet.t) UnionFind.ReprMap.t
end
