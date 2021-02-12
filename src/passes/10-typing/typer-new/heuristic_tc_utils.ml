[@@@warning "-32"]
module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

(* open Trace
 * open Typer_common.Errors *)
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet

module Utils = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  (* open Type_variable_abstraction *)
  open Type_variable_abstraction.Types
  type type_variable = Type_variable.t

  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  (* open All_plugins *)

  let get_cells (tc : c_typeclass_simpl) =
    List.flatten tc.tc
end
