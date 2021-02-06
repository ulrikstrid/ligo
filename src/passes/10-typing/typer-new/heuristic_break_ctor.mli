open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

(* Finds in the databases tuples of constraints which are
   interesting for this plugin and include the given
   type_constraint_simpl. *)

val heuristic_name : string

module Required_flds : sig
  module type S = sig
    val grouped_by_variable : type_variable GroupedByVariable.t
  end
end

module M : Heuristic_plugin_M(Required_flds).S

(* val selector_ : ( type_variable -> type_variable ) -> type_constraint_simpl -> type_variable GroupedByVariable.t -> output_break_ctor list *)

