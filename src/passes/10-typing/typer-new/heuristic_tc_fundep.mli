open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

val heuristic_name : string

module Required_flds : sig
  module type S = sig
    val assignments              : type_variable Assignments.t
    val grouped_by_variable      : type_variable GroupedByVariable.t
    val typeclasses_constraining : type_variable TypeclassesConstraining.t
    val by_constraint_identifier : type_variable ByConstraintIdentifier.t
  end
end

module M : Heuristic_plugin_M(Required_flds).S

(* open Typer_common.Errors
 * open Trace
 * val restrict : (type_variable -> type_variable) -> constructor_or_row -> c_typeclass_simpl -> c_typeclass_simpl
 * val deduce_and_clean : (type_variable -> type_variable) -> c_typeclass_simpl -> (deduce_and_clean_result, typer_error) result *)
