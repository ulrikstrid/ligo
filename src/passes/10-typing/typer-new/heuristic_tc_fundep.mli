open Trace
open Typer_common.Errors
open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

val heuristic : <
    type_variable Assignments.inc ;
    type_variable GroupedByVariable.inc ;
    type_variable TypeclassesConstraining.inc ;
    type_variable ByConstraintIdentifier.inc ;
  ..
> ex_heuristic_plugin

val restrict : (type_variable -> type_variable) -> constructor_or_row -> c_typeclass_simpl -> c_typeclass_simpl
val deduce_and_clean : (type_variable -> type_variable) -> c_typeclass_simpl -> (deduce_and_clean_result, typer_error) result
