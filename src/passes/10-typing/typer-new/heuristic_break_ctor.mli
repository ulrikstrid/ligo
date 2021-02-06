open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

val heuristic : <
  type_variable GroupedByVariable.inc ;
> ex_heuristic_plugin

val selector_ : ( type_variable -> type_variable ) -> type_constraint_simpl -> type_variable GroupedByVariable.t -> output_break_ctor list
