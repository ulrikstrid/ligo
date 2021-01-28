open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

type selector_output = {
  a_k_var : constructor_or_row ;
  a_var_l : c_access_label_simpl ;
}

val heuristic : <
  grouped_by_variable : type_variable GroupedByVariable.t ;
  ..
> ex_heuristic_plugin

val selector_ : type_constraint_simpl -> type_variable GroupedByVariable.t -> output_break_ctor list
