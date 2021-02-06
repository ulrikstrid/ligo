open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

type selector_output = {
  a_k_var : c_row_simpl ;
  a_var_l : c_access_label_simpl ;
}

val heuristic : <
  type_variable GroupedByVariable.inc ;
  ..
> ex_heuristic_plugin
