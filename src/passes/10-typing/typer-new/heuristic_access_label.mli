open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

val heuristic_name : string

module Required_flds : sig
  module type S = sig
    val grouped_by_variable : type_variable GroupedByVariable.t
  end
end

(* type selector_output = {
 *   a_k_var : c_row_simpl ;
 *   a_var_l : c_access_label_simpl ;
 * } *)

module M : Heuristic_plugin_M(Required_flds).S
