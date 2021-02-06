open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

module Required_flds : sig
  module type S = sig
    val grouped_by_variable : type_variable GroupedByVariable.t
  end
end

(* include Typesystem.Solver_types.Heuristic_plugin with
 *   module Required_flds = Required_flds *)

val heuristic_name : string
(* Finds in the databases tuples of constraints which are
   interesting for this plugin and include the given
   type_constraint_simpl. *)
module M : functor (Available_flds : Required_flds.S) -> sig
  type selector_output
  val selector     : (* (selector_output, (module Required_flds)) selector *)
    (type_variable -> type_variable) -> type_constraint_simpl -> (*flds ->*) selector_output list
  (* Select in the databases tuples of constraints which are
     interesting and involve the given two type_viables, knowing that
     these are about to be merged. This function is called before the
     database's merge_aliases functions are called (i.e. the database
     does not reflect the effects of the merge yet). *)
  val alias_selector : type_variable -> type_variable -> (*(module Required_flds) ->*) selector_output list
  val get_referenced_constraints : selector_output -> type_constraint_simpl list
  (* called when two 'data are associated with the same type_constraint *)
  val propagator   : (selector_output , Ast_typed.Typer_errors.typer_error) propagator
  val printer      : Format.formatter -> selector_output -> unit
  val printer_json : selector_output -> Yojson.Safe.t
  val comparator   : selector_output -> selector_output -> int
end

(* include Typesystem.Solver_types.Heuristic_plugin *)

(* open Database_plugins.All_plugins
 * open Ast_typed.Types
 * open Typesystem.Solver_types
 * 
 * val heuristic : <
 *   grouped_by_variable : type_variable GroupedByVariable.t ;
 * 
 *   assignments              : type_variable Assignments.t ;
 *   ..
 * > ex_heuristic_plugin *)

