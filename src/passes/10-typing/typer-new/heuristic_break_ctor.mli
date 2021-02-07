open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

val heuristic : <
  grouped_by_variable : type_variable GroupedByVariable.t ;
  ..
> ex_heuristic_plugin

val selector_ : ( type_variable -> type_variable ) -> type_constraint_simpl -> type_variable GroupedByVariable.t -> output_break_ctor list

module type Indexes = sig
  val grouped_by_variable : type_variable GroupedByVariable.t
end

module H : sig
  open Typer_common.Errors
  type selector_output
  val heuristic_name : string
  val selector : (type_variable -> type_variable) -> type_constraint_simpl ->
    (module Indexes)
    -> selector_output list
  val alias_selector : type_variable -> type_variable ->
    (module Indexes)
    -> selector_output list
  val get_referenced_constraints : selector_output -> type_constraint_simpl list
  val propagator : (selector_output, typer_error) propagator
  val printer : Format.formatter -> output_break_ctor -> unit
  val printer_json : selector_output -> Yojson.Safe.t
  val comparator : selector_output -> selector_output -> int
end
