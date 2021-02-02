open Typesystem.Solver_types
open Ast_typed.Types

module Indexers = Database_plugins

(* module Available_flds = struct
 *   module type S = sig
 *     open Database_plugins.All_plugins
 *     val grouped_by_variable : type_variable GroupedByVariable.t
 *     val assignments         : type_variable Assignments.t (\* fake, not needed *\)
 *   end
 * end *)


module Available_flds = struct
  module type S = sig
    open Database_plugins.All_plugins
    val grouped_by_variable : type_variable GroupedByVariable.t
    val assignments         : type_variable Assignments.t (* fake, not needed *)
  end
end

(* module type HP = Heuristic_plugin with module Required_flds = Available_flds *)

module type HP = functor (Available_flds : Available_flds.S) -> sig
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


let heuristics : (module HP) list = [
  (module Heuristic_specialize1.M) ;
  (module Heuristic_break_ctor.M) ;
  
  (* (module Heuristic_access_label) ;
   * (module Heuristic_specialize1) ;
   * (module Heuristic_tc_fundep) ; *)
]

(* let heuristics : Indexers.PluginFields(PerPluginState).flds heuristic_plugins = [
 *   Heuristic_break_ctor.heuristic ;
 *   Heuristic_access_label.heuristic ;
 *   Heuristic_specialize1.heuristic ;
 *   Heuristic_tc_fundep.heuristic ;
 * ] *)

let f (flds : (module Available_flds.S)) =
  let module Flds = (val flds) in
  match heuristics with
    (a::b::[]) ->
    let module Flds = (val flds) in
    let module A = (val a) in
    let module AA = A(Flds) in
    let module B = (val b) in
    let module BB = B(Flds) in
    let _ = AA.selector (fun x -> x) (??) in
    let _ = BB.selector (fun x -> x) (??) in
    ??
  | _ -> ??
