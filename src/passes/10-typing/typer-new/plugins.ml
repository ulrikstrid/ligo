open Typesystem.Solver_types
open Ast_typed.Types

module Indexers = Database_plugins

module Available_flds = struct
  module type S = Indexers.PluginFields(PerPluginState).Flds
  (* =
   * val assignments                      : type_variable Assignments.t
   * val grouped_by_variable              : type_variable GroupedByVariable.t
   * val cycle_detection_topological_sort : type_variable CycleDetectionTopologicalSort.t
   * val by_constraint_identifier         : type_variable ByConstraintIdentifier.t
   * val typeclasses_constraining         : type_variable TypeclassesConstraining.t *)
end

let heuristics : (module Heuristic_plugin_M(Available_flds).S) list = [
  (module Heuristic_break_ctor.M) ;
  (module Heuristic_access_label.M) ;
  (module Heuristic_specialize1.M) ;  
  (module Heuristic_tc_fundep.M) ;
]

let _f (flds : (module Available_flds.S)) =
  let module Flds = (val flds) in
  match heuristics with
    (a::b::[]) ->
    let module Flds = (val flds) in
    let module A = (val a) in
    let module AA = A(Flds) in
    let module B = (val b) in
    let module BB = B(Flds) in
    let out = AA.selector (fun x -> x) (failwith "just a test") in
    let _ = List.map (fun o -> AA.propagator o (fun x -> x)) out in
    let _ = BB.selector (fun x -> x) (failwith "just a test") in
    ()
  | _ -> failwith "just a test"
