open Ast_typed.Types

(* data PluginFields (Ppt :: PerPluginType) = PluginFields {
     assignments       :: Ppt Assignments,
     groupedByVariable :: Ppt GroupedByVariable,
     …
   }
*)

(* TODO: this is probably hidden by its signature somewhere? *)
module PluginFields = functor (Ppt : PerPluginType) -> struct
  type flds = <
    assignments                      : Ppt(Assignments).t ;
    grouped_by_variable              : Ppt(GroupedByVariable).t ;
    cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t ;
    by_constraint_identifier         : Ppt(ByConstraintIdentifier).t ;
    refined_typeclasses              : Ppt(RefinedTypeclasses).t ;
    typeclasses_constraining         : Ppt(TypeclassesConstraining).t ;
  >

  module Assignments = Assignments
  let assignments flds = (flds :> <assignments:_>)
  let pp_print ppf (flds : flds) =
    let module A = Ppt(Assignments) in
    let module GbV = Ppt(GroupedByVariable) in
    let module CDTS = Ppt(CycleDetectionTopologicalSort) in
    let module BCI = Ppt(ByConstraintIdentifier) in
    let module RTC = Ppt(RefinedTypeclasses) in
    let module TcC = Ppt(TypeclassesConstraining) in
    Format.fprintf ppf "@[ <@ assignments =@ @[<hv 2> %a @] ;@ grouped_by_variable =@ @[<hv 2> %a@] ;@ cycle_detection_topological_sort =@ @[<hv 2> %a@] ;@ by_constraint_identifier =@ @[<hv 2> %a@] ;@ refined_typeclasses =@ @[<hv 2> %a@] ;@ typeclasses_constraining =@ @[<hv 2> %a@] ;@]@ >"
      A.pp flds#assignments
      GbV.pp flds#grouped_by_variable
      CDTS.pp flds#cycle_detection_topological_sort
      BCI.pp flds#by_constraint_identifier
      RTC.pp flds#refined_typeclasses
      TcC.pp flds#typeclasses_constraining

end
(* TODO: try removing this _ workaround *)
module PluginFields_ = PluginFields

(* mapPlugins :: (F : MappedFunction) → (PluginFields F.MakeIn) → (PluginFields F.MakeOut) *)
module MapPlugins = functor (F : MappedFunction) -> struct
  let f :
    F.extra_args ->
    PluginFields(F.MakeInType).flds ->
    PluginFields(F.MakeOutType).flds F.Monad.t
    = fun extra_args fieldsIn ->
      let module Let_syntax = F.Monad in
      let%bind assignments                      = (let module F = F.F(Assignments)                   in F.f "assign" extra_args fieldsIn#assignments)                      in
      let%bind grouped_by_variable              = (let module F = F.F(GroupedByVariable)             in F.f "g by v" extra_args fieldsIn#grouped_by_variable)              in
      let%bind cycle_detection_topological_sort = (let module F = F.F(CycleDetectionTopologicalSort) in F.f "c topo" extra_args fieldsIn#cycle_detection_topological_sort) in
      let%bind by_constraint_identifier         = (let module F = F.F(ByConstraintIdentifier)        in F.f "by  id" extra_args fieldsIn#by_constraint_identifier)         in
      let%bind refined_typeclasses              = (let module F = F.F(RefinedTypeclasses)            in F.f "ref tc" extra_args fieldsIn#refined_typeclasses)              in
      let%bind typeclasses_constraining         = (let module F = F.F(TypeclassesConstraining)       in F.f "tc con" extra_args fieldsIn#typeclasses_constraining) ;       in
      F.Monad.return (object
        method assignments                      = assignments
        method grouped_by_variable              = grouped_by_variable
        method cycle_detection_topological_sort = cycle_detection_topological_sort
        method by_constraint_identifier         = by_constraint_identifier
        method refined_typeclasses              = refined_typeclasses
        method typeclasses_constraining         = typeclasses_constraining
      end)
end

(* A value containing an empty (dummy) unit associated each plugin
   name This allows us to use `map' to discard this `unit' and
   e.g. initialize each plugin. *)
type plugin_units = PluginFields(PerPluginUnit).flds
let plugin_fields_unit : plugin_units = object
  method assignments                      = () ;
  method grouped_by_variable              = () ;
  method cycle_detection_topological_sort = () ;
  method by_constraint_identifier         = () ;
  method refined_typeclasses              = () ;
  method typeclasses_constraining         = () ;
end

module All_plugins = All_plugins
