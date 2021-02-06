open Ast_typed.Types

(* data PluginFields (Ppt :: PerPluginType) = PluginFields {
     assignments       :: Ppt Assignments,
     groupedByVariable :: Ppt GroupedByVariable,
     …
   }
*)

(* TODO: this is probably hidden by its signature somewhere? *)
module PluginFields = functor (Ppt : PerPluginType) -> struct
  module type Flds = sig
    val assignments                      : Ppt(Assignments).t
    val grouped_by_variable              : Ppt(GroupedByVariable).t
    val cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t
    val by_constraint_identifier         : Ppt(ByConstraintIdentifier).t
    val typeclasses_constraining         : Ppt(TypeclassesConstraining).t
  end

  module Assignments = Assignments
  module type AssignmentsFlds = sig val assignments : Ppt(Assignments).t end
  let assignments (flds : (module Flds)) : (module AssignmentsFlds) =
    let module Flds = (val flds) in
    (module struct let assignments = Flds.assignments end)
end

(* TODO: try removing this _ workaround *)
module PluginFields_ = PluginFields

(* mapPlugins :: (F : MappedFunction) → (PluginFields F.MakeIn) → (PluginFields F.MakeOut) *)
module MapPlugins = functor (F : MappedFunction) -> struct
  let f :
    F.extra_args ->
    (module PluginFields(F.MakeInType).Flds) ->
    (module PluginFields(F.MakeOutType).Flds) F.Monad.t
    = fun extra_args fieldsIn ->
      let module Let_syntax = F.Monad in
      let module FieldsIn = (val fieldsIn) in
      let%bind assignments                      = (let module F = F.F(Assignments)                   in F.f "assign" extra_args FieldsIn.assignments)                      in
      let%bind grouped_by_variable              = (let module F = F.F(GroupedByVariable)             in F.f "g by v" extra_args FieldsIn.grouped_by_variable)              in
      let%bind cycle_detection_topological_sort = (let module F = F.F(CycleDetectionTopologicalSort) in F.f "c topo" extra_args FieldsIn.cycle_detection_topological_sort) in
      let%bind by_constraint_identifier         = (let module F = F.F(ByConstraintIdentifier)        in F.f "by  id" extra_args FieldsIn.by_constraint_identifier)         in
      let%bind typeclasses_constraining         = (let module F = F.F(TypeclassesConstraining)       in F.f "tc con" extra_args FieldsIn.typeclasses_constraining) ;       in
      F.Monad.return (module struct
        let assignments                      = assignments
        let grouped_by_variable              = grouped_by_variable
        let cycle_detection_topological_sort = cycle_detection_topological_sort
        let by_constraint_identifier         = by_constraint_identifier
        let typeclasses_constraining         = typeclasses_constraining
      end : PluginFields(F.MakeOutType).Flds)
end

(* A value containing an empty (dummy) unit associated each plugin
   name This allows us to use `map' to discard this `unit' and
   e.g. initialize each plugin. *)
type plugin_units = (module PluginFields(PerPluginUnit).Flds)
let plugin_fields_unit : plugin_units = (module struct
  let assignments                      = ()
  let grouped_by_variable              = ()
  let cycle_detection_topological_sort = ()
  let by_constraint_identifier         = ()
  let typeclasses_constraining         = ()
end)

module All_plugins = All_plugins
