open Ast_typed.Types

(* data PluginFields (Ppt :: PerPluginType) = PluginFields {
     assignments       :: Ppt Assignments,
     groupedByVariable :: Ppt GroupedByVariable,
     …
   }
*)
module PluginFields (Ppt : PerPluginType) = struct
  module type S = sig
    val assignments                      : Ppt(Assignments).t
    val grouped_by_variable              : Ppt(GroupedByVariable).t
    val cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t
    val by_constraint_identifier         : Ppt(ByConstraintIdentifier).t
    val refined_typeclasses              : Ppt(RefinedTypeclasses).t
    val refined_typeclasses_back         : Ppt(RefinedTypeclassesBack).t
    val typeclasses_constrained_by       : Ppt(TypeclassesConstrainedBy).t
  end
end

(* mapPlugins :: (F : MappedFunction) → (PluginFields F.MakeIn) → (PluginFields F.MakeOut) *)
module MapPlugins = functor (F : MappedFunction) -> struct
  let f :
    F.extra_args ->
    (module PluginFields(F.MakeInType).S) ->
    (module PluginFields(F.MakeOutType).S)
    = fun extra_args fieldsIn ->
      let module FieldsIn = (val fieldsIn) in
      (module struct
        let assignments = (let module F = F.F(Assignments) in F.f extra_args FieldsIn.assignments)
        let grouped_by_variable = (let module F = F.F(GroupedByVariable) in F.f extra_args FieldsIn.grouped_by_variable)
        let cycle_detection_topological_sort = (let module F = F.F(CycleDetectionTopologicalSort) in F.f extra_args FieldsIn.cycle_detection_topological_sort)
        let by_constraint_identifier = (let module F = F.F(ByConstraintIdentifier) in F.f extra_args FieldsIn.by_constraint_identifier)
        let refined_typeclasses = (let module F = F.F(RefinedTypeclasses) in F.f extra_args FieldsIn.refined_typeclasses)
        let refined_typeclasses_back = (let module F = F.F(RefinedTypeclassesBack) in F.f extra_args FieldsIn.refined_typeclasses_back)
        let typeclasses_constrained_by = (let module F = F.F(TypeclassesConstrainedBy) in F.f extra_args FieldsIn.typeclasses_constrained_by)
      end)
end

(* A value containing an empty (dummy) unit associated each plugin
   name This allows us to use `map' to discard this `unit' and
   e.g. initialize each plugin. *)
module type PluginUnits = PluginFields(PerPluginUnit).S
module PluginFieldsUnit : PluginUnits = struct
  let assignments                      = ()
  let grouped_by_variable              = ()
  let cycle_detection_topological_sort = ()
  let by_constraint_identifier         = ()
  let refined_typeclasses              = ()
  let refined_typeclasses_back         = ()
  let typeclasses_constrained_by       = ()
end
