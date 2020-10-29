open Ast_typed.Types

(* data PluginFields (Ppt :: PerPluginType) = PluginFields {
     assignments       :: Ppt Assignments,
     groupedByVariable :: Ppt GroupedByVariable,
     …
   }
*)

(* TODO: this is probably hidden by its signature somewhere? *)
module PluginFields = functor (Ppt : PerPluginType) -> struct
  (* [@warning "-34"] type z = int *)
  type flds = <
    assignments                      : Ppt(Assignments).t ;
    grouped_by_variable              : Ppt(GroupedByVariable).t ;
    cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t ;
    by_constraint_identifier         : Ppt(ByConstraintIdentifier).t ;
    refined_typeclasses              : Ppt(RefinedTypeclasses).t ;
    refined_typeclasses_back         : Ppt(RefinedTypeclassesBack).t ;
    typeclasses_constrained_by       : Ppt(TypeclassesConstrainedBy).t ;
  >
end
module PluginFields_ = PluginFields

(* mapPlugins :: (F : MappedFunction) → (PluginFields F.MakeIn) → (PluginFields F.MakeOut) *)
module MapPlugins = functor (F : MappedFunction) -> struct
  let f :
    F.extra_args ->
    PluginFields(F.MakeInType).flds ->
    PluginFields(F.MakeOutType).flds
    = fun extra_args fieldsIn ->
      object
        method assignments                      = (let module F = F.F(Assignments)                   in F.f extra_args fieldsIn#assignments)
        method grouped_by_variable              = (let module F = F.F(GroupedByVariable)             in F.f extra_args fieldsIn#grouped_by_variable)
        method cycle_detection_topological_sort = (let module F = F.F(CycleDetectionTopologicalSort) in F.f extra_args fieldsIn#cycle_detection_topological_sort)
        method by_constraint_identifier         = (let module F = F.F(ByConstraintIdentifier)        in F.f extra_args fieldsIn#by_constraint_identifier)
        method refined_typeclasses              = (let module F = F.F(RefinedTypeclasses)            in F.f extra_args fieldsIn#refined_typeclasses)
        method refined_typeclasses_back         = (let module F = F.F(RefinedTypeclassesBack)        in F.f extra_args fieldsIn#refined_typeclasses_back) ;
        method typeclasses_constrained_by       = (let module F = F.F(TypeclassesConstrainedBy)      in F.f extra_args fieldsIn#typeclasses_constrained_by) ;
      end
end

(* A value containing an empty (dummy) unit associated each plugin
   name This allows us to use `map' to discard this `unit' and
   e.g. initialize each plugin. *)
type pluginUnits = PluginFields(PerPluginUnit).flds
let pluginFieldsUnit : pluginUnits = object
  method assignments                      = () ;
  method grouped_by_variable              = () ;
  method cycle_detection_topological_sort = () ;
  method by_constraint_identifier         = () ;
  method refined_typeclasses              = () ;
  method refined_typeclasses_back         = () ;
  method typeclasses_constrained_by       = () ;
end

module All_plugins = All_plugins
