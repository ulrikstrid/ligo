open Ast_typed.Types

module PluginFields_ (Ppt : PerPluginType) : sig
  (* [@warning "-34"] type z = int *)
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

include Ast_typed.Types.IndexerPlugins
with
    module PluginFields = PluginFields_
(* OCaml/dune hide the contents of a folder unless they are
   re-exported… this is just to be able to access the modules from
   outside. This has nothing to do with the plugin architecture. *)
module All_plugins = All_plugins
