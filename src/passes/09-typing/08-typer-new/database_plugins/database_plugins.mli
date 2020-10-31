open Ast_typed.Types

module PluginFields_ (Ppt : PerPluginType) : sig
  (* [@warning "-34"] type z = int *)
  type flds = <
    assignments                      : Ppt(Assignments).t ;
    grouped_by_variable              : Ppt(GroupedByVariable).t ;
    cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t ;
    by_constraint_identifier         : Ppt(ByConstraintIdentifier).t ;
    refined_typeclasses              : Ppt(RefinedTypeclasses).t ;
    refined_typeclasses_back         : Ppt(RefinedTypeclassesBack).t ;
    typeclasses_constraining         : Ppt(TypeclassesConstraining).t ;
  >
end

include Ast_typed.Types.IndexerPlugins
  (* TODO: do we need this & the definition above? *)
  with module PluginFields = PluginFields_


(* OCaml/dune hide the contents of a folder unless they are
   re-exported… this is just to be able to access the modules from
   outside. This has nothing to do with the plugin architecture. *)
module All_plugins = All_plugins
