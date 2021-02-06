open Ast_typed.Types

module PluginFields_ (Ppt : PerPluginType) : sig
  module type Flds = sig
    val assignments                      : Ppt(Assignments).t
    val grouped_by_variable              : Ppt(GroupedByVariable).t
    val cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t
    val by_constraint_identifier         : Ppt(ByConstraintIdentifier).t
    val typeclasses_constraining         : Ppt(TypeclassesConstraining).t
  end

  module Assignments : sig
    type 'typeVariable t
    val find_opt : 'type_variable -> 'type_variable t -> constructor_or_row option
    val bindings : 'type_variable t -> ('type_variable * constructor_or_row) list
    val pp : (Format.formatter -> 'typeVariable -> unit) -> Format.formatter -> 'typeVariable t -> unit
  end
  module type AssignmentsFlds = sig val assignments : Ppt(Assignments).t end
  val assignments : (module Flds) -> (module AssignmentsFlds)
end

include Ast_typed.Types.IndexerPlugins
  (* TODO: do we need this & the definition above? *)
  with module PluginFields = PluginFields_


(* OCaml/dune hide the contents of a folder unless they are
   re-exported… this is just to be able to access the modules from
   outside. This has nothing to do with the plugin architecture. *)
module All_plugins = All_plugins
