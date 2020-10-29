open Database_plugins.All_plugins
open Ast_typed.Types

module Plugin_fields : functor (Ppt : PerPluginType) -> sig
  (* type z = int *)
  module type S = sig
    val grouped_by_variable : Ppt(GroupedByVariable).t
  end
end
include Typesystem.Solver_types.Heuristic_plugin(Plugin_fields).S
