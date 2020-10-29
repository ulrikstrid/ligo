module Plugin_fields : functor (Ppt : Ast_typed.PerPluginType) -> sig
  (* type z = int *)
  module type S = sig
    val grouped_by_variable : Ppt(Database_plugins.All_plugins.GroupedByVariable).t
  end
end
include Typesystem.Solver_types.Heuristic_plugin(Plugin_fields).S
