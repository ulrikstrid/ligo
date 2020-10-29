module Plugin_fields : functor (Ppt : Ast_typed.PerPluginType) -> sig
  module type S = sig
    val assignments                : Ppt(Database_plugins.All_plugins.Assignments).t
    val grouped_by_variable        : Ppt(Database_plugins.All_plugins.GroupedByVariable).t
    val refined_typeclasses        : Ppt(Database_plugins.All_plugins.RefinedTypeclasses).t
    val refined_typeclasses_back   : Ppt(Database_plugins.All_plugins.RefinedTypeclassesBack).t
    val typeclasses_constrained_by : Ppt(Database_plugins.All_plugins.TypeclassesConstrainedBy).t
  end
end

include Typesystem.Solver_types.Heuristic_plugin(Plugin_fields).S
