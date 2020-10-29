open Typesystem.Solver_types

open Ast_typed.Types
open Database_plugins.All_plugins
module Plugin_fields = functor (Ppt : PerPluginType) -> struct
  module type S = sig
    val grouped_by_variable : Ppt(GroupedByVariable).t
  end
end

module Plugin_fields2 = functor (Ppt : PerPluginType) -> struct
  module type S = sig
    val grouped_by_variable : Ppt(GroupedByVariable).t
    val assignments : Ppt(Assignments).t
  end
end

module H1: Heuristic_plugin(Plugin_fields).S = Heuristic_break_ctor
module H2: Heuristic_plugin(Plugin_fields2).S = Heuristic_break_ctor

module P : Plugins = struct
  module Indexers = Database_plugins
  module PF = Database_plugins.PluginFields
  module type S = Heuristic_plugin(Indexers.PluginFields).S
  module Heuristics = struct
    let heuristics : (module Heuristic_plugin(Indexers.PluginFields).S) list = [
      (module Heuristic_break_ctor) ;
      (module Heuristic_specialize1) ;
      (module Heuristic_tc_fundep) ;
    ]
  end
end
