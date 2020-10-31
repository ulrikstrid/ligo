open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

val heuristic : <
    assignments              : type_variable Assignments.t ;
    grouped_by_variable      : type_variable GroupedByVariable.t ;
    refined_typeclasses      : type_variable RefinedTypeclasses.t ;
    refined_typeclasses_back : type_variable RefinedTypeclassesBack.t ;
    typeclasses_constraining : type_variable TypeclassesConstraining.t ;
  ..
> ex_heuristic_plugin
