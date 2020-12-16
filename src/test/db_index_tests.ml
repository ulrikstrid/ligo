open Test_helpers
open Db_index_assignment_tests
open Db_index_grouped_by_variable_tests
open Db_index_cycle_detection_topological_sort_tests
open Db_index_by_constraint_identifier_tests
open Db_index_refined_typeclasses_tests
open Db_index_typeclasses_constraining_tests

let main =
  test_suite "Indexers" @@
    [
      test "assignments" assignments ;
      test "grouped by variable" grouped_by_variable ;
      test "cycle detection topological sort" cycle_detection_topological_sort ;
      test "by constraint identifier" by_constraint_identifier ;
      test "refined typeclasses" refined_typeclasses ;
      test "typeclasses constraining" typeclasses_constraining ;
      test "invariant" invariant ; (* TODO : do this for all this plugins (?) *)
    ]
