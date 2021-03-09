open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

(* avoid pretty printing *)
let () = Unix.putenv "TERM" "dumb"

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "double_michelson_or.mligo" ; "main" ; "unit" ; "(M_left (1) : storage)" ] ;
  [%expect {|
    Warning: unused variable bar at in file "../../test/contracts/double_michelson_or.mligo", line 8, characters 6-9
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    Warning: unused variable bar at in file "../../test/contracts/double_michelson_or.mligo", line 8, characters 6-9
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    Warning: unused variable main at in file "../../test/contracts/double_michelson_or.mligo", line 6, characters 4-8
    ( LIST_EMPTY() , M_right("one") ) |}];

  run_ligo_good [ "dry-run" ; contract "double_michelson_or.ligo" ; "main" ; "unit" ; "(M_left (1) : storage)" ] ;
  [%expect {|
    Warning: unused variable bar at in file "../../test/contracts/double_michelson_or.ligo", line 9, characters 8-11
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    Warning: unused variable bar at in file "../../test/contracts/double_michelson_or.ligo", line 9, characters 8-11
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    Warning: unused variable main at in file "../../test/contracts/double_michelson_or.ligo", line 6, characters 9-13
    ( LIST_EMPTY() , M_right("one") ) |}]


let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_or_tree.mligo" ; "main" ] ;
  [%expect {|
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    { parameter unit ;
      storage (or (int %three) (or %four (int %one) (nat %two))) ;
      code { DROP ; PUSH int 1 ; LEFT nat ; RIGHT int ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_michelson_or.mligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/bad_michelson_or.mligo", line 6, characters 12-27
      5 | let main (action, store : unit * storage) : return =
      6 |   let foo = M_right ("one") in
      7 |   (([] : operation list), (foo: storage))

    Incorrect usage of type "michelson_or".
    The contructor "M_right" must be annotated with a variant type. |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_or_tree_intermediary.ligo" ; "main" ] ;
  [%expect {|
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    { parameter unit ;
      storage (or (int %three) (or (int %one) (nat %two))) ;
      code { DROP ; PUSH int 1 ; LEFT nat ; RIGHT int ; NIL operation ; PAIR } } |}]

