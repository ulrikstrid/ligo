open Cli_expect

let contract basename =
  "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "cycle_A.mligo" ; "main" ] ;
  [%expect {|
    Node : ../../test/contracts/build/cycle_A.mligo
    Node : ../../test/contracts/build/cycle_B.mligo
    Node : ../../test/contracts/build/cycle_C.mligo
    Dependency cycle detected, please resolve |}]
