open Cli_expect

let contract basename =
  "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "cycle_A.mligo" ; "main" ] ;
  [%expect {|
    Dependency cycle detected, please resolve |}]

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "D.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/D.mligo
        |-- ../../test/contracts/build/C.mligo
        |   |-- ../../test/contracts/build/A.mligo
        |   `-- ../../test/contracts/build/B.mligo
        |       `-- ../../test/contracts/build/A.mligo
        `-- ../../test/contracts/build/E.mligo
            |-- ../../test/contracts/build/F.mligo
            `-- ../../test/contracts/build/G.mligo |}]
