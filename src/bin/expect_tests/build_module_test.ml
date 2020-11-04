open Cli_expect

let contract basename =
  "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "B.mligo" ; "f" ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { PUSH int 1 ; PUSH int 1 ; DIG 2 ; CDR ; ADD ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-typed" ; contract "D.mligo" ] ;
  [%expect {|
    const toto = E.toto
    const main = lambda (#3) return let s = #3.1 in let p = #3.0 in let s = ADD(ADD(p ,
    s) ,
    toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "D.mligo"; "main" ] ;
  [%expect {|
    { parameter int ;
      storage int ;
      code { PUSH int 10 ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CDR ;
             DIG 2 ;
             CAR ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    Dependency cycle detected :
     `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

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

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "cycle_A.mligo"; "--display-format=json" ] ;
  [%expect {|
    {"root":"../../test/contracts/build/cycle_A.mligo","child":{"file":"../../test/contracts/build/cycle_B.mligo","child":{"file":"../../test/contracts/build/cycle_C.mligo","child":{"file":"../../test/contracts/build/cycle_A.mligo","child":{"file":"../../test/contracts/build/cycle_B.mligo"}}}}} |}]

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "D.mligo"; "--display-format=json" ] ;
  [%expect {|
    {"root":"../../test/contracts/build/D.mligo","child":{"file":"../../test/contracts/build/C.mligo","child":{"file":"../../test/contracts/build/A.mligo"},"child":{"file":"../../test/contracts/build/B.mligo","child":{"file":"../../test/contracts/build/A.mligo"}}},"child":{"file":"../../test/contracts/build/E.mligo","child":{"file":"../../test/contracts/build/F.mligo"},"child":{"file":"../../test/contracts/build/G.mligo"}}} |}]
