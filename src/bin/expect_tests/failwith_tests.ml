open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "run-function" ; contract "failwith.ligo" ; "failer" ; "1" ] ;
  [%expect {|
    failwith(42) |}];

  run_ligo_good [ "run-function" ; contract "failwith.ligo" ; "failer" ; "1" ; "--format=json" ] ;
  [%expect {|
    { "value": null, "failure": "failwith(42)" } |}];


  run_ligo_good [ "dry-run" ; contract "subtle_nontail_fail.mligo" ; "main" ; "()" ; "()" ] ;
  [%expect {|
    File "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 9-27:
    Warning: unused variable "ps".

    failwith("This contract always fails") |}];

  run_ligo_good [ "interpret" ; "assert(1=1)" ; "--syntax=pascaligo" ] ;
  [%expect {|
    unit |}];

  run_ligo_good [ "interpret" ; "assert(1=2)" ; "--syntax=pascaligo" ] ;
  [%expect {|
    failwith("failed assertion") |}];

  run_ligo_good [ "interpret" ; "assert(1=1)" ; "--syntax=cameligo" ] ;
  [%expect {|
    unit |}];

  run_ligo_good [ "interpret" ; "assert(1=2)" ; "--syntax=cameligo" ] ;
  [%expect {|
    failwith("failed assertion") |}];
