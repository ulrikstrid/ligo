open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "run-function" ; contract "failwith.ligo" ; "failer" ; "1" ] ;
  [%expect {|
    Warning: unused variable _ at
    Warning: unused variable _ at
    Warning: unused variable _ at
    Warning: unused variable env#5 at
    Warning: unused variable _ at
    Warning: unused variable _ at
    Warning: unused variable left#14 at
    Warning: unused variable right#15 at
    Warning: unused variable _ at
    Warning: unused variable left#20 at
    Warning: unused variable right#21 at
    Warning: unused variable _ at
    Warning: unused variable foobar at in file "../../test/contracts/failwith.ligo", line 18, characters 9-15
    Warning: unused variable main at in file "../../test/contracts/failwith.ligo", line 9, characters 9-13
    failwith(42) |}];

  run_ligo_good [ "run-function" ; contract "failwith.ligo" ; "failer" ; "1" ; "--format=json" ] ;
  [%expect {|
    Warning: unused variable _ at
    Warning: unused variable _ at
    Warning: unused variable _ at
    Warning: unused variable env#5 at
    Warning: unused variable _ at
    Warning: unused variable _ at
    Warning: unused variable left#14 at
    Warning: unused variable right#15 at
    Warning: unused variable _ at
    Warning: unused variable left#20 at
    Warning: unused variable right#21 at
    Warning: unused variable _ at
    Warning: unused variable foobar at in file "../../test/contracts/failwith.ligo", line 18, characters 9-15
    Warning: unused variable main at in file "../../test/contracts/failwith.ligo", line 9, characters 9-13
    { "value": null, "failure": "failwith(42)" } |}];


  run_ligo_good [ "dry-run" ; contract "subtle_nontail_fail.mligo" ; "main" ; "()" ; "()" ] ;
  [%expect {|
    Warning: unused variable ps at in file "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 9-27
    Warning: unused variable main at in file "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 4-8
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
