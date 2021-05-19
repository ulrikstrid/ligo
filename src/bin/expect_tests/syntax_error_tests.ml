open Cli_expect

(* avoid pretty printing *)
let () = Unix.putenv "TERM" "dumb"

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_syntax.ligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_syntax.ligo", line 1, characters 16-17:
      1 | type foo is bar - 42
    Ill-formed contract.
    At this point, one the following is expected:
      * another declaration;
      * the end of the file. |} ]

let%expect_test _ =
  run_ligo_bad [ "compile-expression" ; "jsligo" ; "Bytes.X()" ] ;
  [%expect {| Unknown constructor in module: Bytes |} ]

