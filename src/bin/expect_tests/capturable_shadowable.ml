open Cli_expect

let bad_test s = (bad_test "")^"capturable_shadowable/"^s
let good_test s = (test "")^"capturable_shadowable/"^s

(* Negatives *)

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "shadow_const_param.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/capturable_shadowable/shadow_const_param.ligo", line 3, characters 4-10:
      2 |   block {
      3 |     x := 4;
      4 |   } with x

    Cannot be shadowed. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "shadow_const_params.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/capturable_shadowable/shadow_const_params.ligo", line 3, characters 4-10:
      2 |   block {
      3 |     x := 4;
      4 |     y := 3;

    Cannot be shadowed. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "capture_var_param.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/capturable_shadowable/capture_var_param.ligo", line 1, characters 17-18:
      1 | function foo(var x : int) : int is
      2 |   block {

    Cannot be captured. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "capture_var_params.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/capturable_shadowable/capture_var_params.ligo", line 1, characters 17-18:
      1 | function foo(var x : int; const y : int) : int -> int is
      2 |   block {

    Cannot be captured. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "shadow_const_with_var.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/capturable_shadowable/shadow_const_with_var.ligo", line 3, characters 21-22:
      2 |   block {
      3 |     function bar(var x : int) : int is
      4 |       block { x := x + 1; } with x;

    Cannot be shadowed. |}]

(* Positives *)

let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "shadow_var_with_const.ligo") ] ;
  [%expect{|
    const foo : int -> int =
      lambda (x : int) : int return let bar : int -> int =
                                      lambda (x : int) : int return x in
                                    (bar)@(42) |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "capture_const_param.ligo") ] ;
  [%expect{|
    const foo : int -> int -> int =
      lambda (x : int) : int -> int return let bar : int -> int =
                                             lambda (y : int) : int return
                                             ADD(x , y) in
                                           bar |}]
