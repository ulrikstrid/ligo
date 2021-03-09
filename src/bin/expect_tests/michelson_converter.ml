open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

(* avoid pretty printing *)
let () = Unix.putenv "TERM" "dumb"

let%expect_test _ =
  run_ligo_bad [ "interpret" ; "--init-file="^(bad_contract "michelson_converter_short_record.mligo") ; "l1"] ;
  [%expect {|
    in file "../../test/contracts/negative/michelson_converter_short_record.mligo", line 4, characters 9-44
      3 |
      4 | let l1 = Layout.convert_to_left_comb (v1:t1)

    Incorrect argument provided to Layout.convert_to_(left|right)_comb.
    The record must have at least two elements. |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(contract "michelson_converter_pair.mligo") ; "r3"] ;
  [%expect {|
    Warning: unused variable #4 at
    Warning: unused variable #6 at
    Warning: unused variable l4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 29, characters 4-6
    Warning: unused variable l3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 28, characters 4-6
    Warning: unused variable r4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 26, characters 4-6
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 16, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 11, characters 4-10
    Warning: unused variable test_input_pair_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 8, characters 4-21
    Warning: unused variable test_input_pair_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 7, characters 4-21
    ( 2 , ( +3 , "q" ) ) |}] ;
  run_ligo_good [ "interpret" ; "--init-file="^(contract "michelson_converter_pair.mligo") ; "r4"] ;
  [%expect {|
    Warning: unused variable #4 at
    Warning: unused variable #6 at
    Warning: unused variable l4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 29, characters 4-6
    Warning: unused variable l3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 28, characters 4-6
    Warning: unused variable r3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 25, characters 4-6
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 16, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 11, characters 4-10
    Warning: unused variable test_input_pair_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 8, characters 4-21
    Warning: unused variable test_input_pair_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 7, characters 4-21
    ( 2 , ( +3 , ( "q" , true(unit) ) ) ) |}] ;
  run_ligo_good [ "interpret" ; "--init-file="^(contract "michelson_converter_pair.mligo") ; "l3"] ;
  [%expect {|
    Warning: unused variable #4 at
    Warning: unused variable #6 at
    Warning: unused variable l4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 29, characters 4-6
    Warning: unused variable r4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 26, characters 4-6
    Warning: unused variable r3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 25, characters 4-6
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 16, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 11, characters 4-10
    Warning: unused variable test_input_pair_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 8, characters 4-21
    Warning: unused variable test_input_pair_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 7, characters 4-21
    ( ( 2 , +3 ) , "q" ) |}] ;
  run_ligo_good [ "interpret" ; "--init-file="^(contract "michelson_converter_pair.mligo") ; "l4"] ;
  [%expect {|
    Warning: unused variable #4 at
    Warning: unused variable #6 at
    Warning: unused variable l3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 28, characters 4-6
    Warning: unused variable r4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 26, characters 4-6
    Warning: unused variable r3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 25, characters 4-6
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 16, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 11, characters 4-10
    Warning: unused variable test_input_pair_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 8, characters 4-21
    Warning: unused variable test_input_pair_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 7, characters 4-21
    ( ( ( 2 , +3 ) , "q" ) , true(unit) ) |}];
  run_ligo_good [ "interpret" ; "--init-file="^(contract "michelson_converter_or.mligo") ; "str3"] ;
  [%expect {|
    Warning: unused variable #10 at
    Warning: unused variable #18 at
    Warning: unused variable stl4 at in file "../../test/contracts/michelson_converter_or.mligo", line 43, characters 4-8
    Warning: unused variable stl3 at in file "../../test/contracts/michelson_converter_or.mligo", line 42, characters 4-8
    Warning: unused variable str4 at in file "../../test/contracts/michelson_converter_or.mligo", line 40, characters 4-8
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_or.mligo", line 30, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_or.mligo", line 25, characters 4-10
    Warning: unused variable vl at in file "../../test/contracts/michelson_converter_or.mligo", line 22, characters 4-6
    Warning: unused variable vr at in file "../../test/contracts/michelson_converter_or.mligo", line 17, characters 4-6
    M_right(M_left(+3)) |}] ;
  run_ligo_good [ "interpret" ; "--init-file="^(contract "michelson_converter_or.mligo") ; "str4"] ;
  [%expect {|
    Warning: unused variable #10 at
    Warning: unused variable #18 at
    Warning: unused variable stl4 at in file "../../test/contracts/michelson_converter_or.mligo", line 43, characters 4-8
    Warning: unused variable stl3 at in file "../../test/contracts/michelson_converter_or.mligo", line 42, characters 4-8
    Warning: unused variable str3 at in file "../../test/contracts/michelson_converter_or.mligo", line 39, characters 4-8
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_or.mligo", line 30, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_or.mligo", line 25, characters 4-10
    Warning: unused variable vl at in file "../../test/contracts/michelson_converter_or.mligo", line 22, characters 4-6
    Warning: unused variable vr at in file "../../test/contracts/michelson_converter_or.mligo", line 17, characters 4-6
    M_right(M_right(M_left("eq"))) |}] ;
  run_ligo_good [ "interpret" ; "--init-file="^(contract "michelson_converter_or.mligo") ; "stl3"] ;
  [%expect {|
    Warning: unused variable #10 at
    Warning: unused variable #18 at
    Warning: unused variable stl4 at in file "../../test/contracts/michelson_converter_or.mligo", line 43, characters 4-8
    Warning: unused variable str4 at in file "../../test/contracts/michelson_converter_or.mligo", line 40, characters 4-8
    Warning: unused variable str3 at in file "../../test/contracts/michelson_converter_or.mligo", line 39, characters 4-8
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_or.mligo", line 30, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_or.mligo", line 25, characters 4-10
    Warning: unused variable vl at in file "../../test/contracts/michelson_converter_or.mligo", line 22, characters 4-6
    Warning: unused variable vr at in file "../../test/contracts/michelson_converter_or.mligo", line 17, characters 4-6
    M_left(M_right(+3)) |}] ;
  run_ligo_good [ "interpret" ; "--init-file="^(contract "michelson_converter_or.mligo") ; "stl4"] ;
  [%expect {|
    Warning: unused variable #10 at
    Warning: unused variable #18 at
    Warning: unused variable stl3 at in file "../../test/contracts/michelson_converter_or.mligo", line 42, characters 4-8
    Warning: unused variable str4 at in file "../../test/contracts/michelson_converter_or.mligo", line 40, characters 4-8
    Warning: unused variable str3 at in file "../../test/contracts/michelson_converter_or.mligo", line 39, characters 4-8
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_or.mligo", line 30, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_or.mligo", line 25, characters 4-10
    Warning: unused variable vl at in file "../../test/contracts/michelson_converter_or.mligo", line 22, characters 4-6
    Warning: unused variable vr at in file "../../test/contracts/michelson_converter_or.mligo", line 17, characters 4-6
    M_left(M_right("eq")) |}]

let%expect_test _ =
  run_ligo_good [ "dry-run" ; (contract "michelson_converter_pair.mligo") ; "main_r" ; "test_input_pair_r" ; "s"] ;
  [%expect {|
    Warning: unused variable #4 at
    Warning: unused variable test_input_pair_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 8, characters 4-21
    Warning: unused variable test_input_pair_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 7, characters 4-21
    Warning: unused variable #4 at
    Warning: unused variable #6 at
    Warning: unused variable l4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 29, characters 4-6
    Warning: unused variable l3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 28, characters 4-6
    Warning: unused variable r4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 26, characters 4-6
    Warning: unused variable r3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 25, characters 4-6
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 16, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 11, characters 4-10
    Warning: unused variable test_input_pair_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 8, characters 4-21
    ( LIST_EMPTY() , "eqeq" ) |}] ;
  run_ligo_good [ "compile-contract" ; (contract "michelson_converter_pair.mligo") ; "main_r" ] ;
  [%expect {|
    Warning: unused variable #4 at
    Warning: unused variable test_input_pair_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 8, characters 4-21
    Warning: unused variable test_input_pair_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 7, characters 4-21
    { parameter (pair (int %one) (pair (nat %two) (pair (string %three) (bool %four)))) ;
      storage string ;
      code { PUSH string "eq" ;
             PUSH bool True ;
             SWAP ;
             DUP ;
             DUG 2 ;
             DROP 3 ;
             CAR ;
             DUP ;
             CDR ;
             CDR ;
             CAR ;
             SWAP ;
             CDR ;
             CDR ;
             CAR ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "dry-run" ; (contract "michelson_converter_pair.mligo") ; "main_l" ; "test_input_pair_l" ; "s"] ;
  [%expect {|
    Warning: unused variable #4 at
    Warning: unused variable #6 at
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 11, characters 4-10
    Warning: unused variable test_input_pair_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 8, characters 4-21
    Warning: unused variable test_input_pair_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 7, characters 4-21
    Warning: unused variable #4 at
    Warning: unused variable #6 at
    Warning: unused variable l4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 29, characters 4-6
    Warning: unused variable l3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 28, characters 4-6
    Warning: unused variable r4 at in file "../../test/contracts/michelson_converter_pair.mligo", line 26, characters 4-6
    Warning: unused variable r3 at in file "../../test/contracts/michelson_converter_pair.mligo", line 25, characters 4-6
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 16, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 11, characters 4-10
    Warning: unused variable test_input_pair_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 7, characters 4-21
    ( LIST_EMPTY() , "eqeq" ) |}] ;
  run_ligo_good [ "compile-contract" ; (contract "michelson_converter_pair.mligo") ; "main_l" ] ;
  [%expect {|
    Warning: unused variable #4 at
    Warning: unused variable #6 at
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 11, characters 4-10
    Warning: unused variable test_input_pair_l at in file "../../test/contracts/michelson_converter_pair.mligo", line 8, characters 4-21
    Warning: unused variable test_input_pair_r at in file "../../test/contracts/michelson_converter_pair.mligo", line 7, characters 4-21
    { parameter (pair (pair (pair (int %one) (nat %two)) (string %three)) (bool %four)) ;
      storage string ;
      code { PUSH string "eq" ;
             PUSH bool True ;
             SWAP ;
             DUP ;
             DUG 2 ;
             DROP 3 ;
             CAR ;
             DUP ;
             CAR ;
             CDR ;
             SWAP ;
             CAR ;
             CDR ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "dry-run" ; contract "michelson_converter_or.mligo" ; "main_r" ; "vr" ; "Foo4 2"] ;
  [%expect {|
    Warning: unused variable #10 at
    Warning: unused variable vl at in file "../../test/contracts/michelson_converter_or.mligo", line 22, characters 4-6
    Warning: unused variable vr at in file "../../test/contracts/michelson_converter_or.mligo", line 17, characters 4-6
    Warning: unused variable #10 at
    Warning: unused variable #18 at
    Warning: unused variable stl4 at in file "../../test/contracts/michelson_converter_or.mligo", line 43, characters 4-8
    Warning: unused variable stl3 at in file "../../test/contracts/michelson_converter_or.mligo", line 42, characters 4-8
    Warning: unused variable str4 at in file "../../test/contracts/michelson_converter_or.mligo", line 40, characters 4-8
    Warning: unused variable str3 at in file "../../test/contracts/michelson_converter_or.mligo", line 39, characters 4-8
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_or.mligo", line 30, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_or.mligo", line 25, characters 4-10
    Warning: unused variable vl at in file "../../test/contracts/michelson_converter_or.mligo", line 22, characters 4-6
    ( LIST_EMPTY() , Baz4("eq") ) |}] ;
  run_ligo_good [ "compile-contract" ; contract "michelson_converter_or.mligo" ; "main_r" ] ;
  [%expect {|
    Warning: unused variable #10 at
    Warning: unused variable vl at in file "../../test/contracts/michelson_converter_or.mligo", line 22, characters 4-6
    Warning: unused variable vr at in file "../../test/contracts/michelson_converter_or.mligo", line 17, characters 4-6
    { parameter (or (int %foo4) (or (nat %bar4) (or (string %baz4) (bool %boz4)))) ;
      storage (or (or (nat %bar4) (string %baz4)) (or (bool %boz4) (int %foo4))) ;
      code { CAR ;
             IF_LEFT
               { RIGHT bool ; RIGHT (or nat string) }
               { IF_LEFT
                   { LEFT string ; LEFT (or bool int) }
                   { IF_LEFT
                       { RIGHT nat ; LEFT (or bool int) }
                       { LEFT int ; RIGHT (or nat string) } } } ;
             NIL operation ;
             PAIR } } |}] ;
  run_ligo_good [ "dry-run" ; contract "michelson_converter_or.mligo" ; "main_l" ; "vl" ; "Foo4 2"] ;
  [%expect {|
    Warning: unused variable #10 at
    Warning: unused variable #18 at
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_or.mligo", line 25, characters 4-10
    Warning: unused variable vl at in file "../../test/contracts/michelson_converter_or.mligo", line 22, characters 4-6
    Warning: unused variable vr at in file "../../test/contracts/michelson_converter_or.mligo", line 17, characters 4-6
    Warning: unused variable #10 at
    Warning: unused variable #18 at
    Warning: unused variable stl4 at in file "../../test/contracts/michelson_converter_or.mligo", line 43, characters 4-8
    Warning: unused variable stl3 at in file "../../test/contracts/michelson_converter_or.mligo", line 42, characters 4-8
    Warning: unused variable str4 at in file "../../test/contracts/michelson_converter_or.mligo", line 40, characters 4-8
    Warning: unused variable str3 at in file "../../test/contracts/michelson_converter_or.mligo", line 39, characters 4-8
    Warning: unused variable main_l at in file "../../test/contracts/michelson_converter_or.mligo", line 30, characters 4-10
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_or.mligo", line 25, characters 4-10
    Warning: unused variable vr at in file "../../test/contracts/michelson_converter_or.mligo", line 17, characters 4-6
    ( LIST_EMPTY() , Baz4("eq") ) |}] ;
  run_ligo_good [ "compile-contract" ; contract "michelson_converter_or.mligo" ; "main_l" ] ;
  [%expect {|
    Warning: unused variable #10 at
    Warning: unused variable #18 at
    Warning: unused variable main_r at in file "../../test/contracts/michelson_converter_or.mligo", line 25, characters 4-10
    Warning: unused variable vl at in file "../../test/contracts/michelson_converter_or.mligo", line 22, characters 4-6
    Warning: unused variable vr at in file "../../test/contracts/michelson_converter_or.mligo", line 17, characters 4-6
    { parameter (or (or (or (int %foo4) (nat %bar4)) (string %baz4)) (bool %boz4)) ;
      storage (or (or (nat %bar4) (string %baz4)) (or (bool %boz4) (int %foo4))) ;
      code { CAR ;
             IF_LEFT
               { IF_LEFT
                   { IF_LEFT
                       { RIGHT bool ; RIGHT (or nat string) }
                       { LEFT string ; LEFT (or bool int) } }
                   { RIGHT nat ; LEFT (or bool int) } }
               { LEFT int ; RIGHT (or nat string) } ;
             NIL operation ;
             PAIR } } |}]


let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_comb_type_operators.mligo" ; "main_r"] ;
  [%expect {|
    Warning: unused variable #3 at
    Warning: unused variable #4 at
    { parameter (pair (int %foo) (pair (nat %bar) (string %baz))) ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}] ;

  run_ligo_good [ "compile-contract" ; contract "michelson_comb_type_operators.mligo" ; "main_l"] ;
  [%expect {|
    Warning: unused variable #3 at
    Warning: unused variable #4 at
    Warning: unused variable #5 at
    Warning: unused variable #6 at
    Warning: unused variable main_r at in file "../../test/contracts/michelson_comb_type_operators.mligo", line 6, characters 4-10
    { parameter (pair (pair (int %foo) (nat %bar)) (string %baz)) ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; (contract "michelson_converter_mixed_pair_or.mligo") ; "main2" ] ;
  [%expect {|
    Warning: unused variable #11 at
    { parameter
        (or (pair %option1 (string %bar) (nat %baz)) (pair %option2 (string %bar) (nat %baz))) ;
      storage nat ;
      code { CAR ;
             IF_LEFT { LEFT (pair string nat) } { RIGHT (pair string nat) } ;
             IF_LEFT { LEFT (pair string nat) } { RIGHT (pair string nat) } ;
             IF_LEFT { CDR ; NIL operation ; PAIR } { CDR ; NIL operation ; PAIR } } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; (contract "double_fold_converter.religo") ; "main" ] ;
  [%expect {|
    Warning: unused variable tokenOwner at in file "../../test/contracts/double_fold_converter.religo", line 33, characters 8-18
    Warning: unused variable #16 at
    Warning: unused variable _ at
    { parameter
        (list (pair (address %from_)
                    (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount)))))) ;
      storage (big_map nat address) ;
      code { UNPAIR ;
             ITER { DUP ;
                    DUG 2 ;
                    CAR ;
                    SENDER ;
                    SWAP ;
                    DUP ;
                    DUG 2 ;
                    COMPARE ;
                    NEQ ;
                    IF { PUSH string "NOT_OWNER" ; FAILWITH } {} ;
                    SWAP ;
                    PAIR ;
                    SWAP ;
                    CDR ;
                    ITER { SWAP ;
                           UNPAIR ;
                           DUP 3 ;
                           CDR ;
                           CAR ;
                           DUP 4 ;
                           CAR ;
                           DIG 4 ;
                           CDR ;
                           CDR ;
                           PAIR ;
                           PAIR ;
                           SWAP ;
                           DUP ;
                           DUG 2 ;
                           SWAP ;
                           DUP ;
                           DUG 2 ;
                           CDR ;
                           GET ;
                           IF_NONE
                             { PUSH string "TOKEN_UNDEFINED" ; FAILWITH }
                             { DUP 4 ;
                               SWAP ;
                               DUP ;
                               DUG 2 ;
                               COMPARE ;
                               EQ ;
                               IF { DROP } { DROP ; PUSH string "INSUFFICIENT_BALANCE" ; FAILWITH } } ;
                           DUP ;
                           DUG 3 ;
                           CAR ;
                           CDR ;
                           SOME ;
                           DIG 3 ;
                           CDR ;
                           UPDATE ;
                           PAIR } ;
                    CAR } ;
             NIL operation ;
             PAIR } } |}]