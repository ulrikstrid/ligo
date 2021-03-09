open Cli_expect

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t1" ] ;
  [%expect{|
    Warning: unused variable #23 at
    Warning: unused variable #25 at
    Warning: unused variable #27 at
    Warning: unused variable #28 at
    Warning: unused variable #31 at
    Warning: unused variable #35 at
    Warning: unused variable #36 at
    Warning: unused variable #55 at
    Warning: unused variable #19 at
    Warning: unused variable #20 at
    Warning: unused variable #22 at
    Warning: unused variable t8 at in file "../../test/contracts/let_destructuring.mligo", line 33, characters 4-6
    Warning: unused variable t7 at in file "../../test/contracts/let_destructuring.mligo", line 29, characters 4-6
    Warning: unused variable t6 at in file "../../test/contracts/let_destructuring.mligo", line 24, characters 4-6
    Warning: unused variable t5 at in file "../../test/contracts/let_destructuring.mligo", line 20, characters 4-6
    Warning: unused variable t4 at in file "../../test/contracts/let_destructuring.mligo", line 16, characters 4-6
    Warning: unused variable t3 at in file "../../test/contracts/let_destructuring.mligo", line 12, characters 4-6
    Warning: unused variable t2 at in file "../../test/contracts/let_destructuring.mligo", line 8, characters 4-6
    1 |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t2" ] ;
  [%expect{|
    Warning: unused variable #19 at
    Warning: unused variable #20 at
    Warning: unused variable #22 at
    Warning: unused variable #35 at
    Warning: unused variable #36 at
    Warning: unused variable #55 at
    Warning: unused variable #23 at
    Warning: unused variable #25 at
    Warning: unused variable #27 at
    Warning: unused variable #28 at
    Warning: unused variable #31 at
    Warning: unused variable t8 at in file "../../test/contracts/let_destructuring.mligo", line 33, characters 4-6
    Warning: unused variable t7 at in file "../../test/contracts/let_destructuring.mligo", line 29, characters 4-6
    Warning: unused variable t6 at in file "../../test/contracts/let_destructuring.mligo", line 24, characters 4-6
    Warning: unused variable t5 at in file "../../test/contracts/let_destructuring.mligo", line 20, characters 4-6
    Warning: unused variable t4 at in file "../../test/contracts/let_destructuring.mligo", line 16, characters 4-6
    Warning: unused variable t3 at in file "../../test/contracts/let_destructuring.mligo", line 12, characters 4-6
    Warning: unused variable t1 at in file "../../test/contracts/let_destructuring.mligo", line 4, characters 4-6
    "7" |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t3" ] ;
  [%expect{|
    Warning: unused variable #19 at
    Warning: unused variable #20 at
    Warning: unused variable #22 at
    Warning: unused variable #23 at
    Warning: unused variable #25 at
    Warning: unused variable #27 at
    Warning: unused variable #28 at
    Warning: unused variable #31 at
    Warning: unused variable #35 at
    Warning: unused variable #36 at
    Warning: unused variable #55 at
    Warning: unused variable t8 at in file "../../test/contracts/let_destructuring.mligo", line 33, characters 4-6
    Warning: unused variable t7 at in file "../../test/contracts/let_destructuring.mligo", line 29, characters 4-6
    Warning: unused variable t6 at in file "../../test/contracts/let_destructuring.mligo", line 24, characters 4-6
    Warning: unused variable t5 at in file "../../test/contracts/let_destructuring.mligo", line 20, characters 4-6
    Warning: unused variable t4 at in file "../../test/contracts/let_destructuring.mligo", line 16, characters 4-6
    Warning: unused variable t2 at in file "../../test/contracts/let_destructuring.mligo", line 8, characters 4-6
    Warning: unused variable t1 at in file "../../test/contracts/let_destructuring.mligo", line 4, characters 4-6
    ( 3 , +3 , "7" ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t4" ] ;
  [%expect{|
    Warning: unused variable #19 at
    Warning: unused variable #20 at
    Warning: unused variable #22 at
    Warning: unused variable #23 at
    Warning: unused variable #25 at
    Warning: unused variable #27 at
    Warning: unused variable #28 at
    Warning: unused variable #31 at
    Warning: unused variable #35 at
    Warning: unused variable #36 at
    Warning: unused variable #55 at
    Warning: unused variable t8 at in file "../../test/contracts/let_destructuring.mligo", line 33, characters 4-6
    Warning: unused variable t7 at in file "../../test/contracts/let_destructuring.mligo", line 29, characters 4-6
    Warning: unused variable t6 at in file "../../test/contracts/let_destructuring.mligo", line 24, characters 4-6
    Warning: unused variable t5 at in file "../../test/contracts/let_destructuring.mligo", line 20, characters 4-6
    Warning: unused variable t3 at in file "../../test/contracts/let_destructuring.mligo", line 12, characters 4-6
    Warning: unused variable t2 at in file "../../test/contracts/let_destructuring.mligo", line 8, characters 4-6
    Warning: unused variable t1 at in file "../../test/contracts/let_destructuring.mligo", line 4, characters 4-6
    ( 4 , +3 ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t5" ] ;
  [%expect{|
    Warning: unused variable #19 at
    Warning: unused variable #20 at
    Warning: unused variable #22 at
    Warning: unused variable #23 at
    Warning: unused variable #25 at
    Warning: unused variable #27 at
    Warning: unused variable #28 at
    Warning: unused variable #31 at
    Warning: unused variable #35 at
    Warning: unused variable #36 at
    Warning: unused variable #55 at
    Warning: unused variable t8 at in file "../../test/contracts/let_destructuring.mligo", line 33, characters 4-6
    Warning: unused variable t7 at in file "../../test/contracts/let_destructuring.mligo", line 29, characters 4-6
    Warning: unused variable t6 at in file "../../test/contracts/let_destructuring.mligo", line 24, characters 4-6
    Warning: unused variable t4 at in file "../../test/contracts/let_destructuring.mligo", line 16, characters 4-6
    Warning: unused variable t3 at in file "../../test/contracts/let_destructuring.mligo", line 12, characters 4-6
    Warning: unused variable t2 at in file "../../test/contracts/let_destructuring.mligo", line 8, characters 4-6
    Warning: unused variable t1 at in file "../../test/contracts/let_destructuring.mligo", line 4, characters 4-6
    +1 |}] ;
  run_ligo_bad [ "interpret" ; "--init="^(bad_test "let_destructuring.mligo") ; "t1" ] ;
  [%expect{|
    in file "../../test/contracts/negative/let_destructuring.mligo", line 4, characters 6-23
      3 | let t1 =
      4 |   let { a = a ; f = b }  = { a = 1 ; b = 1n } in
      5 |   (a,b)

    Labels do not match: Expected b but got f . |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t1" ] ;
  [%expect{|
    Warning: unused variable #17 at
    Warning: unused variable #19 at
    Warning: unused variable #21 at
    Warning: unused variable #22 at
    Warning: unused variable #25 at
    Warning: unused variable #29 at
    Warning: unused variable #30 at
    Warning: unused variable #13 at
    Warning: unused variable #14 at
    Warning: unused variable #16 at
    Warning: unused variable t4 at in file "../../test/contracts/let_destructuring.religo", line 16, characters 4-6
    Warning: unused variable t3 at in file "../../test/contracts/let_destructuring.religo", line 12, characters 4-6
    Warning: unused variable t2 at in file "../../test/contracts/let_destructuring.religo", line 8, characters 4-6
    1 |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t2" ] ;
  [%expect{|
    Warning: unused variable #13 at
    Warning: unused variable #14 at
    Warning: unused variable #16 at
    Warning: unused variable #29 at
    Warning: unused variable #30 at
    Warning: unused variable #17 at
    Warning: unused variable #19 at
    Warning: unused variable #21 at
    Warning: unused variable #22 at
    Warning: unused variable #25 at
    Warning: unused variable t4 at in file "../../test/contracts/let_destructuring.religo", line 16, characters 4-6
    Warning: unused variable t3 at in file "../../test/contracts/let_destructuring.religo", line 12, characters 4-6
    Warning: unused variable t1 at in file "../../test/contracts/let_destructuring.religo", line 4, characters 4-6
    "7" |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t3" ] ;
  [%expect{|
    Warning: unused variable #13 at
    Warning: unused variable #14 at
    Warning: unused variable #16 at
    Warning: unused variable #17 at
    Warning: unused variable #19 at
    Warning: unused variable #21 at
    Warning: unused variable #22 at
    Warning: unused variable #25 at
    Warning: unused variable #29 at
    Warning: unused variable #30 at
    Warning: unused variable t4 at in file "../../test/contracts/let_destructuring.religo", line 16, characters 4-6
    Warning: unused variable t2 at in file "../../test/contracts/let_destructuring.religo", line 8, characters 4-6
    Warning: unused variable t1 at in file "../../test/contracts/let_destructuring.religo", line 4, characters 4-6
    ( 3 , +3 , "7" ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t4" ] ;
   [%expect{|
     Warning: unused variable #13 at
     Warning: unused variable #14 at
     Warning: unused variable #16 at
     Warning: unused variable #17 at
     Warning: unused variable #19 at
     Warning: unused variable #21 at
     Warning: unused variable #22 at
     Warning: unused variable #25 at
     Warning: unused variable #29 at
     Warning: unused variable #30 at
     Warning: unused variable t3 at in file "../../test/contracts/let_destructuring.religo", line 12, characters 4-6
     Warning: unused variable t2 at in file "../../test/contracts/let_destructuring.religo", line 8, characters 4-6
     Warning: unused variable t1 at in file "../../test/contracts/let_destructuring.religo", line 4, characters 4-6
     ( 4 , +3 ) |}] ;