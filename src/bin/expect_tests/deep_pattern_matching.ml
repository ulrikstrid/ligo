open Cli_expect

let bad_test s = (bad_test "")^"/deep_pattern_matching/"^s
let good_test s = (test "")^"/deep_pattern_matching/"^s

(* Negatives *)

(* Trying to match on values *)
let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail10.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail10.mligo", line 5, characters 8-9:
      4 |   match x with
      5 |   | One 1 -> 2
      6 |   | Two -> 1

    Invalid pattern.
            Can't match on values. |}]

(* unbound variable *)

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail9.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail9.mligo", line 6, characters 11-12:
      5 |   | One a -> 2
      6 |   | Two -> a

    Variable "a" not found. |}]

(* wrong patterns type *)
let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail1.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail1.mligo", line 6, characters 10-33:
      5 |   match x with
      6 |   | Nil , {a = a ; b = b ; c = c} -> 1
      7 |   | xs  , Nil -> 2

    Pattern do not conform type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail2.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail2.mligo", line 5, characters 11-16:
      4 |   match x with
      5 |   | Nil , (a,b,c) -> 1
      6 |   | xs  , Nil -> 2

    Pattern do not conform type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail5.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail5.mligo", line 6, characters 4-13:
      5 |   | Some_fake x -> x
      6 |   | None_fake -> 1

    Pattern do not conform type option (int) |}]

(* wrong body type *)

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail7.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail7.mligo", line 6, characters 9-10:
      5 |   | A -> "hey"
      6 |   | B -> 2

    Invalid type(s).
    Expected: "string", but got: "int". |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail8.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail8.mligo", line 17, character 8 to line 18, character 15:
     16 |       | Nil ->
     17 |         let f = fun (b:int) -> b + a in
     18 |         f (b+1)
     19 |       | Cons (a,b) -> "invalid"

    Invalid type(s).
    Expected: "string", but got: "int". |}]


(* rendundancy detected while compiling the pattern matching *)
let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail3.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail3.mligo", line 4, character 2 to line 6, character 21:
      3 | let t = fun (x: myt * ( int * int * int)) ->
      4 |   match x with
      5 |   | xs , (a,b,c) -> 1
      6 |   | xs , (c,b,a) -> 2

    Redundant pattern matching |}]

(* anomaly detected in the pattern matching self_ast_typed pass *)

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail11.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail11.mligo", line 2, character 2 to line 4, character 11:
      1 | let t12 = fun (x : int list) ->
      2 |   match x with
      3 |   | hd::(hd2::tl) -> hd + hd2
      4 |   | [] -> 0

    Pattern matching anomaly (redundant, or non exhaustive). |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail12.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail12.mligo", line 4, character 2 to line 6, character 40:
      3 | let t13 = fun (x:recordi) ->
      4 |   match x with
      5 |   | { a = Some ([]) ; b = (hd::tl) } -> hd
      6 |   | { a = Some (hd::tl) ; b = [] } -> hd

    Pattern matching anomaly (redundant, or non exhaustive). |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail4.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail4.mligo", line 4, character 2 to line 6, character 18:
      3 | let t = fun (x: myt * myt) ->
      4 |   match x with
      5 |   | Nil , ys  -> 1
      6 |   | xs  , Nil -> 2

    Pattern matching anomaly (redundant, or non exhaustive). |}]

(* Positives *)

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t1 (Nil,Nil)" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t1 (Nil,Cons(1,2))" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t1 (Cons(1,2),Nil)" ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t1 (Cons(1,2),Cons(3,4))" ] ;
  [%expect{|
    10 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t2 Nil Nil" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t2 Nil (Cons (1,2))" ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t2 (Cons(1,2)) (Cons(1,2))" ] ;
  [%expect{|
    6 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t2 (Cons(1,2)) Nil" ] ;
  [%expect{|
    7 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t3 (One (Nil))" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t3 (One (Cons(1,2)))" ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t3 (Two {a = 1 ; b = 2n ; c = \"tri\"})" ] ;
  [%expect{|
    6 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t2_3 (Cons(1,2)) Nil (One(Nil))" ] ;
  [%expect{|
    8 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t4 (One(Nil)) (One (Nil))" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t4 (One(Nil)) (Two {a=1;b=2n;c=\"tri\"})" ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t4 (One(Cons(1,2))) (Two {a=1;b=2n;c=\"tri\"})" ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t4 (Two {a=0;b=0n;c=\"\"}) (Two {a=1;b=2n;c=\"tri\"})" ] ;
  [%expect{|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t5 1" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t6 42" ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t7 (Some 10)" ] ;
  [%expect{|
    10 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t7 (None: int option)" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t8 (Some (1,2)) 2" ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t8 (None:(int * int) option) 2" ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t9 (None:int option) (None:int option)" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t9 (None:int option) (Some 1)" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t9 (Some 1) (None:int option)" ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t9 (Some 1) (Some 2)" ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t10 (Consi(None:int option)) (Consi(Some 100))" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t11 (Consi(None:int option)) (Consi(Some 100))" ] ;
  [%expect{|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t12 ([]: int list)" ] ;
  [%expect{| 0 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t12 [1]" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t12 [1;2]" ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t12 [1;2;3]" ] ;
  [%expect{|
    6 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t12 [1;2;3;4]" ] ;
  [%expect{|
    -1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t13 none_a some_a" ] ;
  [%expect{|
    -1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t13 some_a a_empty_b_not" ] ;
  [%expect{|
    111 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t13 some_a b_empty_a_not" ] ;
  [%expect{|
    222 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.mligo") ; "t13 some_a some_a" ] ;
  [%expect{|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; (good_test "pm_ticket.mligo") ; "main" ] ;
  [%expect{|
    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 5, characters 18-19:
    Warning: unused variable "s".
    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 7, characters 14-17:
    Warning: unused variable "myt".

    { parameter (pair (pair (nat %mynat) (ticket %myt int)) (option nat)) ;
      storage nat ;
      code { CAR ;
             UNPAIR ;
             CAR ;
             SWAP ;
             IF_NONE { NIL operation ; PAIR } { SWAP ; DROP ; NIL operation ; PAIR } } } |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "list_pattern.mligo") ] ;
  [%expect{|
    const a =
       match CONS(1 , LIST_EMPTY()) with
        | [  ] -> 1
        | a :: b :: c :: [  ] -> 2
        | _ -> 3 |}]


(* REASONLIGO LEFTOVER: for now, we only type the test file *)
let%expect_test _ =
  run_ligo_good [ "print-ast-typed" ; (good_test "pm_test.religo") ] ;
  [%expect{|
    type myt = sum[Cons -> ( int * int ) , Nil -> unit]
    type myr = record[a -> int , b -> nat , c -> string]
    type myd = sum[One -> sum[Cons -> ( int * int ) , Nil -> unit] , Two -> record[a -> int , b -> nat , c -> string]]
    const t1 = lambda (x) return let fr = lambda (x) return 1 in let fl = lambda (x) return 2 in let #8 = x in
     match #8 with
      | ( tuple_proj#9 , ys ) ->
       match tuple_proj#9 with
        | Cons ctor_proj#22 ->
           match ys with
            | Cons ctor_proj#20 ->
               match ctor_proj#22 with
                | ( a , b ) ->
                 match ctor_proj#20 with
                  | ( c , d ) ->
                  ADD(ADD(ADD(a , b) , c) , d)
            | Nil _#19 ->
              (fl)@(tuple_proj#9)
        | Nil _#21 ->
          (fr)@(ys)
    const t7 = lambda (x) return let #23 = x in  match #23 with
                                                  | Some x ->
                                                    x | None _#24 ->
                                                        1
    const t8 = lambda (x) return lambda (y) return let #25 = ( x , y ) in
     match #25 with
      | ( tuple_proj#26 , x ) ->
       match tuple_proj#26 with
        | Some x ->
          ADD(x ,
          x) | None _#29 ->
               x
    const t9 = lambda (x) return lambda (y) return let #30 = ( x , y ) in
     match #30 with
      | ( tuple_proj#31 , ys ) ->
       match tuple_proj#31 with
        | Some ctor_proj#40 ->
           match ys with
            | Some ctor_proj#38 ->
              ADD(ctor_proj#40 ,
              ctor_proj#38)
            | None _#37 ->
              2
        | None _#39 ->
          1
    type optioni = option (int)
    type myti = sum[Consi -> option (int) , Nili -> unit]
    const fl = lambda (x) return 1
    const fo = lambda (x) return 2
    const t10 = lambda (x) return lambda (y) return let #41 = ( x , y ) in
     match #41 with
      | ( tuple_proj#42 , ys ) ->
       match tuple_proj#42 with
        | Consi ctor_proj#58 ->
           match ys with
            | Consi ctor_proj#56 ->
               match ctor_proj#58 with
                | Some ctor_proj#53 ->
                  ADD((fo)@(ctor_proj#58) ,
                  (fo)@(ctor_proj#56))
                | None _#49 ->
                   match ys with
                    | Nili ctor_proj#52 ->
                      ADD((fo)@(ctor_proj#58) ,
                      (fo)@(ctor_proj#56))
                    | Consi ctor_proj#50 ->
                       match ctor_proj#50 with
                        | None ctor_proj#51 ->
                          ADD((fo)@(ctor_proj#58) ,
                          (fo)@(ctor_proj#56))
                        | Some b ->
                          let b = 1 in b
            | Nili _#55 ->
              (fl)@(tuple_proj#42)
        | Nili _#57 ->
          (fl)@(ys)
    const t11 = lambda (x) return lambda (y) return let #59 = ( x , y ) in
     match #59 with
      | ( tuple_proj#60 , ys ) ->
       match tuple_proj#60 with
        | Consi ctor_proj#77 ->
           match ys with
            | Consi ctor_proj#75 ->
               match ctor_proj#77 with
                | None ctor_proj#72 ->
                  let #66 = ctor_proj#77 in  match #66 with
                                              | Some a ->
                                                a
                                              | None _#67 ->
                                                ADD((fo)@(ctor_proj#77) ,
                                                (fo)@(ctor_proj#75))
                | Some a ->
                   match ys with
                    | Nili ctor_proj#71 ->
                      let #66 = ctor_proj#77 in  match #66 with
                                                  | Some a ->
                                                    a
                                                  | None _#67 ->
                                                    ADD((fo)@(ctor_proj#77) ,
                                                    (fo)@(ctor_proj#75))
                    | Consi ctor_proj#69 ->
                       match ctor_proj#69 with
                        | None ctor_proj#70 ->
                          let #66 = ctor_proj#77 in  match #66 with
                                                      | Some a ->
                                                        a
                                                      | None _#67 ->
                                                        ADD((fo)@(ctor_proj#77) ,
                                                        (fo)@(ctor_proj#75))
                        | Some b ->
                          let a = 1 in ADD(a ,
                          b)
            | Nili _#74 ->
              (fl)@(tuple_proj#60)
        | Nili _#76 ->
          (fl)@(ys)
    type recordi = record[a -> option (list (int)) , b -> list (int)]
    const none_a = record[a -> NONE() , b -> CONS(42 , LIST_EMPTY())]
    const some_a = record[a -> SOME(CONS(1 , CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))))) , b -> CONS(42 , LIST_EMPTY())]
    const a_empty_b_not = record[a -> SOME(LIST_EMPTY()) , b -> CONS(111 , LIST_EMPTY())]
    const b_empty_a_not = record[a -> SOME(CONS(222 , LIST_EMPTY())) , b -> LIST_EMPTY()] |}]
