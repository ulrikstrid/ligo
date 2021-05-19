open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_1.mligo"; "main"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_1.mligo", line 1, characters 26-27:
      1 | let main (a:int) : unit = a

    Invalid type(s).
    Expected: "unit", but got: "int". |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_2.mligo"; "f"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_2.mligo", line 1, characters 14-43:
      1 | let f : int = fun (x, y : int*int) -> x + y
      2 | let g (x, y : int * int) : int = f (x, y)

    Invalid type(s).
    Expected: "int", but got: "( int * int ) -> int". |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_3.mligo"; "f"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_3.mligo", line 8, characters 14-20:
      7 |   match s with
      8 |   | Add si -> Add si
      9 |   | Sub si -> Sub si

    Invalid type(s).
    Expected: "( list (operation) * sum[Add -> int , Sub -> int] )", but got: "
    sum[Add -> int , Sub -> int]". |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_no_tail_recursive_function.mligo"; "f"];
  [%expect {|
    File "../../test/contracts/negative/error_no_tail_recursive_function.mligo", line 2, characters 14-21:
      1 | let rec unvalid (n:int):int =
      2 |     let res = unvalid (n) in
      3 |     res + 1

    Recursive call not in tail position.
    The value of a recursive call must be immediately returned by the defined function. |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_type.ligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_type.ligo", line 3, characters 18-28:
      2 |
      3 | const foo : nat = 42 + "bar"

    Invalid arguments.
    Expected an argument of type (bls12_381_g1, bls12_381_g1) or (bls12_381_g2, bls12_381_g2) or (bls12_381_fr, bls12_381_fr) or (nat, nat) or (int, int) or (tez, tez) or (nat, int) or (int, nat) or (timestamp, int) or (int, timestamp), but got an argument of type int, string. |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_type_record_access.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_type_record_access.mligo", line 6, characters 17-20:
      5 | let bar (x : foo) : int =
      6 |   let y : bool = x.i in
      7 |   42

    Invalid type(s).
    Expected: "sum[false -> unit , true -> unit]", but got: "int". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_type_record_update.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_type_record_update.mligo", line 7, characters 23-26:
      6 | let bar (x : foo) : foo =
      7 |   let x = { x with i = x.j } in
      8 |   x

    Invalid type(s).
    Expected: "int", but got: "sum[false -> unit , true -> unit]". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_1.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_1.mligo", line 3, characters 19-27:
      2 |
      3 | let foo : string = 42 + 127
      4 |

    Invalid type(s).
    Expected: "string", but got: "int". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_2.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_2.mligo", line 3, characters 24-39:
      2 |
      3 | let foo : string list = Some (42 + 127)
      4 |

    Invalid type(s).
    Expected: "list (string)", but got: "option (int)". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_3.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_3.mligo", line 3, characters 36-44:
      2 |
      3 | let foo : (int * string * bool) = ((1, "foo") : toto)
      4 |

    Invalid type(s).
    Expected: "( int * string * sum[false -> unit , true -> unit] )", but got: "
    ( int * string )". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_4.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_4.mligo", line 4, characters 18-48:
      3 |
      4 | let foo : tata = ({a = 1 ; b = "foo" ; c = true} : toto)
      5 |

    Invalid type(s).
    Expected: "record[a -> int , c -> sum[false -> unit , true -> unit] , d -> string]", but got: "
    record[a -> int , b -> string , c -> sum[false -> unit , true -> unit]]". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_5.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_5.mligo", line 1, characters 10-17:
      1 | let foo : boolean = 3
      2 |

    Type "boolean" not found. |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_6.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_6.mligo", line 1, characters 31-45:
      1 | let foo : (int, string) map = (Map.literal [] : (int, bool) map)
      2 | let main (p:int) (storage : int) =

    Invalid type(s).
    Expected: "map (int , string)", but got: "map (int ,
    sum[false -> unit , true -> unit])". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_7.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_7.mligo", line 4, characters 18-48:
      3 |
      4 | let foo : tata = ({a = 1 ; b = "foo" ; c = true} : toto)
      5 |

    Invalid type(s).
    Expected: "record[a -> int , b -> string]", but got: "record[a -> int , b -> string , c -> sum[false -> unit , true -> unit]]". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/id.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/id.mligo", line 45, characters 4-51:
     44 |   let updated_identities: (id, id_details) big_map =
     45 |     Big_map.update new_id new_id_details identities
     46 |   in

    Incorrect argument.
    Expected an option, but got an argument of type "record[controller -> address , owner -> address , profile -> bytes]". |}]

(*
  This test is here to ensure compatibility with comparable pairs introduced in carthage
  note that only "comb pairs" are allowed to be compared (would be better if any pair would be comparable ?)
  EDIT: With EDO, all kind of pairs are comparable
*)
let%expect_test _ =
  run_ligo_good [ "interpret" ; "Set.literal [ (1,(2,3)) ; (2,(3,4)) ]" ; "--syntax=cameligo" ] ;
  [%expect {|
    SET_ADD(( 2 , ( 3 , 4 ) ) , SET_ADD(( 1 , ( 2 , 3 ) ) , SET_EMPTY())) |}]

  (* 
  run_ligo_bad [ "interpret" ; "Set.literal [ (1,2,3) ; (2,3,4) ]" ; "--syntax=cameligo" ] ;
  [%expect {|
    Error(s) occurred while parsing the Michelson input:
    At (unshown) location 1, comparable type expected.Type
                                                        pair (pair int int) int
                                                      is not comparable. |}]
  *)

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/failwith_wrong_type.ligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/failwith_wrong_type.ligo", line 2, characters 19-46:
      1 |
      2 | const bad : unit = failwith((nil : list(int)))

    Invalid arguments.
    Expected an argument of type (string) or (nat) or (int), but got an argument of type list (int). |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/compare_sum_types.ligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/compare_sum_types.ligo", line 4, characters 29-36:
      3 | function main (const p : foo; const s : bool) : list(operation) * bool is
      4 |   ((nil : list (operation)), p = Foo)

    Invalid arguments.
    These types cannot be compared: "sum[Bar -> unit , Foo -> unit]" and "
    sum[Bar -> unit , Foo -> unit]". |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/invalid_field_record_update.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/invalid_field_record_update.mligo", line 4, characters 50-54:
      3 | let main (p:int) (storage : abc) =
      4 |   (([] : operation list) , { storage with nofield=2048} )

    Invalid record field "nofield" in record "{ storage with { nofield = 2048 } }". |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/override_option.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/override_option.mligo", line 3, characters 53-57:
      2 |
      3 | let main (x,y:bool * bool) = ([] : operation list), (None : option)

    Incorrect argument.
    Expected an option, but got an argument of type "int". |} ]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/will_be_ignored.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/will_be_ignored.mligo", line 6, characters 20-28:
      5 | let main (amoun, s: tez * storage): operation list * storage =
      6 |      let receiver : contract =
      7 |       match (Tezos.get_contract_opt(s.owner) : contract option) with

    Wrong number of arguments for type constant: contract expected: 1
    got: 0 |}]
