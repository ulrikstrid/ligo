open Cli_expect

let contract = test
let bad_contract = bad_test

(* avoid pretty printing *)
let () = Unix.putenv "TERM" "dumb"

let%expect_test _ =
  run_ligo_good [ "measure-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {| 1175 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {| 567 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {| 1539 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {| 430 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "issue-184-combs.mligo" ; "main2" ] ;
  [%expect {| 231 bytes |}] ;

  run_ligo_good [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {| (Left (Left 1)) |}] ;

  run_ligo_good [ "compile-storage" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {| (Pair (Pair {} {}) 3) |}] ;

  run_ligo_bad [ "compile-storage" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {|
Invalid command line argument.
The provided storage does not have the correct type for the contract.
in file "../../test/contracts/coase.ligo", line 124, characters 9-13
123 |
124 | function main (const action : parameter; const s : storage) : return is
125 |   case action of

Invalid type(s).
Expected: "record[card_patterns -> map (nat , record[coefficient -> tez , quantity -> nat]) , cards -> map (nat , record[card_owner -> address , card_pattern -> nat]) , next_id -> nat]", but got: "
sum[Buy_single -> record[card_to_buy -> nat] , Sell_single -> record[card_to_sell -> nat] , Transfer_single -> record[card_to_transfer -> nat , destination -> address]]". |}] ;

  run_ligo_bad [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {|
Invalid command line argument.
The provided parameter does not have the correct type for the given entrypoint.
in file "../../test/contracts/coase.ligo", line 124, characters 9-13
123 |
124 | function main (const action : parameter; const s : storage) : return is
125 |   case action of

Invalid type(s).
Expected: "sum[Buy_single -> record[card_to_buy -> nat] , Sell_single -> record[card_to_sell -> nat] , Transfer_single -> record[card_to_transfer -> nat , destination -> address]]", but got: "
record[card_patterns -> map (nat , record[coefficient -> tez , quantity -> nat]) , cards -> map (nat , record[card_owner -> address , card_pattern -> nat]) , next_id -> nat]". |}] ;

  ()

let%expect_test _  =
  run_ligo_good [ "compile-storage" ; contract "timestamp.ligo" ; "main" ; "now" ; "--now" ; "2042-01-01T00:00:00Z" ] ;
  [%expect {| "2042-01-01T00:00:00Z" |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {|
{ parameter
    (or (or (nat %buy_single) (nat %sell_single))
        (pair %transfer_single (nat %card_to_transfer) (address %destination))) ;
  storage
    (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
          (nat %next_id)) ;
  code { UNPAIR ;
         IF_LEFT
           { IF_LEFT
               { SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 CAR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 GET ;
                 IF_NONE { PUSH string "buy_single: No card pattern." ; FAILWITH } {} ;
                 PUSH nat 1 ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 ADD ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 MUL ;
                 AMOUNT ;
                 SWAP ;
                 COMPARE ;
                 GT ;
                 IF { PUSH string "Not enough money" ; FAILWITH } {} ;
                 PUSH nat 1 ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 ADD ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 DUP 3 ;
                 CDR ;
                 DUP 4 ;
                 CAR ;
                 CDR ;
                 DIG 4 ;
                 CAR ;
                 CAR ;
                 DIG 3 ;
                 DUP 5 ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 PAIR ;
                 PAIR ;
                 DUP ;
                 CAR ;
                 CDR ;
                 DIG 2 ;
                 SENDER ;
                 PAIR ;
                 DUP 3 ;
                 CDR ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 SWAP ;
                 DIG 2 ;
                 CAR ;
                 CAR ;
                 PAIR ;
                 PAIR ;
                 PUSH nat 1 ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 ADD ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 NIL operation ;
                 PAIR }
               { SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 CDR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 GET ;
                 IF_NONE { PUSH string "sell_single: No card." ; FAILWITH } {} ;
                 SENDER ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 COMPARE ;
                 NEQ ;
                 IF { PUSH string "This card doesn't belong to you" ; FAILWITH } {} ;
                 DUP 3 ;
                 CAR ;
                 CAR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 GET ;
                 IF_NONE { PUSH string "sell_single: No card pattern." ; FAILWITH } {} ;
                 PUSH nat 1 ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 SUB ;
                 ABS ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 DUP 4 ;
                 CDR ;
                 DUP 5 ;
                 CAR ;
                 CDR ;
                 DIG 5 ;
                 CAR ;
                 CAR ;
                 DUP 4 ;
                 DIG 5 ;
                 CDR ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 PAIR ;
                 PAIR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 DIG 2 ;
                 CAR ;
                 MUL ;
                 SENDER ;
                 CONTRACT unit ;
                 IF_NONE { PUSH string "sell_single: No contract." ; FAILWITH } {} ;
                 SWAP ;
                 UNIT ;
                 TRANSFER_TOKENS ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 DUP 3 ;
                 CAR ;
                 CDR ;
                 DIG 4 ;
                 NONE (pair address nat) ;
                 SWAP ;
                 UPDATE ;
                 DIG 3 ;
                 CAR ;
                 CAR ;
                 PAIR ;
                 PAIR ;
                 NIL operation ;
                 DIG 2 ;
                 CONS ;
                 PAIR } }
           { SWAP ;
             DUP ;
             DUG 2 ;
             CAR ;
             CDR ;
             DUP ;
             DUP 3 ;
             CAR ;
             GET ;
             IF_NONE { PUSH string "transfer_single: No card." ; FAILWITH } {} ;
             SENDER ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CAR ;
             COMPARE ;
             NEQ ;
             IF { PUSH string "This card doesn't belong to you" ; FAILWITH } {} ;
             DUP 4 ;
             CDR ;
             DUG 2 ;
             CDR ;
             DUP 4 ;
             CDR ;
             PAIR ;
             DIG 3 ;
             CAR ;
             SWAP ;
             SOME ;
             SWAP ;
             UPDATE ;
             DIG 2 ;
             CAR ;
             CAR ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {|
{ parameter
    (pair (pair (nat %counter) (lambda %message unit (list operation)))
          (list %signatures (pair key_hash signature))) ;
  storage
    (pair (pair (list %auth key) (nat %counter)) (pair (string %id) (nat %threshold))) ;
  code { UNPAIR ;
         DUP ;
         CAR ;
         CDR ;
         DUP 3 ;
         CAR ;
         CDR ;
         DUP 3 ;
         CAR ;
         CAR ;
         COMPARE ;
         NEQ ;
         IF { SWAP ; DROP ; PUSH string "Counters does not match" ; FAILWITH }
            { CHAIN_ID ;
              DUP 4 ;
              CDR ;
              CAR ;
              PAIR ;
              DUP 3 ;
              CAR ;
              CAR ;
              DUP 3 ;
              PAIR ;
              PAIR ;
              PACK ;
              PUSH nat 0 ;
              DUP 5 ;
              CAR ;
              CAR ;
              PAIR ;
              DIG 3 ;
              CDR ;
              ITER { SWAP ;
                     PAIR ;
                     DUP ;
                     CAR ;
                     CDR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     CAR ;
                     DIG 2 ;
                     CDR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     IF_CONS
                       { DIG 3 ;
                         DROP ;
                         DUP ;
                         HASH_KEY ;
                         DUP 4 ;
                         CAR ;
                         COMPARE ;
                         EQ ;
                         IF { DUP 5 ;
                              DIG 3 ;
                              CDR ;
                              DIG 2 ;
                              CHECK_SIGNATURE ;
                              IF { PUSH nat 1 ; DIG 2 ; ADD }
                                 { PUSH string "Invalid signature" ; FAILWITH } }
                            { DROP ; SWAP ; DROP ; SWAP } ;
                         SWAP ;
                         PAIR }
                       { DROP ; PAIR } } ;
              SWAP ;
              DROP ;
              DUP 3 ;
              CDR ;
              CDR ;
              SWAP ;
              CDR ;
              COMPARE ;
              LT ;
              IF { PUSH string "Not enough signatures passed the check" ; FAILWITH }
                 { SWAP ;
                   DUP ;
                   DUG 2 ;
                   CDR ;
                   PUSH nat 1 ;
                   DUP 4 ;
                   CAR ;
                   CDR ;
                   ADD ;
                   DIG 3 ;
                   CAR ;
                   CAR ;
                   PAIR ;
                   PAIR } } ;
         UNIT ;
         DIG 2 ;
         SWAP ;
         EXEC ;
         PAIR } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {|
{ parameter
    (or (or (unit %default) (lambda %send bytes (list operation)))
        (lambda %withdraw bytes (list operation))) ;
  storage
    (pair (pair (pair (set %authorized_addresses address) (nat %max_message_size))
                (pair (nat %max_proposal) (map %message_store bytes (set address))))
          (pair (pair (map %proposal_counters address nat) (bytes %state_hash))
                (nat %threshold))) ;
  code { UNPAIR ;
         IF_LEFT
           { IF_LEFT
               { DROP ; NIL operation ; PAIR }
               { SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 CAR ;
                 CAR ;
                 SENDER ;
                 MEM ;
                 NOT ;
                 IF { PUSH string "Unauthorized address" ; FAILWITH } {} ;
                 DUP ;
                 PACK ;
                 DUP 3 ;
                 CAR ;
                 CAR ;
                 CDR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 SIZE ;
                 COMPARE ;
                 GT ;
                 IF { PUSH string "Message size exceed maximum limit" ; FAILWITH } {} ;
                 DUP 3 ;
                 CAR ;
                 CDR ;
                 CDR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 GET ;
                 IF_NONE
                   { DUP 3 ;
                     CDR ;
                     CDR ;
                     DUP 4 ;
                     CDR ;
                     CAR ;
                     CDR ;
                     DUP 5 ;
                     CDR ;
                     CAR ;
                     CAR ;
                     PUSH nat 1 ;
                     DUP 7 ;
                     CDR ;
                     CAR ;
                     CAR ;
                     SENDER ;
                     GET ;
                     IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                     ADD ;
                     SOME ;
                     SENDER ;
                     UPDATE ;
                     PAIR ;
                     PAIR ;
                     DIG 3 ;
                     CAR ;
                     PAIR ;
                     EMPTY_SET address ;
                     PUSH bool True ;
                     SENDER ;
                     UPDATE ;
                     PAIR }
                   { DUP ;
                     SENDER ;
                     MEM ;
                     IF { DIG 3 }
                        { DUP 4 ;
                          CDR ;
                          CDR ;
                          DUP 5 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DUP 6 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          PUSH nat 1 ;
                          DUP 8 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          SENDER ;
                          GET ;
                          IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                          ADD ;
                          SOME ;
                          SENDER ;
                          UPDATE ;
                          PAIR ;
                          PAIR ;
                          DIG 4 ;
                          CAR ;
                          PAIR } ;
                     SWAP ;
                     PUSH bool True ;
                     SENDER ;
                     UPDATE ;
                     PAIR } ;
                 DUP ;
                 CAR ;
                 SWAP ;
                 CDR ;
                 DUP ;
                 CDR ;
                 CAR ;
                 CAR ;
                 SENDER ;
                 GET ;
                 IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 CDR ;
                 CAR ;
                 SWAP ;
                 COMPARE ;
                 GT ;
                 IF { PUSH string "Maximum number of proposal reached" ; FAILWITH } {} ;
                 DUP ;
                 CDR ;
                 CDR ;
                 DUP 3 ;
                 SIZE ;
                 COMPARE ;
                 GE ;
                 IF { DUP ;
                      CDR ;
                      SWAP ;
                      DUP ;
                      DUG 2 ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DUP 5 ;
                      NONE (set address) ;
                      SWAP ;
                      UPDATE ;
                      DUP 3 ;
                      CAR ;
                      CDR ;
                      CAR ;
                      PAIR ;
                      DIG 2 ;
                      CAR ;
                      CAR ;
                      PAIR ;
                      PAIR ;
                      DUP ;
                      CDR ;
                      CAR ;
                      CDR ;
                      DIG 4 ;
                      SWAP ;
                      EXEC ;
                      SWAP ;
                      DUP ;
                      DUG 2 ;
                      CDR ;
                      CDR ;
                      DIG 4 ;
                      DUP 4 ;
                      CDR ;
                      CAR ;
                      CDR ;
                      CONCAT ;
                      SHA256 ;
                      DUP 4 ;
                      CDR ;
                      CAR ;
                      CAR ;
                      PAIR ;
                      PAIR ;
                      DIG 2 ;
                      CAR ;
                      PAIR ;
                      DUP ;
                      CDR ;
                      CAR ;
                      CAR ;
                      ITER { SWAP ;
                             PAIR ;
                             DUP ;
                             CAR ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             CDR ;
                             CAR ;
                             DUP 5 ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             MEM ;
                             IF { SWAP ;
                                  DUP ;
                                  DUG 2 ;
                                  CDR ;
                                  CDR ;
                                  DUP 3 ;
                                  CDR ;
                                  CAR ;
                                  CDR ;
                                  DUP 4 ;
                                  CDR ;
                                  CAR ;
                                  CAR ;
                                  PUSH nat 1 ;
                                  DIG 6 ;
                                  CDR ;
                                  CDR ;
                                  SUB ;
                                  ABS ;
                                  DIG 4 ;
                                  SWAP ;
                                  SOME ;
                                  SWAP ;
                                  UPDATE ;
                                  PAIR ;
                                  PAIR ;
                                  SWAP ;
                                  CAR ;
                                  PAIR }
                                { DROP ; SWAP ; DROP } } ;
                      DIG 2 ;
                      DROP ;
                      SWAP ;
                      PAIR }
                    { DIG 3 ;
                      DROP ;
                      DUP ;
                      CDR ;
                      SWAP ;
                      DUP ;
                      DUG 2 ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 3 ;
                      DIG 4 ;
                      SWAP ;
                      SOME ;
                      SWAP ;
                      UPDATE ;
                      DUP 3 ;
                      CAR ;
                      CDR ;
                      CAR ;
                      PAIR ;
                      DIG 2 ;
                      CAR ;
                      CAR ;
                      PAIR ;
                      PAIR ;
                      NIL operation ;
                      PAIR } } }
           { PACK ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CAR ;
             CDR ;
             CDR ;
             SWAP ;
             DUP ;
             DUG 2 ;
             GET ;
             IF_NONE
               { DROP }
               { DUP ;
                 PUSH bool False ;
                 SENDER ;
                 UPDATE ;
                 DUP ;
                 SIZE ;
                 DIG 2 ;
                 SIZE ;
                 COMPARE ;
                 NEQ ;
                 IF { DUP 3 ;
                      CDR ;
                      CDR ;
                      DUP 4 ;
                      CDR ;
                      CAR ;
                      CDR ;
                      DUP 5 ;
                      CDR ;
                      CAR ;
                      CAR ;
                      PUSH nat 1 ;
                      DUP 7 ;
                      CDR ;
                      CAR ;
                      CAR ;
                      SENDER ;
                      GET ;
                      IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                      SUB ;
                      ABS ;
                      SOME ;
                      SENDER ;
                      UPDATE ;
                      PAIR ;
                      PAIR ;
                      DIG 3 ;
                      CAR ;
                      PAIR }
                    { DIG 2 } ;
                 PUSH nat 0 ;
                 DUP 3 ;
                 SIZE ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP ;
                      DROP ;
                      DUP ;
                      CDR ;
                      SWAP ;
                      DUP ;
                      DUG 2 ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 3 ;
                      NONE (set address) ;
                      SWAP ;
                      UPDATE ;
                      DUP 3 ;
                      CAR ;
                      CDR ;
                      CAR ;
                      PAIR ;
                      DIG 2 ;
                      CAR ;
                      CAR ;
                      PAIR ;
                      PAIR }
                    { DUP ;
                      CDR ;
                      SWAP ;
                      DUP ;
                      DUG 2 ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 3 ;
                      DIG 4 ;
                      SWAP ;
                      SOME ;
                      SWAP ;
                      UPDATE ;
                      DUP 3 ;
                      CAR ;
                      CDR ;
                      CAR ;
                      PAIR ;
                      DIG 2 ;
                      CAR ;
                      CAR ;
                      PAIR ;
                      PAIR } } ;
             NIL operation ;
             PAIR } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {|
{ parameter
    (or (pair %reset (pair (timestamp %finish_time) (timestamp %start_time)) (string %title))
        (or %vote (unit %nay) (unit %yea))) ;
  storage
    (pair (pair (pair (timestamp %finish_time) (nat %nay))
                (pair (timestamp %start_time) (string %title)))
          (pair (set %voters address) (nat %yea))) ;
  code { UNPAIR ;
         IF_LEFT
           { SWAP ;
             DROP ;
             PUSH nat 0 ;
             EMPTY_SET address ;
             PAIR ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CDR ;
             DUP 3 ;
             CAR ;
             CDR ;
             PAIR ;
             PUSH nat 0 ;
             DIG 3 ;
             CAR ;
             CAR ;
             PAIR ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR }
           { SENDER ;
             SWAP ;
             IF_LEFT
               { DROP ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 DUP 3 ;
                 CAR ;
                 CDR ;
                 PUSH nat 1 ;
                 DUP 5 ;
                 CAR ;
                 CAR ;
                 CDR ;
                 ADD ;
                 DIG 4 ;
                 CAR ;
                 CAR ;
                 CAR ;
                 PAIR ;
                 PAIR ;
                 PAIR }
               { DROP ;
                 PUSH nat 1 ;
                 DUP 3 ;
                 CDR ;
                 CDR ;
                 ADD ;
                 DUP 3 ;
                 CDR ;
                 CAR ;
                 PAIR ;
                 DIG 2 ;
                 CAR ;
                 PAIR } ;
             DUP ;
             CDR ;
             CDR ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CDR ;
             CAR ;
             DIG 3 ;
             PUSH bool True ;
             SWAP ;
             UPDATE ;
             PAIR ;
             SWAP ;
             CAR ;
             PAIR ;
             NIL operation ;
             PAIR } } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "ticket_wallet.mligo" ; "main" ; "--protocol=edo" ; "--disable-michelson-typechecking" ] ;
  [%expect {|
{ parameter
    (or (ticket %receive unit)
        (pair %send
           (contract %destination (ticket unit))
           (pair (nat %amount) (address %ticketer)))) ;
  storage (pair (address %manager) (big_map %tickets address (ticket unit))) ;
  code { PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         EQ ;
         IF {} { PUSH string "failed assertion" ; FAILWITH } ;
         UNPAIR ;
         SWAP ;
         UNPAIR ;
         DIG 2 ;
         IF_LEFT
           { READ_TICKET ;
             CAR ;
             DIG 3 ;
             NONE (ticket unit) ;
             DUP 3 ;
             GET_AND_UPDATE ;
             IF_NONE
               { DIG 2 }
               { DIG 3 ;
                 PAIR ;
                 JOIN_TICKETS ;
                 IF_NONE { PUSH string "impossible?" ; FAILWITH } {} } ;
             SOME ;
             DIG 2 ;
             GET_AND_UPDATE ;
             DROP ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR }
           { SWAP ;
             DUP ;
             DUG 2 ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DIG 2 ;
             NONE (ticket unit) ;
             DUP 3 ;
             CDR ;
             CDR ;
             GET_AND_UPDATE ;
             IF_NONE
               { DROP 3 ; PUSH string "no tickets" ; FAILWITH }
               { READ_TICKET ;
                 CDR ;
                 CDR ;
                 DUP 4 ;
                 CDR ;
                 CAR ;
                 DUP ;
                 DIG 2 ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "not enough tickets" ; FAILWITH } {} ;
                 SWAP ;
                 PAIR ;
                 SWAP ;
                 SPLIT_TICKET ;
                 IF_NONE
                   { DROP 3 ; PUSH string "impossible?" ; FAILWITH }
                   { UNPAIR ;
                     DUG 2 ;
                     SOME ;
                     DUP 4 ;
                     CDR ;
                     CDR ;
                     GET_AND_UPDATE ;
                     DROP ;
                     DIG 2 ;
                     CAR ;
                     PUSH mutez 0 ;
                     DIG 3 ;
                     TRANSFER_TOKENS ;
                     SWAP ;
                     DIG 2 ;
                     PAIR ;
                     NIL operation ;
                     DIG 2 ;
                     CONS ;
                     PAIR } } } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "ticket_builder.mligo" ; "main" ; "--protocol=edo" ] ;
  [%expect {|
{ parameter
    (or (ticket %burn unit)
        (pair %mint (contract %destination (ticket unit)) (nat %amount))) ;
  storage address ;
  code { PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         EQ ;
         IF {} { PUSH string "failed assertion" ; FAILWITH } ;
         UNPAIR ;
         IF_LEFT
           { READ_TICKET ;
             SWAP ;
             DROP ;
             CAR ;
             SELF_ADDRESS ;
             SWAP ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             NIL operation ;
             PAIR }
           { SWAP ;
             DUP ;
             DUG 2 ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DUP ;
             CAR ;
             PUSH mutez 0 ;
             DIG 2 ;
             CDR ;
             PUSH unit Unit ;
             TICKET ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } } |} ]

let%expect_test _ =
    run_ligo_good [ "compile-contract" ; contract "implicit.mligo" ; "main" ] ;
    [%expect {|
      { parameter key_hash ;
        storage unit ;
        code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "amount_lambda.mligo" ; "main" ] ;
  (* AMOUNT should occur inside the second lambda, but not the first lambda *)
  [%expect {|
    { parameter bool ;
      storage (lambda unit mutez) ;
      code { CAR ;
             IF { AMOUNT ; LAMBDA (pair mutez unit) mutez { CAR } ; SWAP ; APPLY }
                { LAMBDA unit mutez { DROP ; AMOUNT } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-typed" ; contract "sequence.mligo" ; ];
  [%expect {| const y = lambda (#1) return let x = +1 in let _ = let x = +2 in UNIT() in let _ = let x = +23 in UNIT() in let _ = let x = +42 in UNIT() in x |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_type_operator.ligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/bad_type_operator.ligo", line 4, characters 16-29
      3 | type binding is nat * nat
      4 | type storage is map (binding)
      5 |
     Wrong number of arguments for type constant: map expected: 2
    got: 1 |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_address_format.religo" ; "main" ] ;
  [%expect {|
    Error(s) occurred while type checking the contract:
    Ill typed contract:
      1: { parameter int ;
      2:   storage address ;
      3:   code { DROP /* [] */ ; PUSH address "KT1badaddr" ; NIL operation ; PAIR } }
    At line 3 characters 38 to 50, value "KT1badaddr"
    is invalid for type address.
    Invalid contract notation "KT1badaddr" |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_timestamp.ligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/bad_timestamp.ligo", line 7, characters 30-44
      6 |   block {
      7 |     var stamp : timestamp := ("badtimestamp" : timestamp)
      8 |   }

    Ill-formed timestamp "badtimestamp".
    At this point, a string with a RFC3339 notation or the number of seconds since Epoch is expected. |}]

let%expect_test _ =
    run_ligo_good [ "dry-run" ; contract "redeclaration.ligo" ; "main" ; "unit" ; "0" ] ;
    [%expect {|( LIST_EMPTY() , 0 ) |}]

let%expect_test _ =
    run_ligo_good [ "dry-run" ; contract "double_main.ligo" ; "main" ; "unit" ; "0" ] ;
    [%expect {|( LIST_EMPTY() , 2 ) |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "subtle_nontail_fail.mligo" ; "main" ] ;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH bool True ;
             IF { PUSH string "This contract always fails" ; FAILWITH }
                { PUSH string "This contract still always fails" ; FAILWITH } } } |}]

let%expect_test _ =
  (* TODO should not be bad? *)
  run_ligo_good [ "dry-run" ; contract "subtle_nontail_fail.mligo" ; "main" ; "()" ; "()" ] ;
  [%expect {|
    failwith("This contract always fails") |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "self_in_lambda.mligo" ; "main" ] ;
  [%expect {|
    "Tezos.self_address" must be used directly and cannot be used via another function. |}]

let%expect_test _ =
  run_ligo_good [ "compile-storage" ; contract "big_map.ligo" ; "main" ; "(big_map1,unit)" ] ;
  [%expect {|
    (Pair { Elt 23 0 ; Elt 42 0 } Unit) |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "key_hash_comparable.ligo" ; "main" ] ;
  [%expect {|
    { parameter int ;
      storage (pair (map %one key_hash nat) (big_map %two key_hash bool)) ;
      code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "long_sum_type_names.ligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/long_sum_type_names.ligo", line 2, character 2 to line 4, character 18
      1 | type action is
      2 | | Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt of int
      3 | // | Increment of int
      4 | | Decrement of int
      5 |

    Ill-formed data constructor "Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt".
    Data constructors have a maximum length of 32 characters, which is a limitation imposed by annotations in Tezos. |}]

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "super-counter.mligo" ; "main" ; "test_param" ; "test_storage" ] ;
  [%expect {|
    ( LIST_EMPTY() , 3 ) |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "redundant_constructors.mligo" ; "main" ] ;
  [%expect{|
    in file "../../test/contracts/negative/redundant_constructors.mligo", line 7, character 2 to line 9, character 15
      6 | type union_b =
      7 | | Add of nat
      8 | | Remove of nat
      9 | | Config of nat
     10 |

    Invalid variant.
    Constructor "Add" already exists as part of another variant. |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "create_contract_toplevel.mligo" ; "main" ] ;
  [%expect {|
in file "../../test/contracts/negative/create_contract_toplevel.mligo", line 4, character 35 to line 8, character 8
  3 | let main (action, store : string * string) : return =
  4 |   let toto : operation * address = Tezos.create_contract
  5 |     (fun (p, s : nat * string) -> (([] : operation list), store))
  6 |     (None: key_hash option)
  7 |     300tz
  8 |     "un"
  9 |   in

Free variable 'store' is not allowed in CREATE_CONTRACT lambda |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "create_contract_var.mligo" ; "main" ] ;
  [%expect {|
in file "../../test/contracts/negative/create_contract_var.mligo", line 6, character 35 to line 10, character 5
  5 | let main (action, store : string * string) : return =
  6 |   let toto : operation * address = Tezos.create_contract
  7 |     (fun (p, s : nat * int) -> (([] : operation list), a))
  8 |     (None: key_hash option)
  9 |     300tz
 10 |     1
 11 |   in

Free variable 'a' is not allowed in CREATE_CONTRACT lambda |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "create_contract_no_inline.mligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/create_contract_no_inline.mligo", line 3, characters 40-46
      2 |
      3 | let dummy_contract (p, s : nat * int) : return =
      4 |  (([] : operation list), foo)

    Type "return" not found. |}] ;

  run_ligo_good [ "compile-contract" ; contract "create_contract.mligo" ; "main" ] ;
  [%expect {|
    { parameter string ;
      storage string ;
      code { CDR ;
             PUSH string "un" ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage string ;
                 code { DROP ; PUSH string "one" ; NIL operation ; PAIR } } ;
             PAIR ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CAR ;
             CONS ;
             PAIR } } |}];

  run_ligo_good [ "compile-contract" ; contract "tuples_no_annotation.religo" ; "main" ] ;
  [%expect {|
    { parameter int ;
      storage (pair (pair int string) (pair nat bool)) ;
      code { DROP ;
             PUSH bool False ;
             PUSH nat 2 ;
             PAIR ;
             PUSH string "2" ;
             PUSH int 2 ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "self_type_annotation.ligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/self_type_annotation.ligo", line 8, characters 41-64
      7 |   block {
      8 |     const self_contract: contract(int) = Tezos.self ("%default");
      9 |   }

    Invalid type annotation.
    "contract (nat)" was given, but "contract (int)" was expected.
    Note that "Tezos.self" refers to this contract, so the parameters should be the same. |}] ;

  run_ligo_good [ "compile-contract" ; contract "self_type_annotation.ligo" ; "main" ] ;
  [%expect {|
    { parameter nat ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_contract.mligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/bad_contract.mligo", line 4, characters 9-46
      3 |
      4 | let main (action, store : parameter * storage) : storage =
      5 |   store + 1

    Invalid type for entrypoint "main".
    An entrypoint must of type "parameter * storage -> operations list * storage". |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "bad_contract2.mligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/bad_contract2.mligo", line 5, characters 9-46
      4 |
      5 | let main (action, store : parameter * storage) : return =
      6 |   ("bad",store + 1)

    Invalid type for entrypoint "main".
    An entrypoint must of type "parameter * storage -> operations list * storage". |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "bad_contract3.mligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/bad_contract3.mligo", line 5, characters 9-46
      4 |
      5 | let main (action, store : parameter * storage) : return =
      6 |   (([]: operation list),"bad")

    Invalid type for entrypoint "main".
    The storage type "int" of the function parameter must be the same as the storage type "string" of the return value. |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "self_with_entrypoint.ligo" ; "main" ] ;
  [%expect {|
    { parameter (or (unit %default) (int %toto)) ;
      storage nat ;
      code { CDR ;
             SELF %toto ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}] ;

  run_ligo_good [ "compile-contract" ; contract "self_without_entrypoint.ligo" ; "main" ] ;
  [%expect {|
    { parameter int ;
      storage nat ;
      code { CDR ;
             SELF %default ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "self_bad_entrypoint_format.ligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/self_bad_entrypoint_format.ligo", line 8, characters 52-58
      7 |   block {
      8 |     const self_contract: contract(int) = Tezos.self("Toto") ;
      9 |     const op : operation = Tezos.transaction (2, 300tz, self_contract) ;

    Invalid entrypoint "Toto".
    One of the following patterns is expected:
      * "%bar" is expected for entrypoint "Bar"
      * "%default" when no entrypoint is used. |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_1.religo"; "main"];
  [%expect {|
    in file "../../test/contracts/negative/nested_bigmap_1.religo", line 1, characters 11-29
      1 | type bar = big_map (nat, int);
      2 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_2.religo"; "main"];
  [%expect {|
    in file "../../test/contracts/negative/nested_bigmap_2.religo", line 2, characters 29-50
      1 | /* this should result in an error as nested big_maps are not supported: */
      2 | type storage = big_map (nat, big_map (int, string));
      3 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_3.religo"; "main"];
  [%expect {|
    in file "../../test/contracts/negative/nested_bigmap_3.religo", line 1, characters 11-29
      1 | type bar = big_map (nat, int);
      2 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_4.religo"; "main"];
  [%expect {|
    in file "../../test/contracts/negative/nested_bigmap_4.religo", line 2, characters 25-61
      1 | /* this should result in an error as nested big_maps are not supported: */
      2 | type storage = map (int, big_map (nat, big_map (int, string)));
      3 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_good ["print-ast"; contract "letin.mligo"];
  [%expect {|
type storage = (int , int)
const main : (int , storage) -> (list (operation) , storage) =
  lambda (n : (int , storage)) : (list (operation) ,
  storage) return let x : (int , int) =
                    let x : int = 7 in (ADD(x , n.0) , ADD(n.1.0 , n.1.1)) in
                  (list[] : list (operation) , x)
const f0 = lambda (a : string) return true(unit)
const f1 = lambda (a : string) return true(unit)
const f2 = lambda (a : string) return true(unit)
const letin_nesting =
  lambda (#1 : unit) return let s = "test" in
                            let p0 = (f0)@(s) in { ASSERTION(p0);
 let p1 = (f1)@(s) in { ASSERTION(p1);
 let p2 = (f2)@(s) in { ASSERTION(p2);
 s}}}
const letin_nesting2 =
  lambda (x : int) return let y = 2 in let z = 3 in ADD(ADD(x , y) , z)
const x = match (+1 , (+2 , +3)) with (#2 , #4) -> match #4 with (x ,
  #3) -> x
    |}];

  run_ligo_good ["print-ast"; contract "letin.religo"];
  [%expect {|
type storage = (int , int)
const main = lambda (n : (int , storage)) : (list (operation) ,
  storage) return let x : (int , int) =
                    let x : int = 7 in (ADD(x , n.0) , ADD(n.1.0 , n.1.1)) in
                  (list[] : list (operation) , x)
const f0 = lambda (a : string) return true(unit)
const f1 = lambda (a : string) return true(unit)
const f2 = lambda (a : string) return true(unit)
const letin_nesting =
  lambda (#1 : unit) return let s = "test" in
                            let p0 = (f0)@(s) in { ASSERTION(p0);
 let p1 = (f1)@(s) in { ASSERTION(p1);
 let p2 = (f2)@(s) in { ASSERTION(p2);
 s}}}
const letin_nesting2 =
  lambda (x : int) return let y = 2 in let z = 3 in ADD(ADD(x , y) , z)
const x = match (+1 , (+2 , +3)) with (#2 , #4) -> match #4 with (x ,
  #3) -> x
    |}];

  run_ligo_bad ["print-ast-typed"; contract "existential.mligo"];
  [%expect {|
    in file "../../test/contracts/existential.mligo", line 1, characters 8-9
      1 | let a : 'a = 2
      2 | let b : _ ->'b = fun _ -> 2
    Unexpected character '\''. |}];
  run_ligo_bad ["print-ast-typed"; bad_contract "missing_funarg_annotation.mligo"];
  [%expect {|
    in file "../../test/contracts/negative/missing_funarg_annotation.mligo", line 2, characters 6-7
      1 | (* these should give a missing type annotation error *)
      2 | let a b = b
      3 | let a (b,c) = b

    Missing a type annotation for argument "b". |}];
  run_ligo_bad ["print-ast-typed"; bad_contract "missing_funarg_annotation.religo"];
  [%expect {|
in file "../../test/contracts/negative/missing_funarg_annotation.religo", line 2, characters 8-9
  1 | /* these should give a missing type annotation error */
  2 | let a = b => b
  3 | let a = (b,c) => b

Missing a type annotation for argument "b". |}];
  run_ligo_bad ["print-ast-typed"; bad_contract "funarg_tuple_wrong.mligo"];
  [%expect {|
    in file "../../test/contracts/negative/funarg_tuple_wrong.mligo", line 1, characters 7-14
      1 | let a (b, c, d: int * int) = d
      2 | let a (((b, c, d)): ((((int))) * int)) = d

    The tuple "b, c, d" does not match the type "int * int". |}];
  run_ligo_bad ["print-ast-typed"; bad_contract "funarg_tuple_wrong.religo"];
  [%expect {|
    Pattern do not match returned expression. |}];

  run_ligo_bad [ "compile-contract" ; bad_contract "duplicate_record_field.mligo" ; "main" ] ;
  [%expect {|
    Duplicate field name "foo" in this record declaration.
    Hint: Change the name. |}];

  ()

(* uncurrying example *)
let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "uncurry_contract.mligo" ; "main" ] ;
  let output = [%expect.output] in
  let lines = String.split_on_char '\n' output in
  let lines = List.take 3 lines in
  let output = String.concat "\n" lines in
  print_string output;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { LAMBDA (pair unit (pair unit (pair unit unit))) unit { DROP ; PUSH unit Unit } ; |}]
