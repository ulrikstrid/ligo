open Trace
open Test_helpers

let type_file f = 
  let%bind typed,state = Ligo.Compile.Utils.type_file f "cameligo" (Contract "main") in
  ok @@ (typed,state)

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/random.mligo" in
        s := Some program ;
        ok program
      )

let compile_main () = 
  let%bind typed_prg,_     = get_program () in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

open Ast_imperative

let mk_time st =
  match Memory_proto_alpha.Protocol.Alpha_context.Timestamp.of_notation st with
  | Some s -> ok s
  | None -> simple_fail "bad timestamp notation"
let to_sec t = Tezos_utils.Time.Protocol.to_seconds t

let (first_committer , first_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let empty_op_list = 
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda (Var.of_name "arguments")
  (Some (t_bytes ())) (Some (t_list (t_operation ())))
  empty_op_list

let commit () = 
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:01:10Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let commits = e_typed_map [] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let%bind commit = e_bytes_hex "bcd170047ec1addde872e946f1bc86a5cf6484d75126cb5fd2125571f1534c79" in
  let new_commits = e_typed_map [(e_address first_committer, commit)] (t_address ()) (t_bytes ()) in
  let new_storage = e_record_ez [("commits", new_commits);
                                 ("reveals", reveals);
                                 ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                                 ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                                 ("min_players", min_players)] in
  let%bind packed_param = pack_payload program (e_nat 1000) in
  let parameter = e_bytes_raw @@ sha_256_hash packed_param in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_eq ~options program "commit"
    (e_pair parameter storage)
    (e_pair (e_list []) new_storage)


(* Test that contract fails if we try to commit past the commit stage *)
let commit_late () =
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-02T00:10:10Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let commits = e_typed_map [] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let%bind packed_param = pack_payload program (e_nat 1000) in
  let parameter = e_bytes_raw @@ sha_256_hash packed_param in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "commit"
    (e_pair parameter storage)
    "This game has passed the commit stage."

let reveal () = 
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-02T00:00:00Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let%bind commit = e_bytes_hex "bcd170047ec1addde872e946f1bc86a5cf6484d75126cb5fd2125571f1534c79" in
  let commits = e_typed_map [(e_address first_committer, commit)] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let new_reveals = e_typed_map [(e_address first_committer, e_nat 1000)] (t_address ()) (t_bytes ()) in
  let new_storage = e_record_ez [("commits", commits);
                                 ("reveals", new_reveals);
                                 ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                                 ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                                 ("min_players", min_players)] in
  let parameter = e_nat 1000 in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ()
  in
  expect_eq ~options program "reveal"
    (e_pair parameter storage)
    (e_pair (e_list []) new_storage)

let reveal_not_commit () = 
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-02T00:00:00Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let%bind commit = e_bytes_hex "0500a80f" in
  let commits = e_typed_map [(e_address (addr 5), commit)] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let parameter = e_nat 1000 in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "reveal"
    (e_pair parameter storage)
    "You didn't commit a hash in the 1st round."

let reveal_commit_overflow () = 
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-02T00:00:00Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let%bind commit = e_bytes_hex "0500a80f" in
  let commits = e_typed_map [(e_address first_committer, commit)] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let parameter = e_nat_z (Z.of_string "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999") in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "reveal"
    (e_pair parameter storage)
    "Your commitment was over the bound of 115792089237316195423570985008687907853269984665640564039457584007913129639936"

let reveal_dont_match () = 
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-02T00:00:00Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let%bind commit = e_bytes_hex "0500a80f" in
  let commits = e_typed_map [(e_address first_committer, commit)] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let parameter = e_nat 100 in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "reveal"
    (e_pair parameter storage)
    "The submitted bytes don't match your commitment."

(* Test that contract fails if we try to reveal before the reveal stage *)
let reveal_early () =
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:10:10Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let commits = e_typed_map [] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let parameter = e_nat 1000 in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "reveal"
    (e_pair parameter storage)
    "This game is not in the reveal stage."

(* Test that contract fails if we try to reveal past the reveal stage *)
let reveal_late () =
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-04T00:10:10Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let commits = e_typed_map [] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let parameter = e_nat 1000 in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "reveal"
    (e_pair parameter storage)
    "This game is not in the reveal stage."

let result () = 
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-04T00:10:20Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 1 in
  let%bind commit = e_bytes_hex "0500a80f" in
  let commits = e_typed_map [(e_address first_committer, commit)] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [(e_address first_committer, e_nat 1000)] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let parameter = e_unit () in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ~source:first_contract
      ()
  in
  expect_eq ~options program "result"
    (e_pair parameter storage)
    (e_pair (e_list []) storage)


let result_early () = 
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-02T00:10:11Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let%bind commit = e_bytes_hex "0500a80f" in
  let commits = e_typed_map [(e_address first_committer, commit)] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [(e_address first_committer, e_nat 1000)] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let parameter = e_unit () in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ~source:first_contract
      ()
  in
  expect_string_failwith ~options program "result"
    (e_pair parameter storage)
    "The reveal stage has not finished yet."

let result_not_enough_player () = 
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-04T00:10:11Z" in
  let%bind end_phase_one = mk_time "2000-01-01T00:10:11Z" in
  let%bind end_phase_two = mk_time "2000-01-03T00:10:11Z" in
  let min_players = e_nat 3 in
  let%bind commit = e_bytes_hex "0500a80f" in
  let commits = e_typed_map [(e_address first_committer, commit)] (t_address ()) (t_bytes ()) in
  let reveals = e_typed_map [(e_address first_committer, e_nat 1000)] (t_address ()) (t_nat ()) in
  let storage = e_record_ez [("commits", commits);
                             ("reveals", reveals);
                             ("end_phase_one", e_timestamp (Int64.to_int (to_sec end_phase_one)));
                             ("end_phase_two", e_timestamp (Int64.to_int (to_sec end_phase_two)));
                             ("min_players", min_players)] in
  let parameter = e_unit () in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ~source:first_contract
      ()
  in
  expect_string_failwith ~options program "result"
    (e_pair parameter storage)
    "Minimum player threshold not met for this game."

let main = test_suite "Random Game" [
    test "commit" commit;
    test "commit (fails once commit stage passed)" commit_late;
    test "reveal" reveal;
    test "reveal (fails for commitment overflow" reveal_commit_overflow;
    test "reveal (fails for not commiting befre)" reveal_not_commit;
    test "reveal (fails for reveal don't match commit)" reveal_dont_match;
    test "reveal (fails before reveal stage start)" reveal_early;
    test "reveal (fails once reveal stage passed)" reveal_late;
    test "result" result;
    test "result (fails before result stage start" result_early;
    test "result (fails for not enough player)" result_not_enough_player;
]
