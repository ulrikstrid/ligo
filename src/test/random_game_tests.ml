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


(* Test that contract fails if we try to commit past the commit stage *)
let commit_late () =
  let%bind program,_ = get_program () in
  let exp_failwith = "This game has passed the commit stage." in
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
  let parameter = e_bytes_raw packed_param in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "commit"
    (e_pair parameter storage)
    exp_failwith

let main = test_suite "Random Game" [
    test "commit (fails once commit stage passed)" commit_late;
]
