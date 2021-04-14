open Trace
open Test_helpers
open Ast_imperative
open Main_errors

let type_file ~options f = Ligo_compile.Utils.type_file ~options f "cameligo" (Contract "main")
let options = Compiler_options.make ()

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
      let%bind program = type_file ~options "./contracts/timelock_repeat.mligo" in
      s := Some program ;
      ok program
    )

let compile_main () =
  let%bind typed_prg,_   = type_file ~options "./contracts/timelock_repeat.mligo" in
  let%bind mini_c_prg      = Ligo_compile.Of_typed.compile typed_prg in
  let%bind michelson_prg   = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~options mini_c_prg "main" in
  let%bind _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract michelson_prg in
  ok ()

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda_ez (Location.wrap @@ Var.of_name "arguments")
  ~ascr:(t_unit ()) (Some (t_list (t_operation ())))
  empty_op_list

let call msg = e_constructor "Call" msg
let mk_time st =
  match Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.of_string st with
  | Some s -> ok s
  | None -> fail @@ test_internal "bad timestamp notation"
let to_sec t = Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.to_zint t
let storage st interval execute =
  e_record_ez [("next_use", e_timestamp_z (to_sec st)) ;
               ("interval", e_int interval) ;
               ("execute", execute)]

let early_call () =
  let%bind (program, env) = get_program () in
  let%bind now = mk_time "2000-01-01T00:10:10Z" in
  let%bind lock_time = mk_time "2000-01-01T10:10:10Z" in
  let init_storage = storage lock_time 86400 empty_message in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options ~now () in
  let exp_failwith = "You have to wait before you can execute this contract again." in
  expect_string_failwith ~options (program, env) "main"
    (e_pair (e_unit ())  init_storage) exp_failwith

let fake_decompiled_empty_message = e_string "[lambda of type: (lambda %execute unit (list operation)) ]"

(* Test that when we use the contract the next use time advances by correct interval *)
let interval_advance () =
  let%bind (program, env) = get_program () in
  let%bind now = mk_time "2000-01-01T10:10:10Z" in
  let%bind lock_time = mk_time "2000-01-01T00:10:10Z" in
  let init_storage = storage lock_time 86400 empty_message in
  let%bind new_timestamp = mk_time "2000-01-02T10:10:10Z" in
  let new_storage_fake = storage new_timestamp 86400 fake_decompiled_empty_message in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options ~now () in
  expect_eq ~options (program, env) "main"
  (e_pair (e_unit ()) init_storage) (e_pair empty_op_list new_storage_fake)

let main = test_suite "Time Lock Repeating" [
    test "compile" compile_main ;
    test "early call" early_call ;
    test "interval advance" interval_advance ;
  ]
