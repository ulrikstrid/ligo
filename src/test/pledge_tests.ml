open Trace
open Test_helpers
open Ast_imperative

let retype_file f = Ligo_compile.Utils.type_file ~options f "reasonligo" Env

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
      let%bind program = retype_file "./contracts/pledge.religo" in
      s := Some program ;
      ok program
    )

let compile_main () =
  let%bind typed_prg,_    = get_program () in
  let%bind mini_c_prg     = Ligo_compile.Of_typed.compile typed_prg in
  let%bind michelson_prg  = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~options mini_c_prg "main" in
  let%bind _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract michelson_prg in
  ok ()

let (oracle_addr , oracle_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let (stranger_addr , stranger_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 1 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda_ez (Location.wrap @@ Var.of_name "arguments")
  ~ascr:(t_unit ()) (Some (t_list (t_operation ())))
  empty_op_list


let pledge () =
  let%bind (program,env) = get_program () in
  let storage = e_address oracle_addr in
  let parameter = e_unit () in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
                  ~sender:oracle_contract
                  ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  expect_eq ~options (program,env) "donate"
    (e_pair parameter storage)
    (e_pair (e_list []) storage)

let distribute () =
  let%bind (program,env) = get_program () in
  let storage = e_address oracle_addr in
  let parameter =  empty_message in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
                  ~sender:oracle_contract ()
  in
  expect_eq ~options (program,env) "distribute"
    (e_pair parameter storage)
    (e_pair (e_list []) storage)

let distribute_unauthorized () =
  let%bind (program,env) = get_program () in
  let storage = e_address oracle_addr in
  let parameter =  empty_message in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
                  ~sender:stranger_contract ()
  in
  expect_string_failwith ~options (program,env) "distribute"
    (e_pair parameter storage)
    "You're not the oracle for this distribution."

let main = test_suite "Pledge & Distribute" [
    test "donate" pledge ;
    test "distribute" distribute ;
    test "distribute (unauthorized)" distribute_unauthorized ;
]
