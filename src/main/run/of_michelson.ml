open Proto_alpha_utils
open Trace
open Memory_proto_alpha.Protocol.Script_ir_translator
open Memory_proto_alpha.X
open Simple_utils.Runned_result

module Errors = Main_errors

type options = Memory_proto_alpha.options

type dry_run_options =
  { parameter_ty : (Location.t, string) Tezos_micheline.Micheline.node option ; (* added to allow dry-running contract using `Tezos.self` *)
    amount : string ;
    balance : string ;
    now : string option ;
    sender : string option ;
    source : string option }

let make_dry_run_options (opts : dry_run_options) : (options , _) result =
  let open Proto_alpha_utils.Trace in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let open Protocol.Alpha_context in
  let* balance = match Tez.of_string opts.balance with
    | None -> fail @@ Errors.invalid_balance opts.balance
    | Some balance -> ok balance in
  let* amount = match Tez.of_string opts.amount with
    | None -> fail @@ Errors.invalid_amount opts.amount
    | Some amount -> ok amount in
  let* sender =
    match opts.sender with
    | None -> ok None
    | Some sender ->
      let* sender =
        trace_alpha_tzresult
          (fun _ -> Errors.invalid_sender sender)
          (Contract.of_b58check sender) in
      ok (Some sender) in
  let* source =
    match opts.source with
    | None -> ok None
    | Some source ->
      let* source =
        trace_alpha_tzresult
          (fun _ -> Errors.invalid_source source)
          (Contract.of_b58check source) in
      ok (Some source) in
  let* now =
    match opts.now with
    | None -> ok None
    | Some st ->
      match Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.of_string st with
        | Some t -> ok (Some t)
        | None -> fail @@ Errors.invalid_timestamp st in
  let* parameter_ty =
    match opts.parameter_ty with
    | Some x ->
      let* x = Trace.trace_tzresult_lwt Errors.parsing_payload_tracer @@ Memory_proto_alpha.prims_of_strings x in
      let x = Tezos_micheline.Micheline.strip_locations x in
      ok (Some x)
    | None -> ok None
  in
  ok @@ make_options ?now:now ~amount ~balance ?sender ?source ?parameter_ty ()

let ex_value_ty_to_michelson (v : ex_typed_value) : (_ Michelson.t * _ Michelson.t , _) result =
  let (Ex_typed_value (ty , value)) = v in
  let* ty' =
    Trace.trace_tzresult_lwt Errors.unparsing_michelson_tracer @@
    Memory_proto_alpha.unparse_michelson_ty ty in
  let* value' =
    Trace.trace_tzresult_lwt Errors.unparsing_michelson_tracer @@
    Memory_proto_alpha.unparse_michelson_data ty value in
  ok (ty', value')

let pack_payload (payload : _ Michelson.t) ty =
  let* ty =
    Trace.trace_tzresult_lwt Errors.parsing_payload_tracer @@
    Memory_proto_alpha.prims_of_strings ty in
  let* (Ex_ty ty) =
    Trace.trace_tzresult_lwt Errors.parsing_payload_tracer @@
    Memory_proto_alpha.parse_michelson_ty ty in
  let* payload =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings payload in
  let* payload =
    Trace.trace_tzresult_lwt Errors.parsing_payload_tracer @@
    Memory_proto_alpha.parse_michelson_data payload ty in
  let* data =
    Trace.trace_tzresult_lwt Errors.packing_payload_tracer @@
    Memory_proto_alpha.pack ty payload in
  ok @@ data

let fetch_lambda_types (contract_ty : _ Michelson.t) =
  match contract_ty with
  | Prim (_, "lambda", [in_ty; out_ty], _) -> ok (in_ty, out_ty)
  | _ -> fail Errors.unknown (*TODO*)

let run_contract ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) (input_michelson : _ Michelson.t) =
  let open! Tezos_raw_protocol_008_PtEdo2Zk in
  let* (input_ty, output_ty) = fetch_lambda_types exp_type in
  let* input_ty =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings input_ty in
  let (param_type, storage_type) =
    match input_ty with
    | Prim (_, T_pair, [x; y], _) -> (x, y)
    | _ -> failwith ("Internal error: input_ty was not a pair " ^ __LOC__) in
  let* (Ex_ty input_ty) =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty input_ty in
  let* (Ex_ty param_type) =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty param_type in
  let* (Ex_ty storage_type) =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty storage_type in
  let* output_ty =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings output_ty in
  let* (Ex_ty output_ty) =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty output_ty in
  let* input_michelson =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings input_michelson in
  let* input =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_data input_michelson input_ty
  in
  let top_level = Script_ir_translator.Toplevel
    { storage_type ; param_type ;
      root_name = None ; legacy_create_contract_literal = false } in
  let ty_stack_before = Script_typed_ir.Item_t (input_ty, Empty_t, None) in
  let ty_stack_after = Script_typed_ir.Item_t (output_ty, Empty_t, None) in
  let* (descr : (_*unit,_*unit) Script_typed_ir.descr) =
    Trace.trace_tzresult_lwt Errors.parsing_code_tracer @@
    Memory_proto_alpha.parse_michelson_fail ~top_level exp ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let* res =
    Trace.trace_tzresult_lwt Errors.error_of_execution_tracer @@
    Memory_proto_alpha.failure_interpret ?options descr (input, ()) in
  match res with
  | Memory_proto_alpha.Succeed stack ->
    let (output, ()) = stack in
    let* (ty, value) = ex_value_ty_to_michelson (Ex_typed_value (output_ty, output)) in
    ok @@ Success (ty, value)
  | Memory_proto_alpha.Fail expr -> ( match Tezos_micheline.Micheline.root @@ Memory_proto_alpha.strings_of_prims expr with
    | Int (_ , i)    -> ok @@ Fail (Failwith_int (Z.to_int i))
    | String (_ , s) -> ok @@ Fail (Failwith_string s)
    | Bytes (_, s)   -> ok @@ Fail (Failwith_bytes s)
    | _              -> fail @@ Errors.unknown_failwith_type )

let run_expression ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) =
  let open! Tezos_raw_protocol_008_PtEdo2Zk in
  let* exp_type =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings exp_type in
  let* (Ex_ty exp_type') =
    Trace.trace_tzresult_lwt Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty exp_type in
  let top_level = Script_ir_translator.Lambda
  and ty_stack_before = Script_typed_ir.Empty_t
  and ty_stack_after = Script_typed_ir.Item_t (exp_type', Empty_t, None) in
  let* descr =
    Trace.trace_tzresult_lwt Errors.parsing_code_tracer @@
    Memory_proto_alpha.parse_michelson_fail ~top_level exp ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let* res =
    Trace.trace_tzresult_lwt Errors.error_of_execution_tracer @@
    Memory_proto_alpha.failure_interpret ?options descr () in
  match res with
  | Memory_proto_alpha.Succeed stack ->
    let (output, ()) = stack in
    let* (ty, value) = ex_value_ty_to_michelson (Ex_typed_value (exp_type', output)) in
    ok @@ Success (ty, value)
  | Memory_proto_alpha.Fail expr -> ( match Tezos_micheline.Micheline.root @@ Memory_proto_alpha.strings_of_prims expr with
    | Int (_ , i)    -> ok @@ Fail (Failwith_int (Z.to_int i))
    | String (_ , s) -> ok @@ Fail (Failwith_string s)
    | Bytes (_, s)   -> ok @@ Fail (Failwith_bytes s)
    | _              -> fail @@ Errors.unknown_failwith_type )

let run_failwith ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) : (failwith , _) result =
  let* expr = run_expression ?options exp exp_type in
  match expr with
  | Success _  -> fail Errors.unknown (* TODO : simple_fail "an error of execution was expected" *)
  | Fail res -> ok res

let run_no_failwith ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) =
  let* expr = run_expression ?options exp exp_type in
  match expr with
  | Success tval  -> ok tval
  | Fail _ -> fail Errors.unknown (* TODO : simple_fail "unexpected error of execution" *)

let evaluate_expression ?options exp exp_type =
  let* etv = run_expression ?options exp exp_type in
  match etv with
    | Success (_, value) -> ok value
    | Fail res -> fail @@ Errors.main_failwith res
