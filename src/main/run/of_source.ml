open Trace

let compile_file_contract_parameter : string -> string -> string -> Compile.Helpers.s_syntax -> Michelson.t result =
  fun source_filename _entry_point expression syntax ->
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let env = Ast_typed.program_environment program in
  let%bind syntax = Compile.Helpers.syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = Compile.Helpers.parsify_expression syntax expression in
  Of_simplified.compile_expression simplified ~env ~state


let compile_expression : string -> Typer.Solver.state -> Compile.Helpers.s_syntax -> Michelson.t result =
  fun expression state syntax ->
  let%bind syntax = Compile.Helpers.syntax_to_variant syntax None in
  let%bind simplified = Compile.Helpers.parsify_expression syntax expression in
  Of_simplified.compile_expression ~state simplified

let compile_file_contract_storage : string -> string -> string -> Compile.Helpers.s_syntax -> Michelson.t result =
  fun source_filename _entry_point expression syntax ->
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let env = Ast_typed.program_environment program in
  let%bind syntax = Compile.Helpers.syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = Compile.Helpers.parsify_expression syntax expression in
  Of_simplified.compile_expression simplified ~env ~state

let compile_file_contract_args ~env ~state : string -> string -> Compile.Helpers.v_syntax -> Michelson.t result =
    fun storage parameter syntax ->
  let%bind storage_simplified = Compile.Helpers.parsify_expression syntax storage in
  let%bind parameter_simplified = Compile.Helpers.parsify_expression syntax parameter in
  let args = Ast_simplified.e_pair storage_simplified parameter_simplified in
  Of_simplified.compile_expression args ~env ~state

let compile_file_expression ~env ~state : string -> Compile.Helpers.v_syntax -> Michelson.t result =
    fun expression syntax ->
  let%bind simplified = Compile.Helpers.parsify_expression syntax expression in
  Of_simplified.compile_expression simplified ~env ~state

type dry_run_options =
  { amount : string ;
    sender : string option ;
    source : string option }

let make_dry_run_options (opts : dry_run_options) : Of_michelson.options result =
  let open Proto_alpha_utils.Trace in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let open Protocol.Alpha_context in
  let%bind amount = match Tez.of_string opts.amount with
    | None -> simple_fail "invalid amount"
    | Some amount -> ok amount in
  let%bind sender =
    match opts.sender with
    | None -> ok None
    | Some sender ->
      let%bind sender =
        trace_alpha_tzresult
          (simple_error "invalid address")
          (Contract.of_b58check sender) in
      ok (Some sender) in
  let%bind source =
    match opts.source with
    | None -> ok None
    | Some source ->
      let%bind source =
        trace_alpha_tzresult
          (simple_error "invalid source address")
          (Contract.of_b58check source) in
      ok (Some source) in
  ok @@ make_options ~amount ?source:sender ?payer:source ()

let type_ligo_code syntax source_filename =
  let%bind (typed_program , state) = Compile.Of_source.type_file syntax source_filename in
  let%bind syntax = Compile.Helpers.syntax_to_variant syntax (Some source_filename) in
  let env = Ast_typed.program_environment typed_program in
  ok @@ (typed_program,state,syntax,env)

let run ~options typed_program entry_point args =
  let%bind code = Compile.Of_typed.compile_function_entry typed_program entry_point in
  let%bind options = make_dry_run_options options in
  let%bind ex_value_ty = Of_michelson.run ~options code args in
  Compile.Of_simplified.uncompile_typed_program_entry_function_result typed_program entry_point ex_value_ty


let run_contract ~options source_filename entry_point storage parameter syntax =
  let%bind (typed_program, state, syntax, env) = type_ligo_code syntax source_filename in
  let%bind args = compile_file_contract_args ~env ~state storage parameter syntax in
  run ~options typed_program entry_point args 

let run_function_entry ~options source_filename entry_point input syntax =
  let%bind (typed_program, state, syntax, env) = type_ligo_code syntax source_filename in
  let%bind args = compile_file_expression ~env ~state input syntax in
  run ~options typed_program entry_point args 




let evaluate_entry ~options source_filename entry_point syntax =
  let%bind (program , state) = Compile.Of_source.type_file syntax source_filename in
  let () = Typer.Solver.discard_state state in
  let%bind code = Compile.Of_typed.compile_expression_as_function_entry program entry_point in
  let%bind options = make_dry_run_options options in
  let%bind ex_value_ty = Of_michelson.evaluate ~options code in
  Compile.Of_simplified.uncompile_typed_program_entry_expression_result program entry_point ex_value_ty

let evaluate_michelson expression syntax =
  let%bind code = Compile.Of_source.compile_expression_as_function expression syntax in
  Of_michelson.evaluate_michelson code
