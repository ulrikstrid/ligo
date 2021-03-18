open Display

let rec error_ppformat : display_format:string display_format ->
  Format.formatter -> Types.all -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Build_error_tracer err -> error_ppformat ~display_format f err
    | `Build_dependency_cycle trace ->
      Format.fprintf f "@[<hv>Dependency cycle detected :@, %s@]" trace
    | `Build_corner_case (loc,msg) ->
      Format.fprintf f "[@<hv>Building corner case at %s : %s@]" loc msg
    | `Test_err_tracer (name,err) ->
      Format.fprintf f "@[<hv>Test '%s'@ %a@]"
        name (error_ppformat ~display_format) err
    | `Test_run_tracer (ep, err) ->
      Format.fprintf f "@[<hv>Running entrypoint '%s'@ %a@]"
        ep (error_ppformat ~display_format) err
    | `Test_expect_tracer (expected, actual) ->
      Format.fprintf f "@[<hv>Expected:@ %a@ got:@ %a@]"
        Ast_core.PP.expression expected
        Ast_core.PP.expression actual
    | `Test_expect_n_tracer (i,err) ->
      Format.fprintf f "@[<hv>Expect n=%d@ %a@]"
        i (error_ppformat ~display_format) err
    | `Test_expect_exp_tracer (e,err) ->
      Format.fprintf f "@[<hv>Expect %a@ %a@]"
        Ast_core.PP.expression e
        (error_ppformat ~display_format) err
    | `Test_expect_eq_n_tracer (i,err) ->
      Format.fprintf f "@[<hv>Expected eq_n=%d@ %a@]"
        i (error_ppformat ~display_format) err
    | `Test_internal loc ->
      Format.fprintf f "@[<hv>Internal error:@ %s@]" loc
    | `Test_internal_msg (loc, msg) ->
      Format.fprintf f "@[<hv>Internal error:@ %s@ %s@]" loc msg
    | `Test_md_file (md_file,s,grp,prg,err) ->
      let sep = "======================" in
      Format.fprintf f "@[<v>\
                          %s@,\
                          Failed to compile code block in %s@,\
                          Syntax: %s@,\
                          Group: %s@,\
                          %s@,\
                          Program:@,%s@,\
                          %s@,\
                          Error:@,## IGNORE THE LOCATION IF ANY##@,%a
                          %s@,@]"
        sep md_file s grp sep prg sep
        (error_ppformat ~display_format) err sep
    | `Test_bad_code_block arg ->
      Format.fprintf f "@[<hv>Bad code block argument '%s'@ only 'group=NAME' or 'skip' are allowed@]"
        arg
    | `Test_expected_to_fail -> Format.fprintf f "test was expected to fail but did not"
    | `Test_not_expected_to_fail -> Format.fprintf f "test was not expected to fail but did"
    | `Test_repl (expected, actual) ->
       Format.fprintf f "@[<hv>Expected:@ %a@ got:@ %a@]"
        (Simple_utils.PP_helpers.list_sep_d Simple_utils.PP_helpers.string)
        expected
        (Simple_utils.PP_helpers.list_sep_d Simple_utils.PP_helpers.string)
        actual

    | `Main_invalid_syntax_name syntax ->
      Format.fprintf f
        "@[<hv>Invalid syntax option: '%s'. @.Use 'pascaligo', 'cameligo', or 'reasonligo'. @]"
          syntax
    | `Main_invalid_dialect_name syntax ->
      Format.fprintf f
        "@[<hv>Invalid dialect option: '%s'. @.Use 'verbose' or 'terse'. @]"
          syntax
    | `Main_invalid_protocol_version (possible,actual) ->
      Format.fprintf f
      "@[<hv>Invalid protocol version '%s'. Available versions: %a"
        actual
        (Simple_utils.PP_helpers.list_sep_d Format.pp_print_string) possible
    | `Main_invalid_typer_switch actual ->
      Format.fprintf f
      "@[<hv>Invalid typer switch '%s'. Available: 'new' 'old'"
        actual
    | `Main_invalid_extension extension ->
      Format.fprintf f
        "@[<hv>Invalid file extension '%s'. @.Use '.ligo' for PascaLIGO, '.mligo' for CameLIGO, '.religo' for ReasonLIGO, or the --syntax option.@]"
        extension

    | `Main_unparse_tracer errs ->
      let errs = List.map ( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[Error(s) occurred while translating to Michelson:@.%a@]"
      (Tezos_client_008_PtEdoTez.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Main_typecheck_contract_tracer (_c,err_l) ->
      let errs = List.map ( fun e -> match e with `Tezos_alpha_error a -> a) err_l in
      Format.fprintf f "@[<hv>Error(s) occurred while type checking the contract:@.%a@]"
      (Tezos_client_008_PtEdoTez.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Main_could_not_serialize errs ->
      let errs = List.map ( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while serializing Michelson code:@.%a @]"
      (Tezos_client_008_PtEdoTez.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Main_check_typed_arguments (Simple_utils.Runned_result.Check_parameter, err) ->
      Format.fprintf f "@[<hv>Invalid command line argument. @.The provided parameter does not have the correct type for the given entrypoint.@ %a@]"
        (error_ppformat ~display_format) err

    | `Main_check_typed_arguments (Simple_utils.Runned_result.Check_storage, err) ->
      Format.fprintf f "@[<hv>Invalid command line argument. @.The provided storage does not have the correct type for the contract.@ %a@]"
        (error_ppformat ~display_format) err

    | `Main_unknown_failwith_type ->
      Format.fprintf f "@[<v>The contract failed to dry run, and returned an unsupported failwith type. Only int, string, and bytes are supported as failwith types for dry-run.@]"

    | `Main_unknown ->
      Format.fprintf f "@[<v>An unknown error occurred.@]"

    | `Main_execution_failed (fw:Runned_result.failwith) ->
      let value = match fw with
        | Failwith_int i -> string_of_int i
        | Failwith_string s -> s
        | Failwith_bytes b -> Bytes.to_string b in
      Format.fprintf f
        "@[<hv>An error occurred while evaluating an expression: %s@]"
        value
    | `Main_entrypoint_not_a_function -> Format.fprintf f "@[<hv>Invalid command line argument. @.The provided entrypoint is not a function.@]"
    | `Main_entrypoint_not_found -> Format.fprintf f "@[<hv>Invalid command line argument. @.The provided entrypoint is not found in the contract.@]"
    | `Main_invalid_balance a -> Format.fprintf f "@[<hv>Invalid command line option \"--balance\". @.The provided balance \"%s\" is invalid. Use an integer instead. @]" a
    | `Main_invalid_amount a -> Format.fprintf f "@[<hv>Invalid command line option \"--amount\". @.The provided amount \"%s\" is invalid. Use an integer instead. @]" a
    | `Main_invalid_source a -> Format.fprintf f "@[<hv>Invalid command line option \"--source\". @.The provided source address \"%s\" is invalid. A valid Tezos address is a string prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.@]" a
    | `Main_invalid_sender a -> Format.fprintf f "@[<hv>Invalid command line option \"--sender\". @.The provided sender address \"%s\" is invalid. A valid Tezos address is a string prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.@]" a
    | `Main_invalid_timestamp t -> Format.fprintf f "@[<hv>Invalid command line option \"--now\". @.The provided now \"%s\" is invalid. It should use RFC3339 notation in a string, or the number of seconds since Epoch.@]" t

    | `Main_unparse_michelson_result errs ->
      let errs = List.map ( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while unparsing the Michelson result:@.%a @]"
      (Tezos_client_008_PtEdoTez.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Main_parse_payload _ -> Format.fprintf f "@[<hv>Error parsing message. @]" (* internal testing *)
    | `Main_pack_payload _ -> Format.fprintf f "@[<hv>Error packing message. @]" (* internal testing *)
    | `Main_parse_michelson_input errs ->
      let errs = List.map ( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while parsing the Michelson input:@.%a @]"
      (Tezos_client_008_PtEdoTez.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Main_parse_michelson_code errs ->
      let errs = List.map ( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while checking the contract:@.%a @]"
        (Tezos_client_008_PtEdoTez.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Main_michelson_execution_error errs ->
      let errs = List.map ( fun e -> match e with `Tezos_alpha_error a -> a) errs in
      Format.fprintf f "@[<hv>Error(s) occurred while executing the contract:@.%a @]"
      (Tezos_client_008_PtEdoTez.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs

    | `Main_preproc e -> Preproc.Errors.error_ppformat ~display_format f e
    | `Main_parser e -> Parser.Errors.error_ppformat ~display_format f e
    | `Main_pretty _e -> () (*no error in this pass*)
    | `Main_self_cst_cameligo e -> Self_cst.Cameligo.Errors.error_ppformat ~display_format f e
    | `Main_self_cst_pascaligo e -> Self_cst.Pascaligo.Errors.error_ppformat ~display_format f e
    | `Main_self_cst_reasonligo e -> Self_cst.Reasonligo.Errors.error_ppformat ~display_format f e
    | `Main_cit_pascaligo e -> Tree_abstraction.Pascaligo.Errors.error_ppformat ~display_format f e
    | `Main_cit_cameligo e -> Tree_abstraction.Cameligo.Errors.error_ppformat ~display_format f e
    | `Main_cit_reasonligo e -> Tree_abstraction.Reasonligo.Errors.error_ppformat ~display_format f e
    | `Main_self_ast_imperative e -> Self_ast_imperative.Errors.error_ppformat ~display_format f e
    | `Main_purification e -> Purification.Errors.error_ppformat ~display_format f e
    | `Main_depurification _e -> () (*no error in this pass*)
    | `Main_desugaring _e -> () (*no error in this pass*)
    | `Main_sugaring _e -> () (*no error in this pass*)
    | `Main_typer e -> Typer.Errors.error_ppformat ~display_format f e
    | `Main_interpreter e -> Interpreter.Errors.error_ppformat ~display_format f e
    | `Main_self_ast_typed e -> Self_ast_typed.Errors.error_ppformat ~display_format f e
    | `Main_self_mini_c e -> Self_mini_c.Errors.error_ppformat ~display_format f e
    | `Main_spilling e -> Spilling.Errors.error_ppformat ~display_format f  e
    | `Main_stacking e -> Stacking.Errors.error_ppformat ~display_format f e

    | `Main_decompile_michelson e -> Stacking.Errors.error_ppformat ~display_format f  e
    | `Main_decompile_mini_c e -> Spilling.Errors.error_ppformat ~display_format f  e
    | `Main_decompile_typed e -> Typer.Errors.error_ppformat ~display_format f  e

    | `Repl_unexpected -> Format.fprintf f "unexpected error, missing expression?"
  )

let rec error_jsonformat : Types.all -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Test_err_tracer (name,err) ->
    let content = `Assoc [
      ("message", `String name);
      ("children", error_jsonformat err);
      ] in
    json_error ~stage:"test_tracer" ~content

  | `Test_run_tracer _
  | `Test_expect_tracer _
  | `Test_expect_n_tracer _
  | `Test_expect_exp_tracer _
  | `Test_expect_eq_n_tracer _
  | `Test_internal _
  | `Test_internal_msg _
  | `Test_md_file _
  | `Test_bad_code_block _
  | `Test_expected_to_fail
  | `Test_not_expected_to_fail
  | `Test_repl _
  -> `Null

  (* Top-level errors *)
  | `Build_error_tracer e -> json_error ~stage:"build system" ~content:(error_jsonformat e)
  | `Build_dependency_cycle trace ->
    let content = `Assoc [
      ("message", `String "dependency cycle detected") ;
      ("cycle",    `String trace) ; ] in
    json_error ~stage:"build system" ~content
  | `Build_corner_case (loc,msg) ->
    let content = `Assoc [
      ("message", `String msg) ;
      ("loc", `String loc) ]
    in
    json_error ~stage:"build system" ~content
  | `Main_invalid_syntax_name _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad syntax name")
  | `Main_invalid_dialect_name _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad dialect name")
  | `Main_invalid_protocol_version _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad protocol version")
  | `Main_invalid_typer_switch _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad typer switch")
  | `Main_invalid_extension _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad file extension")

  | `Main_unparse_tracer _ ->
    let content = `Assoc [("message", `String "could not unparse michelson type")] in
    json_error ~stage:"michelson contract build" ~content

  | `Main_typecheck_contract_tracer (c,_) ->
    let code = Format.asprintf "%a" Michelson.pp c in
    let content = `Assoc [
      ("message", `String "Could not typecheck michelson code") ;
      ("code",    `String code) ; ] in
    json_error ~stage:"michelson contract build" ~content

  | `Main_could_not_serialize _errs ->
    let content = `Assoc [("message", `String "Could not serialize michelson code")] in
    json_error ~stage:"michelson serialization" ~content

  | `Main_check_typed_arguments (Simple_utils.Runned_result.Check_parameter, err) ->
    let content = `Assoc [
      ("message", `String "Passed parameter does not match the contract type");
      ("children", error_jsonformat err);
      ] in
    json_error ~stage:"contract argument typechecking" ~content

  | `Main_check_typed_arguments (Simple_utils.Runned_result.Check_storage, err) ->
    let content = `Assoc [
      ("message", `String "Passed storage does not match the contract type");
      ("children", error_jsonformat err);
      ] in
    json_error ~stage:"contract argument typechecking" ~content

  | `Main_unknown_failwith_type ->
    json_error ~stage:"michelson execution" ~content:(`String "unknown failwith type")
  | `Main_unknown ->
    json_error ~stage:"michelson execution" ~content:(`String "unknown error")

  | `Main_execution_failed (fw:Runned_result.failwith) ->
    let value = match fw with
      | Failwith_int i -> `Assoc [("value", `Int i) ; ("type", `String "int")]
      | Failwith_string s -> `Assoc [("value", `String s) ; ("type", `String "int")]
      | Failwith_bytes b -> `Assoc [("value", `String (Bytes.to_string b)) ; ("type", `String "bytes")]
    in
    let content = `Assoc [("failwith", value)] in
    json_error ~stage:"michelson execution" ~content

  | `Main_invalid_amount a ->
    let message = `String "invalid amount" in
    let value = `String a in
    let content = `Assoc [("message", message) ; ("value", value)] in
    json_error ~stage:"parsing command line parameters" ~content
  | `Main_invalid_balance a ->
    let message = `String "invalid balance" in
    let value = `String a in
    let content = `Assoc [("message", message) ; ("value", value)] in
    json_error ~stage:"parsing command line parameters" ~content
  | `Main_invalid_source a ->
    let message = `String "invalid source" in
    let value = `String a in
    let content = `Assoc [("message", message) ; ("value", value)] in
    json_error ~stage:"parsing command line parameters" ~content
  | `Main_invalid_sender a ->
    let message = `String "invalid sender" in
    let value = `String a in
    let content = `Assoc [("message", message) ; ("value", value)] in
    json_error ~stage:"parsing command line parameters" ~content
  | `Main_invalid_timestamp t ->
    let message = `String "invalid timestamp notation" in
    let value = `String t in
    let content = `Assoc [("message", message) ; ("value", value)] in
    json_error ~stage:"parsing command line parameters" ~content

  | `Main_unparse_michelson_result _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error unparsing michelson result")

  | `Main_parse_payload _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error parsing message")

  | `Main_pack_payload _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error packing message")

  | `Main_parse_michelson_input _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error parsing input")

  | `Main_parse_michelson_code _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error parsing program code")

  | `Main_michelson_execution_error _ ->
    json_error ~stage:"michelson execution" ~content:(`String "error of execution")

  | `Main_entrypoint_not_a_function -> json_error ~stage:"top-level glue" ~content:(`String "given entrypoint is not a function")
  | `Main_entrypoint_not_found -> json_error ~stage:"top-level glue" ~content:(`String "Missing entrypoint")

  | `Main_preproc e -> Preproc.Errors.error_jsonformat e
  | `Main_parser e -> Parser.Errors.error_jsonformat e
  | `Main_pretty _ -> `Null (*no error in this pass*)
  | `Main_self_cst_cameligo e -> Self_cst.Cameligo.Errors.error_jsonformat e
  | `Main_self_cst_pascaligo e -> Self_cst.Pascaligo.Errors.error_jsonformat e
  | `Main_self_cst_reasonligo e -> Self_cst.Reasonligo.Errors.error_jsonformat e
  | `Main_cit_pascaligo e -> Tree_abstraction.Pascaligo.Errors.error_jsonformat e
  | `Main_cit_cameligo e -> Tree_abstraction.Cameligo.Errors.error_jsonformat e
  | `Main_cit_reasonligo e -> Tree_abstraction.Reasonligo.Errors.error_jsonformat e
  | `Main_self_ast_imperative e -> Self_ast_imperative.Errors.error_jsonformat e
  | `Main_purification e -> Purification.Errors.error_jsonformat e
  | `Main_depurification _ -> `Null (*no error in this pass*)
  | `Main_desugaring _ -> `Null (*no error in this pass*)
  | `Main_sugaring _ -> `Null (*no error in this pass*)
  | `Main_typer e -> Typer.Errors.error_jsonformat e
  | `Main_interpreter _ -> `Null (*no error*)
  | `Main_self_ast_typed e -> Self_ast_typed.Errors.error_jsonformat e
  | `Main_spilling e -> Spilling.Errors.error_jsonformat e
  | `Main_self_mini_c e -> Self_mini_c.Errors.error_jsonformat e
  | `Main_stacking e -> Stacking.Errors.error_jsonformat e

  | `Main_decompile_michelson e -> Stacking.Errors.error_jsonformat e
  | `Main_decompile_mini_c e -> Spilling.Errors.error_jsonformat e
  | `Main_decompile_typed e -> Typer.Errors.error_jsonformat e

  | `Repl_unexpected ->
     let message = `String "unexpected error" in
     let content = `Assoc [("message", message)] in
     json_error ~stage:"evaluating expression" ~content

let error_format : _ Display.format = {
  pp = error_ppformat;
  to_json = error_jsonformat;
}
