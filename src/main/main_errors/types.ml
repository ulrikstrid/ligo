type tezos_alpha_error =  [`Tezos_alpha_error of Tezos_error_monad.Error_monad.error]

type all =
[
 | `Build_error_tracer of all
 | `Build_dependency_cycle of string
 | `Build_corner_case of string * string (* TO REMOVE *)

 | `Main_invalid_syntax_name of string
 | `Main_invalid_dialect_name of string
 | `Main_invalid_extension of string
 | `Main_invalid_protocol_version of string list *  string
 | `Main_invalid_typer_switch of string
 | `Main_unparse_tracer of tezos_alpha_error list
 | `Main_typecheck_contract_tracer of int Michelson.michelson * tezos_alpha_error list
 | `Main_could_not_serialize of tezos_alpha_error list
 | `Main_check_typed_arguments of Simple_utils.Runned_result.check_type * all
 | `Main_unknown_failwith_type
 | `Main_unknown
 | `Main_execution_failed of Runned_result.failwith
 | `Main_unparse_michelson_result of tezos_alpha_error list
 | `Main_parse_payload of tezos_alpha_error list
 | `Main_pack_payload of tezos_alpha_error list
 | `Main_parse_michelson_input of tezos_alpha_error list
 | `Main_parse_michelson_code of tezos_alpha_error list
 | `Main_michelson_execution_error of tezos_alpha_error list

 | `Main_preproc of Preproc.Errors.preproc_error
 | `Main_parser of Parser.Errors.parse_error
 | `Main_pretty of Parser.Errors.parse_error
 | `Main_self_cst_cameligo of Self_cst.Cameligo.Errors.self_cst_cameligo_error
 | `Main_self_cst_pascaligo of Self_cst.Pascaligo.Errors.self_cst_pascaligo_error
 | `Main_self_cst_reasonligo of Self_cst.Reasonligo.Errors.self_cst_reasonligo_error
 | `Main_cit_pascaligo of Tree_abstraction.Pascaligo.Errors.abs_error
 | `Main_cit_cameligo of Tree_abstraction.Cameligo.Errors.abs_error
 | `Main_cit_reasonligo of Tree_abstraction.Reasonligo.Errors.abs_error
 | `Main_self_ast_imperative of Self_ast_imperative.Errors.self_ast_imperative_error
 | `Main_purification   of Purification.Errors.purification_error
 | `Main_depurification of Purification.Errors.purification_error
 | `Main_desugaring of Desugaring.Errors.desugaring_error
 | `Main_sugaring   of Desugaring.Errors.desugaring_error
 | `Main_typer of Typer.Errors.typer_error
 | `Main_interpreter of Interpreter.interpreter_error
 | `Main_self_ast_typed of Self_ast_typed.Errors.self_ast_typed_error
 | `Main_self_mini_c of Self_mini_c.Errors.self_mini_c_error
 | `Main_spilling of Spilling.Errors.spilling_error
 | `Main_stacking of Stacking.Errors.stacking_error

 | `Main_decompile_michelson of Stacking.Errors.stacking_error
 | `Main_decompile_mini_c of Spilling.Errors.spilling_error
 | `Main_decompile_typed of Typer.Errors.typer_error
 | `Main_entrypoint_not_a_function
 | `Main_entrypoint_not_found
 | `Main_invalid_balance of string
 | `Main_invalid_amount of string
 | `Main_invalid_sender of string
 | `Main_invalid_source of string
 | `Main_invalid_timestamp of string

 | `Test_err_tracer of string * all
 | `Test_run_tracer of string * all
 | `Test_expect_tracer of Ast_core.expression * Ast_core.expression
 | `Test_expect_n_tracer of int * all
 | `Test_expect_exp_tracer of Ast_core.expression * all
 | `Test_expect_eq_n_tracer of int * all
 | `Test_internal of string
 | `Test_internal_msg of string * string
 | `Test_md_file of string * string * string * string * all
 | `Test_bad_code_block of string
 | `Test_expected_to_fail
 | `Test_not_expected_to_fail
 | `Test_repl of string list * string list

 | `Repl_unexpected
]
