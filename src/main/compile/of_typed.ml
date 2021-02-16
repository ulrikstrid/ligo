open Trace
open Ast_typed
open Spilling
open Main_errors

let compile : Ast_typed.module_fully_typed -> (Mini_c.program, _) result = fun p ->
  trace spilling_tracer @@ compile_module p

let compile_expression : expression -> (Mini_c.expression, _) result = fun e ->
  trace spilling_tracer @@ compile_expression e

let assert_equal_contract_type : Simple_utils.Runned_result.check_type -> string -> Ast_typed.module_fully_typed -> Ast_typed.expression -> (unit , _) result =
    fun c entry contract param ->
  let%bind entry_point = trace_option entrypoint_not_found (Ast_typed.get_entry contract entry) in
  trace (arguments_check_tracer c) (
    match entry_point.type_expression.type_content with
    | T_arrow {type1=args} -> (
        match args.type_content with
        | T_record m when LMap.cardinal m.content = 2 -> (
          let {associated_type=param_exp;_} = LMap.find (Label "0") m.content in
          let {associated_type=storage_exp;_} = LMap.find (Label "1") m.content in
            match c with
            | Check_parameter -> trace typer_tracer @@ Typer.assert_type_expression_eq entry_point.location (param_exp, param.type_expression)
            | Check_storage   -> trace typer_tracer @@ Typer.assert_type_expression_eq entry_point.location (storage_exp, param.type_expression)
        )
        | _ -> fail @@ entrypoint_not_a_function )
    | _ -> fail @@ entrypoint_not_a_function
  )

let some_interpret ~options x test_entry = trace interpret_tracer @@ Interpreter.eval_test ~options x test_entry

let some_interpret_random ~options n x test_entry = trace interpret_tracer @@ Interpreter.eval_test_random ~options n x test_entry
