open Mini_c
open Proto_alpha_utils
open Trace

let compile_contract : expression -> Compiler.compiled_expression result = fun e ->
  let%bind e = Self_mini_c.contract_check e in
  let%bind (input_ty , _) = get_t_function e.type_value in
  let%bind body = get_function e in
  let%bind body = Compiler.Program.translate_function_body body [] input_ty in
  let expr = Self_michelson.optimize body in
  let%bind expr_ty = Compiler.Type.Ty.type_ e.type_value in
  ok ({ expr_ty ; expr } : Compiler.Program.compiled_expression)

let compile_expression : expression -> Compiler.compiled_expression result = fun e ->
  let%bind expr = Compiler.Program.translate_expression e Compiler.Environment.empty in
  let expr = Self_michelson.optimize expr in
  let%bind expr_ty = Compiler.Type.Ty.type_ e.type_value in
  ok ({ expr_ty ; expr } : Compiler.Program.compiled_expression)

let aggregate_and_compile ~aggressive_inlining = fun program form ->
  let%bind aggregated = aggregate_entry program form in
  let aggregated' = Self_mini_c.all_expression ~aggressive_inlining aggregated in
  match form with
  | ContractForm _ -> compile_contract aggregated'
  | ExpressionForm _ -> compile_expression aggregated'

let aggregate_and_compile_contract ?(aggressive_inlining = false) = fun (program : Types.program) name ->
  let%bind (exp, idx) = get_entry program name in
  let program' = List.take idx program in
  aggregate_and_compile ~aggressive_inlining program' (ContractForm exp)

let aggregate_and_compile_expression ?(aggressive_inlining = false) = fun program exp ->
  aggregate_and_compile ~aggressive_inlining program (ExpressionForm exp)

let pretty_print program = 
  Mini_c.PP.program program
