open Trace

let source_to_typed syntax source_file state =
  let%bind simplified  = Of_source.compile source_file syntax in
  let%bind (typed, state) = Typer.type_program simplified state in
  let env = Ast_typed.program_environment typed in
  ok (typed, state, env)

let source_to_typed_expression ~env ~state parameter syntax =
  let%bind simplified = Of_source.compile_expression syntax parameter in
  let%bind (typed, state) = Typer.type_expression env state simplified in
  ok (typed, state)

let typed_to_michelson_program
    (typed: Ast_typed.program) (entry_point:string) : Compiler.compiled_program result =
  let%bind mini_c = Of_typed.compile typed in
  Of_mini_c.compile_function_entry mini_c entry_point

let typed_to_michelson_value_as_function
    (typed: Ast_typed.program) (entry_point:string) : Compiler.compiled_program result =
  let%bind mini_c = Of_typed.compile typed in
  Of_mini_c.compile_expression_as_function_entry mini_c entry_point

let typed_expression_to_michelson_value_as_function
    (typed: Ast_typed.annotated_expression) : Compiler.compiled_program result =
  let%bind mini_c = Of_typed.compile_expression typed in
  Of_mini_c.compile_expression_as_function mini_c

let simplified_to_compiled_program
    ~env ~state (exp: Ast_simplified.expression) : (Compiler.compiled_program * Typer.state) result =
  let%bind (typed, state) = Typer.type_expression env state exp in
  let%bind compiled = typed_expression_to_michelson_value_as_function typed in
  ok (compiled, state)

let typed_to_michelson_contract
    (typed: Ast_typed.program) (entry_point:string) : Michelson.michelson result =
  let%bind mini_c = Of_typed.compile typed in
  Of_mini_c.compile_contract_entry mini_c entry_point

let source_to_michelson_contract syntax source_file entry_point state =
  let%bind (typed, state, _env) = source_to_typed syntax source_file state in
  let%bind compiled = typed_to_michelson_contract typed entry_point in
  ok (compiled, state)

let source_expression_to_michelson_value_as_function ~env ~state parameter syntax =
  let%bind (typed, state) = source_to_typed_expression ~env ~state parameter syntax in
  let%bind mini_c = Of_typed.compile_expression typed in
  let%bind compiled = Of_mini_c.compile_expression_as_function mini_c in
  ok (compiled, state)

let source_contract_input_to_michelson_value_as_function ~env ~state (storage,parameter) syntax =
  let%bind simplified = Of_source.compile_contract_input storage parameter syntax in
  let%bind (typed, state) = Typer.type_expression env state simplified in
  let%bind compiled = typed_expression_to_michelson_value_as_function typed in
  ok (compiled, state)
