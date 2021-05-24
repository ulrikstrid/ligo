open Types

let extend :
  env -> ?ast_type:Ast_typed.type_expression -> (expression_variable * value) -> env
  = fun env ?ast_type (var,eval_term) ->
  Env.add var {ast_type;eval_term} env

let lookup :
  env -> expression_variable -> value_expr option
    = fun env var -> Env.find_opt var env

let empty_env = Env.empty

let to_kv_list = Env.to_kv_list
