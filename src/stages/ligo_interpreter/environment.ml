open Types

let extend :
  env -> ?init_term:Ast_typed.expression -> (expression_variable * value) -> env
  = fun env ?init_term (var,eval_term) ->
  Env.add var {init_term;eval_term} env

let lookup :
  env -> expression_variable -> value_expr option
    = fun env var -> Env.find_opt var env

let empty_env = Env.empty

let to_kv_list = Env.to_kv_list
