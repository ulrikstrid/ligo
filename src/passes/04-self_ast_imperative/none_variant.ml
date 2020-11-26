open Errors
open Ast_imperative
open Trace

let peephole_expression : expression -> (expression , self_ast_imperative_error) result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  (* | E_constructor {constructor=Label "Some";element=e} ->
     return @@ E_constant {cons_name=(Const C_SOME);arguments=[ e ]}
  | E_constructor {constructor=Label "None";element=_} ->
     return @@ E_constant {cons_name=(Const C_NONE) ; arguments=[]} *)
  | E_matching {matchee;cases=Match_variant [((Label none_n, _   ),none_expr);((Label some_n, some),some_expr)]} when (
      String.equal none_n Stage_common.Constant.ctor_none_name &&
      String.equal some_n Stage_common.Constant.ctor_some_name ) ->
   let match_none = none_expr in
   let match_some = some,some_expr in
   let cases = Match_option {match_none;match_some} in
   return @@ E_matching {matchee;cases}
  | E_matching {matchee;cases=Match_variant [((Label some_n, some),some_expr);((Label none_n, _),none_expr)]} when (
      String.equal none_n Stage_common.Constant.ctor_none_name &&
      String.equal some_n Stage_common.Constant.ctor_some_name ) ->
   let match_none = none_expr in
   let match_some = some,some_expr in
   let cases = Match_option {match_none;match_some} in
   return @@ E_matching {matchee;cases}
  | e -> return e
