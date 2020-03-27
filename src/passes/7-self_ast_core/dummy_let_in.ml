open Ast_core
open Trace


let peephole_expression : expression -> expression result = fun e ->
  match e.expression_content with
  | E_let_in {let_binder;rhs={expression_content=E_literal (Literal_unit)};let_result} 
    when Var.equal (fst let_binder) (Var.of_name "_") ->
    ok @@ let_result
  | _ -> ok @@ e
