open Ast_sugar
open Trace

let peephole_expression : expression -> expression result = fun e ->
  match e.expression_content with
  | E_sequence {expr1={expression_content=E_skip;_};expr2} ->
    ok @@ expr2
  | E_sequence {expr1;expr2={expression_content=E_skip;_}} ->
    ok @@ expr1
  | _ -> ok @@ e
