open Trace
(* open Ast_typed *)
include Stage_common.Types

type env = Ast_typed.environment

type control_elt =
  | C_elt of Ast_typed.annotated_expression
  | C_ap
type control = control_elt list

type stack_elt =
  | S_exp of Ast_typed.annotated_expression
  | S_cloture of (Ast_typed.lambda * env)
type stack = stack_elt list

type dump_elt = (stack * env * control)
type dump = dump_elt list


let dummy : Ast_typed.program -> string result =
  fun prg ->
    let _ = List.iter
        (fun el ->
          let (Ast_typed.Declaration_constant (named_exp, _inline, (pre, post))) = Location.unwrap el in
          Format.printf " \n------\n";
          Format.printf " name : %s\n" (Var.to_name named_exp.name) ;
          Format.printf " pre : \n %a\n" Ast_typed.Environment.PP.full_environment pre ;
          Format.printf " post : \n %a\n" Ast_typed.Environment.PP.full_environment post;
          Format.printf " \n------\n"
        )
        prg in
    ok "dummy"
