open Trace


type s_syntax = Syntax_name of string
type v_syntax =  [`pascaligo | `cameligo ]

include sig
  open Ast_typed
  val assert_valid_entry_point : program -> string -> unit result

  (*
  val get_entry_point_type : type_value -> (type_value * type_value) result

  val get_entry_point : program -> string -> (type_value * type_value) result
  *)
end


val compile_contract_file : string -> string -> s_syntax -> string result

val compile_contract_parameter : string -> string -> string -> s_syntax -> string result

val compile_contract_storage : string -> string -> string -> s_syntax -> string result

val run_contract : ?amount:string -> string -> string -> string -> string -> s_syntax -> Ast_simplified.expression result

val run_function : ?amount:string -> string -> string -> string -> s_syntax -> Ast_simplified.expression result

val evaluate_value : ?amount:string -> string -> string -> s_syntax -> Ast_simplified.expression result

(*
include sig
  open Ast_simplified

  val assert_entry_point_defined : program -> string -> unit result
end

val transpile_value : Ast_typed.annotated_expression -> Mini_c.value result

val parsify_pascaligo : string -> Ast_simplified.program result

val parsify_expression_pascaligo : string -> Ast_simplified.expression result

val parsify_ligodity : string -> Ast_simplified.program result

val parsify_expression_ligodity : string -> Ast_simplified.expression result


val syntax_to_variant : s_syntax -> string option -> v_syntax result

val parsify : v_syntax -> string -> Ast_simplified.program result

val parsify_expression : v_syntax -> string -> Ast_simplified.expression result

val type_file : ?debug_simplify:bool -> ?debug_typed:bool -> v_syntax -> string -> Ast_typed.program result
*)
