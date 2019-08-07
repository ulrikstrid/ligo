open Trace

val evaluate_typed : ?debug_mini_c:bool -> ?debug_michelson:bool -> ?options:Proto_alpha_utils.Memory_proto_alpha.options -> string 
-> Ast_typed.program -> Ast_typed.annotated_expression result

val run_typed : ?debug_mini_c:bool -> ?debug_michelson:bool -> ?options:Proto_alpha_utils.Memory_proto_alpha.options -> string ->
    Ast_typed.program -> Ast_typed.annotated_expression -> Ast_typed.annotated_expression result

(*
val transpile_value : Ast_typed.annotated_expression -> Mini_c.value result
*)
