open Trace
val run_simplityped : ?options:Proto_alpha_utils.Memory_proto_alpha.options -> ?debug_mini_c:bool -> ?debug_michelson:bool 
-> Ast_typed.program -> string -> Ast_simplified.expression -> Ast_simplified.expression result

val evaluate_simplityped : ?options:Proto_alpha_utils.Memory_proto_alpha.options ->  ?debug_mini_c:bool -> ?debug_michelson:bool -> Ast_typed.program
-> string -> Ast_simplified.expression result 
