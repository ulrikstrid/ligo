open Trace
(*
module Parser = Parser_ligodity.Parser
*)
module AST = Parser_ligodity.AST

val parse_file : string -> AST.t result
val parse_string : string -> AST.t result
val parse_expression : string -> AST.expr result
