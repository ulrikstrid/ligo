open Trace
(*
module Parser = Parser_pascaligo.Parser
*)
module AST = Parser_pascaligo.AST
module ParserLog = Parser_pascaligo.ParserLog

val parse_file : string -> AST.t result 
val parse_string : string -> AST.t result
val parse_expression : string -> AST.expr result
