(* Interfacing the PascaLIGO lexer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module Token = Lexer_pascaligo.Token

(* Results and errors *)

type error  = Lexer_shared.Errors.lexing_error
type result = (Token.t list, error) Trace.result

type file_path = string
type dirs      = file_path list (* For #include and #import *)

(* Lexing various inputs *)

val lex_file   : file_path -> result
val lex_string : string -> result
