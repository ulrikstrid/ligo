(* Interfacing the JsLIGO lexer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module Token = Lexing_jsligo.Token

(* Results and errors *)

type errors = Lexing_shared.Errors.t
type result = (Token.t list, errors) Trace.result

type file_path = string
type dirs      = file_path list (* For #include and #import *)

(* Lexing various inputs *)

val lex_file   : file_path -> result
val lex_string : string -> result
