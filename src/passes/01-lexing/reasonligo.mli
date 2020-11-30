(* Interfacing the ReasonLIGO lexer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module Token = Lexer_reasonligo.Token

(* Results and errors *)

type error  = Lexer_shared.Errors.lexing_error
type result = (Token.t list, error) Trace.result

type file_path = string
type dirs      = file_path list (* For #include and #import *)

(* Preprocessing and lexing various inputs *)

(*
val lex_file   : dirs -> file_path -> result
val lex_string : dirs -> string -> result
 *)

(* Only lexing various inputs *)

val only_lex_file   : file_path -> result
val only_lex_string : string -> result
