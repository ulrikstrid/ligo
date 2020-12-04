(* Interfacing the PascaLIGO lexer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module Token  = Lexing_pascaligo.Token
module Errors = Lexing_shared.Errors

(* Lexing various inputs *)

type result = (Token.t list, Errors.t) Trace.result
type file_path = string

val from_file    : file_path -> result
val from_string  : string    -> result
val from_buffer  : Buffer.t  -> result

(* Aliases *)

val lex_file   : file_path -> result
val lex_string : string    -> result
val lex_buffer : Buffer.t  -> result
