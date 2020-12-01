(* Interfacing the ReasonLIGO lexer. *)

(* LIGO dependencies *)

module Comments = Preprocessing_reasonligo.Comments
module File     = Preprocessing_reasonligo.File

(* Internal dependencies *)

module Token  = Lexing_reasonligo.Token
module Lexer  = Lexing_shared.Lexer.Make (Token)
module Errors = Lexing_shared.Errors

(* Vendor dependencies *)

module Trace = Simple_utils.Trace
module Scan  = LexerLib.API.Make (Lexer)

(* Results and errors *)

type error  = Errors.lexing_error
type result = (Token.t list, error) Trace.result

(* Lexer configurations *)

let mk_config ~input =
  object
    method block     = Comments.block
    method line      = Comments.line
    method input     = input
    method offsets   = true       (* TODO: Should flow from CLI *)
    method mode      = `Point
    method command   = None
    method is_eof    = Token.is_eof
    method to_region = Token.to_region
    method to_lexeme = Token.to_lexeme
    method to_string = Token.to_string
  end

(* Lexing functions *)

type file_path = string
type dirs      = file_path list (* For #include and #import *)

let filter = function
  Stdlib.Ok tokens -> Trace.ok tokens
| Error msg -> Trace.fail @@ Errors.generic msg

let lex_file file_path =
  let config = mk_config ~input:(Some file_path)
  in Scan.all_from_file config file_path |> filter

let lex_string string =
  Scan.all_from_string (mk_config ~input:None) string |> filter
