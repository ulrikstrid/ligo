(* Interfacing the CameLIGO lexer. *)

(* LIGO dependencies *)

module Comments = Preproc_cameligo.Comments
module File     = Preproc_cameligo.File

(* Internal dependencies *)

module Token  = Lexer_cameligo.Token
module Lexer  = Lexer_shared.Lexer.Make (Token)
module Errors = Lexer_shared.Errors

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

let only_lex_file file_path =
  let config = mk_config ~input:(Some file_path)
  in Scan.all_from_file config file_path |> filter

let only_lex_string string =
  Scan.all_from_string (mk_config ~input:None) string |> filter
