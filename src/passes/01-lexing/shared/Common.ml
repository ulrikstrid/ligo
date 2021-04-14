(* Internal dependencies *)

module type COMMENTS = Preprocessing_shared.Comments.S

(* Making lexers *)

module Make (Comments : COMMENTS) (Token : Token.S) =
  struct
    module Trace = Simple_utils.Trace
    module Errors = Errors

    type file_path = string

    module Lexer = Lexer.Make (Token)
    module Scan  = LexerLib.API.Make (Lexer)

    (* Results *)

    type result = (Token.t list, Errors.t) Trace.result

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

    (* Lifting [Stdlib.result] to [Trace.result]. *)

    let lift = function
      Stdlib.Ok tokens -> Trace.ok tokens
    | Error msg -> Trace.fail @@ Errors.generic msg

    (* Lexing functions *)

    let from_file file_path =
      let config = mk_config ~input:(Some file_path)
      in Scan.Tokens.from_file config file_path |> lift

    let from_string string =
      Scan.Tokens.from_string (mk_config ~input:None) string |> lift

    let from_buffer buffer =
      Scan.Tokens.from_buffer (mk_config ~input:None) buffer |> lift

    let from_channel channel =
      Scan.Tokens.from_channel (mk_config ~input:None) channel |> lift

    (* Aliases *)

    let lex_file    = from_file
    let lex_string  = from_string
    let lex_buffer  = from_buffer
    let lex_channel = from_channel
  end
