(* Internal dependencies *)

module type COMMENTS = Preprocessing_shared.Comments.S

(* Making lexers *)

module Make (Comments : COMMENTS) (Token : Token.S) =
  struct
    module Trace = Simple_utils.Trace

    type file_path = string
    type dirs      = file_path list (* For #include and #import *)

    module Lexer = Lexer.Make (Token)
    module Scan  = LexerLib.API.Make (Lexer)

    (* Results and errors *)

    type errors = Errors.t
    type result = (Token.t list, errors) Trace.result

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

    let filter = function
      Stdlib.Ok tokens -> Trace.ok tokens
    | Error msg -> Trace.fail @@ Errors.generic msg

    let lex_file file_path =
      let config = mk_config ~input:(Some file_path)
      in Scan.all_from_file config file_path |> filter

    let lex_string string =
      Scan.all_from_string (mk_config ~input:None) string |> filter

    let lex_channel channel =
      Scan.all_from_channel (mk_config ~input:None) channel |> filter
  end
