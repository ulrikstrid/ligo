(* Internal dependencies *)

module type COMMENTS = Preprocessing_shared.Comments.S

(* Making lexers *)

module Make (Comments : COMMENTS) (Token : Token.S) :
  sig
    module Trace = Simple_utils.Trace

    type file_path = string
    type dirs      = file_path list (* For #include and #import *)

    type errors = Errors.t
    type result = (Token.t list, errors) Trace.result

    val lex_file    : file_path  -> result
    val lex_string  : string     -> result
    val lex_channel : in_channel -> result
  end
