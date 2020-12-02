(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module type COMMENTS = Preprocessing_shared.Comments.S

(* File system *)

type file_path = string
type dirs      = file_path list (* For #include and #import *)

(* Making lexers *)

module Make (Comments : COMMENTS) (Token : Token.S) :
  sig
    type result = (Token.t list, Errors.t) Trace.result

    val lex_file    : file_path  -> result
    val lex_string  : string     -> result
    val lex_channel : in_channel -> result
  end
