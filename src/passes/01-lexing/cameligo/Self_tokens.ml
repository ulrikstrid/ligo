(* This module implements a filter on the lexical units of CameLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core   = LexerLib.Core
module Region = Simple_utils.Region
module Pos = Simple_utils.Pos
module Utils  = Simple_utils.Utils

(* Signature *)

module type S =
  sig
    type token
    type token_next
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token_next list, message) result
  end

(* Filters *)

let ok x = Stdlib.Ok x

type message = string Region.reg

type token = Token.t
type token_next = Comments.t
type lex_unit = token Core.lex_unit

let to_token_next = Comments.to_t

let to_token t = Comments.to_token t

(* Exported *)

let filter = Utils.(Comments.attach <@ Style.check)
