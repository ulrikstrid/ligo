(* This module implements a filter on the lexical units of JsLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core   = LexerLib.Core
module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token list, message) result
  end

(* Filters *)

let ok x = Stdlib.Ok x

type message = string Region.reg

type token = Token.t
type lex_unit = token Core.lex_unit

let tokens_of = function
  Stdlib.Ok lex_units ->
    let open Core in
    let apply tokens = function
      Token token -> token::tokens
    | Markup _ | Directive _ -> tokens
    in List.fold_left apply [] lex_units |> List.rev |> ok
| Error _ as err -> err

let filter
    : (lex_unit list, message) result -> (token list, message) result =
  Utils.(tokens_of <@ Style.check)
