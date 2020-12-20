(* This module implements a filter on the lexical units of PascaLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core = LexerLib.Core

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    val filter : lex_unit list -> token list
  end

type token = Token.t
type lex_unit = token Core.lex_unit

let filter lex_units =
  let open Core in
  let apply tokens = function
    Token token -> token::tokens
  | Markup _ | Directive _ -> tokens
  in List.fold_left apply [] lex_units |> List.rev
