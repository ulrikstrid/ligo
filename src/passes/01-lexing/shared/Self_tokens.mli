(* Vendor dependencies *)

module Core = LexerLib.Core

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    val filter : lex_unit list -> token list
  end
