(* Vendor dependencies *)

module Core   = LexerLib.Core
module Region = Simple_utils.Region

(* Signature *)

module type S =
  sig
    type token
    type token_next
    type lex_unit = token Core.lex_unit

    val to_token_next: 
      token -> token_next

    val to_token: 
      token_next -> token

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token_next list, message) result
  end
