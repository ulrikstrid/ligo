(* Interfacing the CameLIGO lexer. *)

(* LIGO dependencies *)

module Comments = Preprocessing_cameligo.Comments

(* Internal dependencies *)

module Token = Lexing_cameligo.Token

include Lexing_shared.Common.Make (Comments) (Token)

(* Aliases *)

let lex_file   = from_file
let lex_string = from_string
let lex_buffer = from_buffer
