(* Interfacing the PascaLIGO lexer. *)

(* LIGO dependencies *)

module Comments = Preprocessing_pascaligo.Comments

(* Internal dependencies *)

module Token = Lexing_pascaligo.Token

include Lexing_shared.Common.Make (Comments) (Token)

(* Aliases *)

let lex_file   = from_file
let lex_string = from_string
let lex_buffer = from_buffer
