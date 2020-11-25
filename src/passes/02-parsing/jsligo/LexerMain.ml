(* Driver for the JsLIGO lexer *)

module Comments = Lexer_jsligo.Comments
module File     = Lexer_jsligo.File
module Token    = Lexer_jsligo.Token

module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module Lexer_CLI   = LexerLib.CLI.Make (Preproc_CLI)

module MainGen = Shared.LexerMainGen
module Main    = MainGen.Make (Comments) (File) (Token) (Lexer_CLI)

let () = Main.check_cli ()
let () = Main.scan_all ()
