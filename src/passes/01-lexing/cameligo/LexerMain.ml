(* Driver for the CameLIGO lexer *)

module Comments    = Preproc_cameligo.Comments
module File        = Preproc_cameligo.File
module Token       = Lexer_cameligo.Token
module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module Lexer_CLI   = LexerLib.CLI.Make (Preproc_CLI)
module Self_tokens = Lexer_cameligo.Self_tokens

module Main = Lexer_shared.LexerMainGen.Make
                (Comments) (File) (Token) (Lexer_CLI) (Self_tokens)

let () = Main.check_cli ()
let () = Main.scan_all ()
