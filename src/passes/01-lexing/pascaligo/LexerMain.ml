(* Driver for the PascaLIGO lexer *)

module Comments    = Preproc_pascaligo.Comments
module File        = Preproc_pascaligo.File
module Token       = Lexer_pascaligo.Token
module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module Lexer_CLI   = LexerLib.CLI.Make (Preproc_CLI)
module Self_tokens = Lexer_pascaligo.Self_tokens

module Main = Lexer_shared.LexerMainGen.Make
                (Comments) (File) (Token) (Lexer_CLI) (Self_tokens)

let () = Main.check_cli ()
let () = Main.scan_all ()
