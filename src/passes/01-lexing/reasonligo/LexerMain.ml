(* Driver for the ReasonLIGO lexer *)

module Comments    = Preproc_reasonligo.Comments
module File        = Preproc_reasonligo.File
module Token       = Lexer_reasonligo.Token
module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module Lexer_CLI   = LexerLib.CLI.Make (Preproc_CLI)
module Self_tokens = Lexer_reasonligo.Self_tokens

module Main = Lexer_shared.LexerMainGen.Make
                (Comments) (File) (Token) (Lexer_CLI) (Self_tokens)

let () = Main.check_cli ()
let () = Main.scan_all ()
