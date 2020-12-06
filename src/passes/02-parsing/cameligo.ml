(* This file provides an interface to the CameLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module File        = Preprocessing_cameligo.File
module Comments    = Preprocessing_cameligo.Comments
module Token       = Lexing_cameligo.Token
module Self_tokens = Lexing_cameligo.Self_tokens
module Scoping     = Parsing_cameligo.Scoping
module ParErr      = Parsing_cameligo.ParErr
module Parser      = Parsing_cameligo.Parser
module CST         = Cst.Cameligo
module Pretty      = Parsing_cameligo.Pretty

(* Making the parsers *)

module CameligoParser =
  struct
    module CST = CST
    include Parser
  end

include Parsing_shared.Common.MakeTwoParsers
          (File) (Comments) (Token) (Scoping) (ParErr) (Self_tokens)
          (CST) (CameligoParser)

(* Making the pretty-printers *)

include Parsing_shared.Common.MakePretty (CST) (Pretty)

let pretty_print_file buffer file_path =
  ContractParser.parse_file buffer file_path |> Trace.map pretty_print
