(* This file provides an interface to the ReasonLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module File        = Preprocessing_reasonligo.File
module Comments    = Preprocessing_reasonligo.Comments
module Token       = Lexing_reasonligo.Token
module Self_tokens = Lexing_reasonligo.Self_tokens
module Scoping     = Parsing_reasonligo.Scoping
module ParErr      = Parsing_reasonligo.ParErr
module Parser      = Parsing_reasonligo.Parser
module CST         = Cst.Reasonligo
module Pretty      = Parsing_reasonligo.Pretty

(* Making the parsers *)

module ReasonligoParser =
  struct
    module CST = CST
    include Parser
  end

include Parsing_shared.Common.MakeTwoParsers
          (File) (Comments) (Token) (Scoping) (ParErr) (Self_tokens)
          (CST) (ReasonligoParser)

(* Making the pretty-printers *)

include Parsing_shared.Common.MakePretty (CST) (Pretty)

let pretty_print_file buffer file_path =
  ContractParser.parse_file buffer file_path |> Trace.map pretty_print
