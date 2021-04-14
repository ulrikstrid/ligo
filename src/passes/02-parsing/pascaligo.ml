(* This file provides an interface to the PascaLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module File        = Preprocessing_pascaligo.File
module Comments    = Preprocessing_pascaligo.Comments
module Token       = Lexing_pascaligo.Token
module Self_tokens = Lexing_pascaligo.Self_tokens
module ParErr      = Parsing_pascaligo.ParErr
module Parser      = Parsing_pascaligo.Parser
module CST         = Cst.Pascaligo
module Pretty      = Parsing_pascaligo.Pretty

(* Making the parsers *)

module PascaligoParser =
  struct
    module CST = CST
    include Parser
  end

include Parsing_shared.Common.MakeTwoParsers
          (File) (Comments) (Token) (ParErr) (Self_tokens)
          (CST) (PascaligoParser)

(* Making the pretty-printers *)

include Parsing_shared.Common.MakePretty (CST) (Pretty)

let pretty_print_file buffer file_path =
  ContractParser.parse_file buffer file_path |> Trace.map pretty_print

let pretty_print_cst buffer file_path =
  let cst = parse_file buffer file_path in
  let buffer = Buffer.create 59 in
  let state =
    Cst_pascaligo.Printer.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  let apply tree =
    Cst_pascaligo.Printer.pp_cst state tree; buffer
  in Trace.map apply cst
