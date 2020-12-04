(* This file provides an interface to the CameLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies on CameLIGO *)

module File        = Preprocessing_cameligo.File
module Comments    = Preprocessing_cameligo.Comments
module Token       = Lexing_cameligo.Token
module Self_tokens = Lexing_cameligo.Self_tokens
module Scoping     = Parsing_cameligo.Scoping
module ParErr      = Parsing_cameligo.ParErr
module CST         = Cst.Cameligo
module Errors      = Parsing_shared.Errors
module MakeParser  = Parsing_shared.Common.MakeParser
module MkParser    = MakeParser (File) (Comments) (Token) (Scoping) (ParErr)

(* Parser for contracts *)

module ContractCST =
  struct
    type t = CST.t
  end

module ContractParser_Menhir =
  struct
    include Parsing_cameligo.Parser
    type tree = ContractCST.t

    let main = contract

    module Incremental =
      struct
        let main = Incremental.contract
      end
  end

module ContractParser =
  MkParser (ContractCST) (ContractParser_Menhir) (Self_tokens)

(* Parser for expressions *)

module ExprCST =
  struct
    type t = CST.expr
  end

module ExprParser_Menhir =
  struct
    include Parsing_cameligo.Parser
    type tree = ExprCST.t

    let main = interactive_expr

    module Incremental =
      struct
        let main = Incremental.interactive_expr
      end
  end

module ExprParser =
  MkParser (ExprCST) (ExprParser_Menhir) (Self_tokens)

(* Results and errors *)

type cst    = (CST.t ,   Errors.t) Trace.result
type expr   = (CST.expr, Errors.t) Trace.result
type buffer = (Buffer.t, Errors.t) Trace.result

let fail msg = Trace.fail @@ Errors.generic msg
type file_path = string

(* Lifting [Stdlib.result] to [Trace.result]. *)

let lift = function
  Stdlib.Ok tree -> Trace.ok tree
| Error msg -> fail msg

(* Parsing contracts *)

let from_file buffer file_path =
  ContractParser.parse_file buffer file_path |> lift

let parse_file = from_file

let from_string buffer = ContractParser.parse_string buffer |> lift

let parse_string = from_string

(* Parsing expressions *)

let expression buffer = ExprParser.parse_string buffer |> lift

let parse_expression = expression

(* Calling the pretty-printers *)

module Pretty    = Parsing_cameligo.Pretty
module MkPretty  = Parsing_shared.Common.MakePretty
module AllPretty = MkPretty (CST) (Pretty)

let pretty_print            = AllPretty.print_cst
let pretty_print_expression = AllPretty.print_expr
let pretty_print_type_expr  = AllPretty.print_type_expr
let pretty_print_pattern    = AllPretty.print_pattern

let pretty_print_file buffer file_path =
  match ContractParser.parse_file buffer file_path with
    Stdlib.Error msg -> fail msg
  | Ok tree -> Trace.ok @@ pretty_print @@ tree
