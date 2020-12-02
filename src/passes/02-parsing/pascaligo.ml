(* This file provides an interface to the PascaLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace
module Utils = Simple_utils.Utils

(* Internal dependencies *)

module File        = Lexer_pascaligo.File
module Comments    = Lexer_pascaligo.Comments
module Token       = Lexer_pascaligo.Token
module Self_lexing = Lexer_pascaligo.Self_lexing
module Scoping     = Parser_pascaligo.Scoping
module ParErr      = Parser_pascaligo.ParErr
module MakeParser  = Shared_parser.Common.MakeParser
module MkParser    = MakeParser
                       (File) (Comments) (Token) (Scoping) (ParErr)
module CST         = Cst.Pascaligo

(* Parser for contracts *)

module ContractParser_Menhir =
  struct
    include Parser_pascaligo.Parser
    type tree = CST.t

    let main = contract

    module Incremental =
      struct
        let main = Incremental.contract
      end
  end

module ContractParser = MkParser (ContractParser_Menhir) (Self_lexing)

(* Parser for expressions *)

module ExprParser_Menhir =
  struct
    include Parser_pascaligo.Parser
    type tree = CST.expr

    let main = interactive_expr

    module Incremental =
      struct
        let main = Incremental.interactive_expr
      end
  end

module ExprParser = MkParser (ExprParser_Menhir) (Self_lexing)

(* Results and errors *)

type error  = Errors.parse_error
type cst    = (CST.t,    error) Trace.result
type expr   = (CST.expr, error) Trace.result
type buffer = (Buffer.t, error) Trace.result

let fail msg = Trace.fail @@ Errors.generic msg
type file_path = string
type dirs      = file_path list (* For #include directives *)

(* Calling the parsers *)

let filter = function
  Stdlib.Error msg -> fail msg
| Stdlib.Ok thunk ->
    match thunk () with
      Stdlib.Ok tree -> Trace.ok tree
    | Stdlib.Error msg -> fail msg

(* Parsing contracts *)

let parse_file   = Utils.(filter <@ ContractParser.parse_file)
let parse_string = Utils.(filter <@ ContractParser.parse_string)

(* Parsing expressions *)

let parse_expression = Utils.(filter <@ ExprParser.parse_string)

(* Calling the pretty-printers *)

module Pretty    = Parser_pascaligo.Pretty
module MkPretty  = Shared_parser.Common.MakePretty
module AllPretty = MkPretty (CST) (Pretty)

let pretty_print            = AllPretty.print_cst
let pretty_print_expression = AllPretty.print_expr
let pretty_print_pattern    = AllPretty.print_pattern
let pretty_print_type_expr  = AllPretty.print_type_expr

let pretty_print_file dirs buffer file_path =
  match ContractParser.parse_file dirs buffer file_path with
    Stdlib.Error msg -> fail msg
  | Ok thunk ->
      match thunk () with
        Stdlib.Ok tree -> Trace.ok @@ pretty_print @@ tree
      | Stdlib.Error msg -> fail msg
