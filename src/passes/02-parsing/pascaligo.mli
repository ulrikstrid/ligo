(* Interfacing the PascaLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module CST = Cst.Pascaligo

(* Results and errors *)

type error  = Errors.parse_error
type cst    = (CST.t, error) Trace.result
type expr   = (CST.expr, error) Trace.result
type buffer = (Buffer.t, error) Trace.result

(* Some parameter types *)

type file_path = string
type dirs      = file_path list (* For #include directives *)

(* Parsing *)

val parse_file       : dirs -> Buffer.t -> file_path -> cst
val parse_contract   : dirs -> Buffer.t -> cst
val parse_expression : dirs -> Buffer.t -> expr

(* Pretty-printing *)

val pretty_print             : CST.t -> Buffer.t
val pretty_print_expression  : CST.expr -> Buffer.t
val pretty_print_pattern     : CST.pattern -> Buffer.t
val pretty_print_type_expr   : CST.type_expr -> Buffer.t
val pretty_print_file        : dirs -> Buffer.t -> file_path -> buffer
