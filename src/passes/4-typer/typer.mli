val use_new_typer : bool

open Trace

module I = Ast_simplified
module O = Ast_typed

module SMap = O.SMap
module Environment = O.Environment

type state
val initial_state : state
val discard_state : state -> unit

type environment = Environment.t

val type_program : I.program -> state -> (O.program * state) result
val type_expression : environment -> state -> ?tv_opt:O.type_value -> I.expression -> (O.annotated_expression * state) result
val untype_expression : O.annotated_expression -> I.expression result
