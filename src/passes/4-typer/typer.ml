let use_new_typer = false

module I = Ast_simplified
module O = Ast_typed

module SMap = O.SMap
module Environment = O.Environment

type state = Typer_new.Solver.state (* Both the old typer and the new typer use the same solver state. *)
let initial_state = Typer_new.Solver.initial_state
let discard_state = Typer_new.Solver.discard_state

type environment = Environment.t

let type_program = if use_new_typer then Typer_new.type_program else Typer_old.type_program
let type_expression = if use_new_typer then Typer_new.type_expression else Typer_old.type_expression
let untype_expression = if use_new_typer then Typer_new.untype_expression else Typer_old.untype_expression
