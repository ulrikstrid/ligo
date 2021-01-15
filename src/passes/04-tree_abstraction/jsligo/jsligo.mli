[@@@warning "-45"]

open Trace

module CST = Cst.Jsligo
module AST = Ast_imperative
module Errors = Errors

val compile_expression :
  CST.expr -> (AST.expr, Errors.abs_error) result

val compile_module :
  CST.ast -> (AST.module_, Errors.abs_error) result
