open Errors
open Trace
open Mini_c

open Michelson
open Memory_proto_alpha.Protocol.Script_ir_translator
open Predefined.Stacking

(*
module Contract_types = Meta_michelson.Types
module Stack = Meta_michelson.Stack
*)
type compiled_expression = {
  expr_ty : ex_ty ;
  expr : michelson ;
}


val get_operator : constant' -> type_expression -> expression list -> (predicate, stacking_error) result

val translate_expression : expression -> environment -> (michelson, stacking_error) result

val translate_function_body : anon_function -> environment_element list -> type_expression -> (michelson, stacking_error) result

val translate_value : value -> type_expression -> (michelson, stacking_error) result