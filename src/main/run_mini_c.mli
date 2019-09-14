open Proto_alpha_utils
open Trace
open Mini_c
open! Compiler.Program
(*
open Memory_proto_alpha.Protocol.Script_ir_translator
*)
open Memory_proto_alpha.X

val run_aux : ?options:Proto_alpha_utils.Memory_proto_alpha.options -> compiled_program -> Michelson.t -> ex_typed_value result

val run_entry : ?debug_michelson:bool -> ?options:Proto_alpha_utils.Memory_proto_alpha.options -> anon_function -> type_value * type_value ->  value -> value result
