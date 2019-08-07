open Trace
open Mini_c.Types

open Proto_alpha_utils.Memory_proto_alpha
open Script_ir_translator

module O = Tezos_utils.Michelson
module Contract_types = Meta_michelson.Types

module Ty : sig
  val type_ : type_value -> ex_ty result

  val environment_representation : environment -> Script_ir_translator.ex_ty result

  val environment : environment -> Meta_michelson.Stack.ex_stack_ty result
  (*
  val not_comparable : string -> unit -> error
  val not_compilable_type : string -> unit -> error

  val comparable_type_base : type_base -> ex_comparable_ty result

  val comparable_type : type_value -> ex_comparable_ty result

  val base_type : type_base -> ex_ty result

  *)
end

val type_ : type_value -> O.michelson result

val environment_element : string * type_value -> O.michelson result

val environment : ( 'a * type_value ) list -> O.michelson list result 

val environment_closure : environment ->  O.michelson result
(*
val base_type : type_base -> O.michelson result

*)
