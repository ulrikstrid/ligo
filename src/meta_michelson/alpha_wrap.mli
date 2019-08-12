module Proto_alpha = Proto_alpha_utils.Memory_proto_alpha
open Proto_alpha

(*
open Alpha_context
open Alpha_environment

val dummy_environment : Misc.environment
val tc : t
*)

val pack : 'a Script_typed_ir.ty -> 'a -> Raw_context.value

(*
val unpack_opt : 'a Script_typed_ir.ty -> MBytes.t -> 'a option 
*)

val unpack : 'a Proto_alpha.Script_typed_ir.ty -> Proto_alpha.Raw_context.value -> 'a

val blake2b : Proto_alpha.Raw_context.value -> Proto_alpha.Raw_context.value
