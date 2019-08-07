open Mini_c.Types
open Memory_proto_alpha
open Proto_alpha_utils.Trace

val translate_value : Script_ir_translator.ex_typed_value -> value result
