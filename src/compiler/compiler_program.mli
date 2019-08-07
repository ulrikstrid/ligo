open Trace
open Mini_c

open Michelson
open Memory_proto_alpha.Script_ir_translator

module Contract_types = Meta_michelson.Types
module Stack = Meta_michelson.Stack
type compiled_program = {
  input : ex_ty ;
  output : ex_ty ;
  body : michelson ;
}

val translate_value : value -> michelson result 

val translate_program : program -> string -> compiled_program result

val translate_contract : anon_function -> michelson result

val translate_entry : anon_function -> compiled_program result

(*

open Operators.Compiler

val get_predicate : string -> type_value -> expression list -> predicate result

val translate_function : anon_function -> michelson result

val translate_expression : ?push_var_name:string -> expression -> environment -> ( michelson * environment ) result

val translate_quote_body : anon_function -> michelson result

val get_main : program -> string -> anon_function result


module Errors : sig
  val corner_case : loc:string -> string -> unit -> error
end

*)
