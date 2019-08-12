open Misc

open Memory_proto_alpha
open Alpha_context

open Script_typed_ir
open Error_monad

module Option = Simple_utils.Option
module Cast = Proto_alpha_utils.Cast

type ('param, 'storage) toplevel = {
  param_type : 'param ty ;
  storage_type : 'storage ty ;
  code : ('param * 'storage, packed_internal_operation list * 'storage) lambda
}

type ex_toplevel =
    Ex_toplevel : ('a, 'b) toplevel -> ex_toplevel

val get_toplevel : ?environment:environment -> string -> 'a ty -> 'b ty -> ('b, 'a) toplevel 

val make_toplevel :  ('a * 'b, packed_internal_operation list * 'b) lambda -> 'b ty -> 'a ty -> ('a, 'b) toplevel

module type ENVIRONMENT = sig
  val identities : identity list
  val tezos_context : t
end

type ex_typed_stack = Ex_typed_stack : ('a stack_ty * 'a Script_interpreter.stack) -> ex_typed_stack

module Step (Env: ENVIRONMENT) : sig
  type config = {
    source : Contract.t option ;
    payer : Contract.t option ;
    self : Contract.t option ;
    visitor : (Script_interpreter.ex_descr_stack -> unit) option ;
    timestamp : Script_timestamp.t option ;
    debug_visitor : (ex_typed_stack -> unit) option ;
    amount : Tez.t option ;
  }

  (*
  val no_config : config
  *)
  
  val of_param : config option -> config option -> config option
  val make_config : 
    ?base_config : config -> 
    ?source : Contract.t ->
    ?payer : Contract.t ->
    ?self : Contract.t ->
    ?visitor : (Script_interpreter.ex_descr_stack -> unit) ->
    ?debug_visitor : (ex_typed_stack -> unit) ->
    ?timestamp : Script_timestamp.t ->
    ?amount : Tez.t ->
    unit -> config

  (*
  open Error_monad

  val debug_visitor : ?f:(ex_typed_stack -> unit  ) -> unit -> Script_interpreter.ex_descr_stack -> unit
  val step_lwt : ?config:config -> 'a Script_interpreter.stack -> ('a, 'b) descr -> 'b Script_interpreter.stack tzresult Lwt.t
  val step_1_1 : ?config:config -> 'a -> ('a * end_of_stack, 'b * end_of_stack) descr -> 'b tzresult Lwt.t
  *)

  val step_1_2 : ?config:config -> 'a -> ('a * end_of_stack, 'b * ('c * end_of_stack)) descr -> ('b * 'c) tzresult Lwt.t
  val step_3_1 : ?config:config -> 'a -> 'b -> 'c -> ('a * ('b * ('c * end_of_stack)), 'd * end_of_stack) descr -> 'd tzresult Lwt.t
  val step_2_1 : ?config:config -> 'a -> 'b -> ('a * ('b * end_of_stack), 'c * end_of_stack) descr -> 'c tzresult Lwt.t
  val step_value : ?config:config -> 'a -> ('a * end_of_stack, 'a * end_of_stack) descr -> 'a tzresult Lwt.t
  val step : ?config:config -> 'a Script_interpreter.stack -> ('a, 'b) descr -> 'b Script_interpreter.stack
end

val run_lwt_full : ?source:Contract.t -> ?payer:Contract.t ->  ?self:Contract.t -> ('a,'b) toplevel -> 'b -> 'a -> environment ->
 ('b * packed_internal_operation list * Z.t) tzresult Lwt.t
val run_lwt : ?source:Contract.t -> ?payer:Contract.t -> ?self:Contract.t -> ('a, 'b) toplevel -> 'b -> 'a -> environment -> 'b tzresult Lwt.t
val run : ?environment:environment -> ('a,'b) toplevel -> 'b -> 'a -> 'b
val run_node : ?environment: environment -> ('a, 'b) toplevel -> Script.node -> Script.node -> Script.node
val run_str: ('a, 'b) toplevel -> string -> string -> Script.node

type input = {
  toplevel_path : string ;
  storage : string ;
  parameter : string
}

val parse_json : string -> input
val generate_json : Script.node -> string

module Types : sig
  open Script_typed_ir

  val union : 'a ty -> 'b ty -> ( 'a , 'b ) union ty
  val assert_union : ( 'a , 'b ) union ty -> 'a ty * 'b ty

  val pair : 'a ty -> 'b ty -> ('a, 'b ) pair ty
  val assert_pair : ( 'a , 'b ) pair ty -> 'a ty * 'b ty
  val assert_pair_ex : ?msg:string -> Script_ir_translator.ex_ty -> Script_ir_translator.ex_ty * Script_ir_translator.ex_ty

  val unit : unit ty

  val bytes : Raw_context.value ty
  val bytes_k : Raw_context.value comparable_ty

  val nat : Script_int.n Script_int.num ty
  val tez : Tez.tez ty
  val int : Script_int.z Script_int.num ty
  val nat_k : Script_int.n Script_int.num comparable_ty
  val tez_k : Tez.tez comparable_ty
  val int_k : Script_int.z Script_int.num comparable_ty

  val big_map : 'a comparable_ty -> 'b ty -> ( 'a , 'b ) big_map ty 

  val signature : signature ty
  val operation : packed_internal_operation ty

  val bool : bool ty

  val mutez : Tez.tez ty

  val string : string ty
  val string_k : string comparable_ty
  val address_k : Contract.t comparable_ty

  val key : public_key ty

  val list: 'a ty -> 'a list ty
  val set : 'a comparable_ty -> 'a set ty
  val assert_list : 'a list ty -> 'a ty

  val option : 'a ty -> 'a option ty
  val contract: 'a ty -> 'a typed_contract ty
  val assert_option : 'a option ty -> 'a ty

  val address : Contract.t ty

  val lambda : 'a ty -> 'b ty -> ( 'a, 'b) lambda ty
  val assert_lambda : ( 'a ,'b) lambda ty -> 'a ty * 'b ty
  type ex_lambda = Ex_lambda : (_, _) lambda ty -> ex_lambda
  val is_lambda : 'a ty -> ex_lambda option

  val timestamp : Script_timestamp.t ty 
  val timestamp_k : Script_timestamp.t comparable_ty

  val map : 'a comparable_ty -> 'b ty -> ('a,'b) map ty

  val assert_type : 'a ty -> 'a -> unit
end

module Values : sig
  (*
  val empty_map : 'a comparable_ty -> ('a , 'b) map
  *)
  val empty_big_map : 'a ty -> 'a comparable_ty -> 'b ty -> ('a, 'b) big_map

  val int : int -> Script_int.z Script_int.num

  val nat : int -> Script_int.n Script_int.num
  val nat_to_int : 'a Script_int.num -> int

  val tez : int -> Tez.tez

  val left : 'a -> ( 'a, 'b) union
  val right: 'b -> ( 'a, 'b) union
end
