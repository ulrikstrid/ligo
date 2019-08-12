module Signature = Tezos_base.TzPervasives.Signature
open Proto_alpha_utils.Memory_proto_alpha
module Data_encoding = Alpha_environment.Data_encoding
module MBytes = Alpha_environment.MBytes
module Error_monad = Proto_alpha_utils.Error_monad
open Error_monad

(*
module Context_init : sig

  type account = {
      pkh : Signature.Public_key_hash.t ;
      pk :  Signature.Public_key.t ;
      sk :  Signature.Secret_key.t ;
    }

  val generate_accounts : int -> (account * Tez_repr.t) list

  val make_shell : 
        level:int32 ->
        predecessor:Tezos_crypto.Block_hash.t ->
        timestamp:Proto_alpha_utils.Time.t ->
        fitness:Tezos_base.Fitness.t ->
        operations_hash:Tezos_crypto.Operation_list_list_hash.t ->
        Tezos_base.Block_header.shell_header

  val default_proof_of_work_nonce : MBytes.t 

  val protocol_param_key : string list

  val check_constants_consistency :
        Memory_proto_alpha.Constants_repr.parametric ->
        unit tzresult Lwt.t

  val initial_context : 
        Memory_proto_alpha.Constants_repr.parametric ->
        Tezos_base.Block_header.shell_header ->
        Memory_proto_alpha.Commitment_repr.t list ->
        (account * Memory_proto_alpha.Tez_repr.tez) list ->
        int option ->
        int option ->
        Tezos_protocol_environment_memory.Context.t
        Tezos_base.TzPervasives.Error_monad.tzresult Lwt.t


  val genesis : 
        ?preserved_cycles:int ->
        ?blocks_per_cycle:int32 ->
        ?blocks_per_commitment:int32 ->
        ?blocks_per_roll_snapshot:int32 ->
        ?blocks_per_voting_period:int32 ->
        ?time_between_blocks:Memory_proto_alpha.Period_repr.t list ->
        ?endorsers_per_block:int ->
        ?hard_gas_limit_per_operation:Z.t ->
        ?hard_gas_limit_per_block:Z.t ->
        ?proof_of_work_threshold:int64 ->
        ?tokens_per_roll:Memory_proto_alpha.Tez_repr.tez ->
        ?michelson_maximum_type_size:int ->
        ?seed_nonce_revelation_tip:Memory_proto_alpha.Tez_repr.tez ->
        ?origination_size:int ->
        ?block_security_deposit:Memory_proto_alpha.Tez_repr.tez ->
        ?endorsement_security_deposit:Memory_proto_alpha.Tez_repr.tez ->
        ?block_reward:Memory_proto_alpha.Tez_repr.tez ->
        ?endorsement_reward:Memory_proto_alpha.Tez_repr.tez ->
        ?cost_per_byte:Memory_proto_alpha.Tez_repr.tez ->
        ?hard_storage_limit_per_operation:Z.t ->
        ?commitments:Memory_proto_alpha.Commitment_repr.t list ->
        ?security_deposit_ramp_up_cycles:int option ->
        ?no_reward_cycles:int option ->
        (account * Memory_proto_alpha.Tez_repr.tez) list ->
        (Tezos_protocol_environment_memory.Context.t *
        Tezos_base.Block_header.shell_header * Tezos_crypto.Block_hash.t)
        Tezos_base.TzPervasives.Error_monad.tzresult Lwt.t


  val init : 
        ?slow:bool ->
        ?preserved_cycles:int ->
        ?endorsers_per_block:int ->
        ?commitments:Memory_proto_alpha.Commitment_repr.t list ->
        int ->
        ((Tezos_protocol_environment_memory.Context.t *
        Tezos_base.Block_header.shell_header * Tezos_crypto.Block_hash.t) *
        (account * Memory_proto_alpha.Tez_repr.tez) list *
        Memory_proto_alpha.Alpha_context.Contract.t list)
        tzresult Lwt.t

  val contents:
        ?proof_of_work_nonce:MBytes.t ->
        ?priority:int -> 
        ?seed_nonce_hash:Nonce_hash.t -> 
        unit -> 
        Alpha_context.Block_header.contents

  val begin_construction: 
        ?priority:int -> 
        timestamp:Proto_alpha_utils.Time.t ->
        header:Tezos_base.Block_header.shell_header ->
        hash:Tezos_crypto.Block_hash.t ->
        Tezos_protocol_environment_memory.Context.t ->
        Alpha_context.t tzresult Lwt.t


  val main : int -> 
        ( Alpha_context.t * (account * Tez_repr.tez) list * Alpha_context.Contract.t list) tzresult Lwt.t

end
*)

type identity = {
  public_key_hash : Signature.public_key_hash;
  public_key : Signature.public_key;
  secret_key : Signature.secret_key;
  implicit_contract : Alpha_context.Contract.t;
}

type environment = {
  tezos_context : Alpha_context.t ;
  identities : identity list ;
}
val init_environment : unit -> environment tzresult Lwt.t

val contextualize : msg:string -> ?environment:environment -> 
 (environment -> 'a tzresult Lwt.t) -> 'a
