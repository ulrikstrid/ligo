module type MY_PROTOCOL_IN = module type of Memory_proto_alpha_006_PsCARTHA
(* module type MY_PROTOCOL_OUT = sig
  module Trace : sig
    type tezos_alpha_error
    val trace_tzresult : (tezos_alpha_error list -> 'b) -> ('a, Tezos_error_monad.Error_monad.error list) Stdlib.result -> ('a, 'b) Trace.result 
  end
  module Memory_proto_alpha : sig
    type options

    module Protocol : sig
      module Alpha_context : sig
        module Contract : sig
          type t
        end
      end
    end
  end
end *)

module Make(My_protocol : MY_PROTOCOL_IN) (* : MY_PROTOCOL_OUT *) = struct
  open My_protocol

  module Environment = My_protocol
  module Stdlib_unix = Tezos_stdlib_unix
  module Data_encoding = Data_encoding
  module Crypto = Tezos_crypto
  module Signature = Tezos_base.TzPervasives.Signature
  module Time = Tezos_base.TzPervasives.Time

  open Protocol

  module Error_monad = struct
    include Tezos_error_monad.Error_monad
    include Tezos_utils.Error_monad

    let (>>??) = Alpha_environment.Error_monad.(>>?)

    let alpha_wrap a = Alpha_environment.wrap_error a
    let alpha_error_wrap x = Alpha_environment.Ecoproto_error x

    let force_ok_alpha ~msg a = force_ok ~msg @@ alpha_wrap a

    let force_lwt ~msg a = force_ok ~msg @@ Lwt_main.run a

    let force_lwt_alpha ~msg a = force_ok ~msg @@ alpha_wrap @@ Lwt_main.run a

    let assert_error () = function
      | Ok _ -> fail @@ failure "assert_error"
      | Error _ -> return ()

    let (>>=??) a f =
      a >>= fun a ->
      match alpha_wrap a with
      | Ok result -> f result
      | Error errs -> Lwt.return (Error errs)

  end

  module Trace = struct
    include Simple_utils.Trace

    module AE = Memory_proto_alpha_006_PsCARTHA.Alpha_environment
    module TP = Tezos_error_monad.Error_monad

    type tezos_alpha_error =  [`Tezos_alpha_error of TP.error]

    let of_tz_error (err:Error_monad.error) : tezos_alpha_error =
      `Tezos_alpha_error err

    let of_alpha_tz_error err = of_tz_error (AE.Ecoproto_error err)

    let trace_alpha_tzresult :
      (tezos_alpha_error list -> 'b) -> 'a AE.Error_monad.tzresult -> ('a, 'b) result =
      fun tracer err -> match err with
      | Ok x -> ok x
      | Error errs ->
        fail @@ tracer (List.map of_alpha_tz_error errs)

    let trace_alpha_tzresult_lwt tracer (x:_ AE.Error_monad.tzresult Lwt.t) : _ result =
      trace_alpha_tzresult tracer @@ Lwt_main.run x

    let trace_tzresult :
      (tezos_alpha_error list -> _) -> ('a, Tezos_error_monad.Error_monad.error list) Stdlib.result -> ('a, _) result =
      fun tracer err -> match err with
      | Ok x -> ok x
      | Error errs -> fail @@ tracer (List.map of_tz_error errs)

    let trace_tzresult_lwt err (x:_ TP.tzresult Lwt.t) : _ result =
      trace_tzresult err @@ Lwt_main.run x
  end

  module Init_proto_alpha = struct
    module Signature = Tezos_base.TzPervasives.Signature
    open Error_monad

    module Context_init = struct


      type account = {
          pkh : Signature.Public_key_hash.t ;
          pk :  Signature.Public_key.t ;
          sk :  Signature.Secret_key.t ;
        }

      let generate_accounts n : (account * Tez_repr.t) list =
        let amount = Tez_repr.of_mutez_exn 4_000_000_000_000L in
        List.map (fun _ ->
            let (pkh, pk, sk) = Signature.generate_key () in
            let account = { pkh ; pk ; sk } in
            account, amount)
          (List.range n)

      let make_shell
            ~level ~predecessor ~timestamp ~fitness ~operations_hash =
        Tezos_base.Block_header.{
            level ;
            predecessor ;
            timestamp ;
            fitness ;
            operations_hash ;
            (* We don't care of the following values, only the shell validates them. *)
            proto_level = 0 ;
            validation_passes = 0 ;
            context = Obj.magic 0 ;
        }

      let default_proof_of_work_nonce =
        Bytes.create Alpha_context.Constants.proof_of_work_nonce_size

      let protocol_param_key = [ "protocol_parameters" ]


      let check_constants_consistency constants =
        let open Constants_repr in
        let { blocks_per_cycle ; blocks_per_commitment ;
              blocks_per_roll_snapshot ; _ } = constants in
        unless (blocks_per_commitment <= blocks_per_cycle)
          (fun () -> failwith "Inconsistent constants : blocks per commitment must be \
                              less than blocks per cycle") >>=? fun () ->
        unless (blocks_per_cycle >= blocks_per_roll_snapshot)
          (fun () -> failwith "Inconsistent constants : blocks per cycle \
                              must be superior than blocks per roll snapshot") >>=?
          return


      let initial_context
            constants
            header
            commitments
            initial_accounts
            security_deposit_ramp_up_cycles
            no_reward_cycles
        =
        let open Tezos_base.TzPervasives.Error_monad in
        let bootstrap_accounts =
          List.map (fun ({ pk ; pkh ; _ }, amount) ->
              Parameters_repr.{ public_key_hash = pkh ; public_key = Some pk ; amount }
            ) initial_accounts
        in
        let json =
          Data_encoding.Json.construct
            Parameters_repr.encoding
            Parameters_repr.{
              bootstrap_accounts ;
              bootstrap_contracts = [] ;
              commitments ;
              constants ;
              security_deposit_ramp_up_cycles ;
              no_reward_cycles ;
          }
        in
        let proto_params =
          Data_encoding.Binary.to_bytes_exn Data_encoding.json json
        in
        Tezos_protocol_environment.Context.(
          set Memory_context.empty ["version"] (Bytes.of_string "genesis")
        ) >>= fun ctxt ->
        Tezos_protocol_environment.Context.(
          set ctxt protocol_param_key proto_params
        ) >>= fun ctxt ->
        Main.init ctxt header
        >|= Alpha_environment.wrap_error >>=? fun { context; _ } ->
        return context

      let genesis
            ?(commitments = [])
            ?(security_deposit_ramp_up_cycles = None)
            ?(no_reward_cycles = None)
            (initial_accounts : (account * Tez_repr.t) list)
        =
        if initial_accounts = [] then
          Stdlib.failwith "Must have one account with a roll to bake";

        (* Check there is at least one roll *)
        let constants : Constants_repr.parametric = Tezos_protocol_ligo006_PsCARTHA_parameters.Default_parameters.constants_test in
        check_constants_consistency constants >>=? fun () ->

        let hash =
          Alpha_environment.Block_hash.of_b58check_exn "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
        in
        let shell = make_shell
                      ~level:0l
                      ~predecessor:hash
                      ~timestamp:Tezos_base.TzPervasives.Time.Protocol.epoch
                      ~fitness: (Fitness_repr.from_int64 0L)
                      ~operations_hash: Alpha_environment.Operation_list_list_hash.zero in
        initial_context
          constants
          shell
          commitments
          initial_accounts
          security_deposit_ramp_up_cycles
          no_reward_cycles
        >>=? fun context ->
        return (context, shell, hash)

      let init
            ?(slow=false)
            ?commitments
            n =
        let open Error_monad in
        let accounts = generate_accounts n in
        let contracts = List.map (fun (a, _) ->
                            Alpha_context.Contract.implicit_contract (a.pkh)) accounts in
        begin
          if slow then
            genesis
              ?commitments
              accounts
          else
            genesis
              ?commitments
              accounts
        end >>=? fun ctxt ->
        return (ctxt, accounts, contracts)

      let contents
            ?(proof_of_work_nonce = default_proof_of_work_nonce)
            ?(priority = 0) ?seed_nonce_hash () =
        Alpha_context.Block_header.({
            priority ;
            proof_of_work_nonce ;
            seed_nonce_hash ;
          })


      let begin_construction ?(priority=0) ~timestamp ~(header:Alpha_context.Block_header.shell_header) ~hash ctxt =
        let contents = contents ~priority () in
        let protocol_data =
          let open! Alpha_context.Block_header in {
            contents ;
            signature = Signature.zero ;
          } in
        let timestamp = Alpha_environment.Time.add timestamp @@ Int64.of_int 180 in
        Main.begin_construction
          ~chain_id: Alpha_environment.Chain_id.zero
          ~predecessor_context: ctxt
          ~predecessor_timestamp: header.timestamp
          ~predecessor_fitness: header.fitness
          ~predecessor_level: header.level
          ~predecessor:hash
          ~timestamp
          ~protocol_data
          () >>= fun x -> Lwt.return @@ Alpha_environment.wrap_error x >>=? fun state ->
                          return state.ctxt

      let main n =
        init n >>=? fun ((ctxt, header, hash), accounts, contracts) ->
        let timestamp = Environment.Time.of_seconds @@ Int64.of_float @@ Unix.time () in
        begin_construction ~timestamp ~header ~hash ctxt >>=? fun ctxt ->
        return (ctxt, accounts, contracts)

    end

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

    let init_environment () =
      Context_init.main 10 >>=? fun (tezos_context, accounts, contracts) ->
      let accounts = List.map fst accounts in
      let tezos_context = Alpha_context.Gas.set_limit tezos_context @@ Z.of_int 800000 in
      let identities =
        List.map (fun ((a:Context_init.account), c) -> {
                      public_key = a.pk ;
                      public_key_hash = a.pkh ;
                      secret_key = a.sk ;
                      implicit_contract = c ;
          }) @@
          List.combine accounts contracts in
      return {tezos_context ; identities}

    let contextualize ~msg ?environment f =
      let lwt =
        let environment = match environment with
          | None -> init_environment ()
          | Some x -> return x in
        environment >>=? f
      in
      force_ok ~msg @@ Lwt_main.run lwt

    let dummy_environment =
      force_lwt ~msg:"Init_proto_alpha : initing dummy environment" @@
      init_environment ()

  end

  module Memory_proto_alpha = struct
    module Michelson = Tezos_utils.Michelson

    include Memory_proto_alpha_006_PsCARTHA
    let init_environment = Init_proto_alpha.init_environment
    let dummy_environment = Init_proto_alpha.dummy_environment


    open Protocol
    open Script_typed_ir
    open Script_ir_translator
    open Script_interpreter

    module X = struct
      open Alpha_context
      open Script_tc_errors
      open Alpha_environment.Error_monad

      let rec stack_ty_eq
        : type ta tb. context -> int -> ta stack_ty -> tb stack_ty ->
          ((ta stack_ty, tb stack_ty) eq * context) tzresult
        = fun ctxt lvl ta tb ->
          match ta, tb with
          | Item_t (tva, ra, _), Item_t (tvb, rb, _) ->
            ty_eq ctxt tva tvb |>
            record_trace (Bad_stack_item lvl) >>? fun (Eq, ctxt) ->
            stack_ty_eq ctxt (lvl + 1) ra rb >>? fun (Eq, ctxt) ->
            (Ok (Eq, ctxt) : ((ta stack_ty, tb stack_ty) eq * context) tzresult)
          | Empty_t, Empty_t -> Ok (Eq, ctxt)
          | _, _ -> error Bad_stack_length



      open Script_typed_ir
      open Protocol.Environment.Error_monad
      module Unparse_costs = Michelson_v1_gas.Cost_of.Unparse
      open Protocol.Environment

      type ex_typed_value =
        Ex_typed_value : ('a Script_typed_ir.ty * 'a) -> ex_typed_value

    module Interp_costs = Michelson_v1_gas.Cost_of
    type ex_descr_stack = Ex_descr_stack : (('a, 'b) descr * 'a stack) -> ex_descr_stack

    let unparse_stack ctxt (stack, stack_ty) =
      (* We drop the gas limit as this function is only used for debugging/errors. *)
      let ctxt = Gas.set_unlimited ctxt in
      let rec unparse_stack
        : type a. a stack * a stack_ty -> (Script.expr * string option) list tzresult Lwt.t
        = function
          | Empty, Empty_t -> return_nil
          | Item (v, rest), Item_t (ty, rest_ty, annot) ->
              unparse_data ctxt Readable ty v >>=? fun (data, _ctxt) ->
              unparse_stack (rest, rest_ty) >>=? fun rest ->
              let annot = match Script_ir_annot.unparse_var_annot annot with
                | [] -> None
                | [ a ] -> Some a
                | _ -> assert false in
              let data = Micheline.strip_locations data in
              return ((data, annot) :: rest) in
      unparse_stack (stack, stack_ty)

    end

    open Error_monad

    let stack_ty_eq (type a b)
        ?(tezos_context = dummy_environment.tezos_context)
        (a:a stack_ty) (b:b stack_ty) =
      alpha_wrap (X.stack_ty_eq tezos_context 0 a b) >>? fun (Eq, _) ->
      ok Eq

    let ty_eq (type a b)
        ?(tezos_context = dummy_environment.tezos_context)
        (a:a ty) (b:b ty)
      =
      alpha_wrap (Script_ir_translator.ty_eq tezos_context a b) >>? fun (Eq, _) ->
      ok Eq

    (* should not need lwt *)
    let prims_of_strings michelson =
      let (michelson, errs) =
        Tezos_client_ligo006_PsCARTHA.Michelson_v1_macros.expand_rec michelson in
      match errs with
      | _ :: _ ->
        Lwt.return (Error errs)
      | [] ->
      Lwt.return
        (alpha_wrap
          (Michelson_v1_primitives.prims_of_strings
              (Tezos_micheline.Micheline.strip_locations michelson))) >>=? fun michelson ->
      return (Tezos_micheline.Micheline.root michelson)

    let parse_michelson (type aft)
        ?(tezos_context = dummy_environment.tezos_context)
        ?(top_level = Lambda) michelson
        ?type_logger
        (bef:'a Script_typed_ir.stack_ty) (aft:aft Script_typed_ir.stack_ty)
      =
      prims_of_strings michelson >>=? fun michelson ->
      parse_instr
        ?type_logger
        top_level tezos_context
        michelson bef ~legacy:false >>=?? fun (j, _) ->
      match j with
      | Typed descr -> (
          Lwt.return (
            alpha_wrap (X.stack_ty_eq tezos_context 0 descr.aft aft) >>? fun (Eq, _) ->
            let descr : (_, aft) Script_typed_ir.descr = {descr with aft} in
            Ok descr
          )
        )
      | _ -> Lwt.return @@ error_exn (Failure "Typing instr failed")

    let parse_michelson_fail (type aft)
        ?(tezos_context = dummy_environment.tezos_context)
        ?(top_level = Lambda) michelson
        ?type_logger
        (bef:'a Script_typed_ir.stack_ty) (aft:aft Script_typed_ir.stack_ty)
      =
      prims_of_strings michelson >>=? fun michelson ->
      parse_instr
        ?type_logger
        top_level tezos_context
        michelson bef ~legacy:false >>=?? fun (j, _) ->
      match j with
      | Typed descr -> (
          Lwt.return (
            alpha_wrap (X.stack_ty_eq tezos_context 0 descr.aft aft) >>? fun (Eq, _) ->
            let descr : (_, aft) Script_typed_ir.descr = {descr with aft} in
            Ok descr
          )
        )
      | Failed { descr } ->
          Lwt.return (Ok (descr aft))

    let parse_michelson_data
        ?(tezos_context = dummy_environment.tezos_context)
        michelson ty =
      parse_data tezos_context ty michelson ~legacy:false >>=?? fun (data, _) ->
      return data

    let parse_michelson_ty
        ?(tezos_context = dummy_environment.tezos_context)
        ?(allow_big_map = true) ?(allow_operation = true) ?(allow_contract = true)
        michelson =
      Lwt.return @@ parse_ty tezos_context ~allow_big_map ~allow_operation michelson ~legacy:false ~allow_contract >>=?? fun (ty, _) ->
      return ty

    let strings_of_prims michelson =
      let michelson = Tezos_micheline.Micheline.strip_locations michelson in
      let michelson = Michelson_v1_primitives.strings_of_prims michelson in
      Tezos_micheline.Micheline.root michelson

    let unparse_michelson_data
        ?(tezos_context = dummy_environment.tezos_context)
        ty value =
      unparse_data tezos_context
        Readable ty value >>=?? fun (michelson, _) ->
      return (strings_of_prims michelson)

    let unparse_michelson_ty
        ?(tezos_context = dummy_environment.tezos_context)
        ty =
      Script_ir_translator.unparse_ty tezos_context ty >>=?? fun (michelson, _) ->
      return (strings_of_prims michelson)

    type options = {
      tezos_context: Alpha_context.t ;
      source: Alpha_context.Contract.t ;
      payer: Alpha_context.Contract.t ;
      self: Alpha_context.Contract.t ;
      amount: Alpha_context.Tez.t ;
      chain_id: Environment.Chain_id.t ;
      balance : Alpha_context.Tez.t;
      now : Alpha_context.Script_timestamp.t;
    }

    let make_options
        ?(tezos_context = dummy_environment.tezos_context)
        ?(now = Alpha_context.Script_timestamp.now dummy_environment.tezos_context)
        ?(sender = (List.nth dummy_environment.identities 0).implicit_contract)
        ?(self = (List.nth dummy_environment.identities 0).implicit_contract)
        ?(source = (List.nth dummy_environment.identities 1).implicit_contract)
        ?(amount = Alpha_context.Tez.one)
        ?(balance = Alpha_context.Tez.zero)
        ?(chain_id = Environment.Chain_id.zero)
        ()
      =
      {
        tezos_context ;
        source = sender ;
        payer = source ;
        self ;
        amount ;
        chain_id ;
        balance ;
        now ;
      }

    let default_options = make_options ()

    let interpret ?(options = default_options) (instr:('a, 'b) descr) (bef:'a stack) : 'b stack tzresult Lwt.t  =
      let {
        tezos_context ;
        source ;
        self ;
        payer ;
        amount ;
        chain_id ;
        balance ;
        now ;
      } = options in
      let step_constants = { source ; self ; payer ; amount ; chain_id ; balance ; now } in
      Script_interpreter.step tezos_context step_constants instr bef >>=??
      fun (stack, _) -> return stack

    let unparse_ty_michelson ty =
      Script_ir_translator.unparse_ty dummy_environment.tezos_context ty >>=??
      fun (n,_) -> return n

    type typecheck_res =
      | Type_checked
      | Err_parameter | Err_storage | Err_contract
      | Err_gas
      | Err_unknown

    let typecheck_contract contract =
      let contract' = Tezos_micheline.Micheline.strip_locations contract in
      Script_ir_translator.typecheck_code dummy_environment.tezos_context contract' >>= fun x ->
      match x with
      | Ok _ -> return @@ contract
      | Error errs -> Lwt.return @@ Error (List.map (alpha_error_wrap) errs)

    let assert_equal_michelson_type ty1 ty2 =
      (* alpha_wrap (Script_ir_translator.ty_eq tezos_context a b) >>? fun (Eq, _) -> *)
      alpha_wrap (Script_ir_translator.ty_eq dummy_environment.tezos_context ty1 ty2)

    type 'a interpret_res =
      | Succeed of 'a stack
      | Fail of Script_repr.expr

    let failure_interpret
        ?(options = default_options)
        (instr:('a, 'b) descr)
        (bef:'a stack) : 'b interpret_res tzresult Lwt.t =
      let {
        tezos_context ;
        source ;
        self ;
        payer ;
        amount ;
        chain_id ;
        balance ;
        now ;
      } = options in
      let step_constants = { source ; self ; payer ; amount ; chain_id ; balance ; now } in
      Script_interpreter.step tezos_context step_constants instr bef >>= fun x ->
      match x with
      | Ok (s , _ctxt) -> return @@ Succeed s
      | Error ((Reject (_, expr, _))::_t) -> return @@ Fail expr (* This catches failwith errors *)
      | Error errs -> Lwt.return @@ Error (List.map (alpha_error_wrap) errs)

    let pack (data_ty: 'a ty) (data: 'a) : bytes tzresult Lwt.t =
      pack_data dummy_environment.tezos_context data_ty data >>=?? fun (packed,_) -> return packed

    let strings_of_prims = Michelson_v1_primitives.strings_of_prims

    let to_hex = fun michelson ->
      let michelson =
        force_lwt ~msg:"Internal error: could not serialize Michelson"
          (prims_of_strings michelson) in
      let canonical = Tezos_micheline.Micheline.strip_locations michelson in
      let bytes = Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding canonical in
      Hex.of_bytes bytes
  end

  module Measure = struct
    open Error_monad
    open Tezos_micheline.Micheline
    open Memory_proto_alpha.Protocol

    let measure = fun michelson ->
      Memory_proto_alpha.prims_of_strings michelson >>=? fun michelson ->
      let canonical = strip_locations michelson in
      let bytes = Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding canonical in
      return (Bytes.length bytes)
  end

end

include Make(Memory_proto_alpha_006_PsCARTHA)