(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script
open Script_typed_ir

(* ---- Run-time errors -----------------------------------------------------*)

type execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

module Error = struct
  type t =
    | Bad_contract_parameter of Contract.t (* `Permanent *)
    | Cannot_serialize_failure
    | Cannot_serialize_log
    | Cannot_serialize_storage
    | Overflow of Script.location * execution_trace option
    | Reject of Script.location * Script.expr * execution_trace option
    | Runtime_contract_error of Contract.t * Script.expr
end

type error += Script_interpreter_error of Error.t

let () =
  let open Data_encoding in
  let trace_encoding =
    (list @@ obj3
       (req "location" Script.location_encoding)
       (req "gas" Gas.encoding)
       (req "stack"
          (list
             (obj2
                (req "item" (Script.expr_encoding))
                (opt "annot" string))))) in
  (* Reject *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.script_rejected"
    ~title: "Script failed"
    ~description: "A FAILWITH instruction was reached"
    (obj3
       (req "location" Script.location_encoding)
       (req "with" Script.expr_encoding)
       (opt "trace" trace_encoding))
    (function Script_interpreter_error (Error.Reject (loc, v, trace)) -> Some (loc, v, trace) | _ -> None)
    (fun (loc, v, trace) -> Script_interpreter_error (Error.Reject (loc, v, trace)));
  (* Overflow *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.script_overflow"
    ~title: "Script failed (overflow error)"
    ~description: "A FAIL instruction was reached due to the detection of an overflow"
    (obj2
       (req "location" Script.location_encoding)
       (opt "trace" trace_encoding))
    (function Script_interpreter_error (Error.Overflow (loc, trace)) -> Some (loc, trace) | _ -> None)
    (fun (loc, trace) -> Script_interpreter_error (Error.Overflow (loc, trace)));
  (* Runtime contract error *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.runtime_error"
    ~title: "Script runtime error"
    ~description: "Toplevel error for all runtime script errors"
    (obj2
       (req "contract_handle" Contract.encoding)
       (req "contract_code" Script.expr_encoding))
    (function
      | Script_interpreter_error (Error.Runtime_contract_error (contract, expr)) ->
          Some (contract, expr)
      | _ -> None)
    (fun (contract, expr) ->
      Script_interpreter_error (Error.Runtime_contract_error (contract, expr)));
  (* Bad contract parameter *)
  register_error_kind
    `Permanent
    ~id:"michelson_v1.bad_contract_parameter"
    ~title:"Contract supplied an invalid parameter"
    ~description:"Either no parameter was supplied to a contract with \
                  a non-unit parameter type, a non-unit parameter was \
                  passed to an account, or a parameter was supplied of \
                  the wrong type"
    Data_encoding.(obj1 (req "contract" Contract.encoding))
    (function Script_interpreter_error (Error.Bad_contract_parameter c) -> Some c | _ -> None)
    (fun c -> Script_interpreter_error (Error.Bad_contract_parameter c));
  (* Cannot serialize log *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_log"
    ~title:"Not enough gas to serialize execution trace"
    ~description:"Execution trace with stacks was to big to be serialized with \
                  the provided gas"
    Data_encoding.empty
    (function Script_interpreter_error Error.Cannot_serialize_log -> Some () | _ -> None)
    (fun () -> Script_interpreter_error Error.Cannot_serialize_log) ;
  (* Cannot serialize failure *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_failure"
    ~title:"Not enough gas to serialize argument of FAILWITH"
    ~description:"Argument of FAILWITH was too big to be serialized with \
                  the provided gas"
    Data_encoding.empty
    (function Script_interpreter_error Error.Cannot_serialize_failure -> Some () | _ -> None)
    (fun () -> Script_interpreter_error Error.Cannot_serialize_failure) ;
  (* Cannot serialize storage *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_storage"
    ~title:"Not enough gas to serialize execution storage"
    ~description:"The returned storage was too big to be serialized with \
                  the provided gas"
    Data_encoding.empty
    (function Script_interpreter_error Error.Cannot_serialize_storage -> Some () | _ -> None)
    (fun () -> Script_interpreter_error Error.Cannot_serialize_storage)

(* ---- Monad ---------------------------------------------------------------*)

module Command = struct
  type 'a t =
    | Assert_false : 'a t
    | Big_map_collect : 'a Script_typed_ir.ty * 'a -> Script_ir_translator.big_map_ids t
    | Big_map_extract_diff
      : Script_ir_translator.unparsing_mode *
        bool *
        Script_ir_translator.big_map_ids *
        Script_ir_translator.big_map_ids *
        'a Script_typed_ir.ty *
        'a ->
        ('a * Contract.big_map_diff option) t
    | Big_map_get : 'k * ('k, 'v) big_map -> 'v option t
    | Big_map_mem : 'k * ('k, 'v) big_map -> bool t
    | Fail_overflow : Script.location -> 'a t
    | Fail_reject : Script.location * Script.expr -> 'a t
    | Fresh_contract_from_current_nonce : Contract.t t
    | Fresh_internal_nonce : int t
    | Gas_check_enough : Gas.cost -> unit t
    | Gas_consume : Gas.cost -> unit t
    | Gas_level : Gas.t t
    | Get_balance : Contract.t -> Tez.t t
    | Legacy_add_do :
      Signature.Public_key_hash.t * Script_repr.lazy_expr * Script_repr.lazy_expr ->
      (Script_repr.lazy_expr * Script_repr.lazy_expr) t
    | Legacy_add_root_entrypoint: Script_repr.lazy_expr -> Script_repr.lazy_expr t
    | Legacy_add_set_delegate :
      Signature.Public_key_hash.t * Script_repr.lazy_expr * Script_repr.lazy_expr ->
      (Script_repr.lazy_expr * Script_repr.lazy_expr) t
    | Lift_tz_result : 'a tzresult -> 'a t
    | Log : Script.location * Gas.t * (Script.expr * string option) list -> unit t
    | Now : Script_timestamp.t t
    | Parse_contract_for_script : bool * Script.location * 'a ty * Contract.t * string -> 'a typed_contract option t
    | Parse_data : Script_ir_translator.type_logger * bool * 'a ty * Script.node -> 'a t
    | Serialize_pack_data : 'a ty * 'a -> MBytes.t t
    | Serialize_unpack_data : 'a ty * Script.node -> 'a option t
    | Unparse_data : Script_ir_translator.unparsing_mode * 'a ty * 'a -> Script.node t
    | Unparse_ty : 'a ty -> Script.node t

  let eval
    : type a.
      a t ->
      context ->
      execution_trace ref option ->
      (a * context) tzresult Lwt.t
    = fun command ctxt log ->
    let get_log (log : execution_trace ref option) =
      Option.map ~f:(fun l -> List.rev !l) log in
    match command with
    | Assert_false -> assert false
    | Big_map_collect (ty, x) ->
      Script_ir_translator.collect_big_maps ctxt ty x
    | Big_map_extract_diff (mode, temporary, to_duplicate, to_update, ty, v) ->
      Script_ir_translator.extract_big_map_diff
        ctxt
        mode
        temporary
        to_duplicate
        to_update
        ty
        v >>=? fun (p, big_map_diff, ctxt) ->
      Error_monad.return ((p, big_map_diff), ctxt)
    | Big_map_get (key, big_map) ->
      Script_ir_translator.big_map_get ctxt key big_map
    | Big_map_mem (key, big_map) ->
      Script_ir_translator.big_map_mem ctxt key big_map
    | Fail_overflow location ->
      Error_monad.fail (Script_interpreter_error (Error.Overflow (location, get_log log)))
    | Fail_reject (location, e) ->
      Error_monad.fail (Script_interpreter_error (Error.Reject (location, e, get_log log)))
    | Fresh_contract_from_current_nonce ->
      Contract.fresh_contract_from_current_nonce ctxt >>=? fun (ctxt, contract) ->
      Error_monad.return (contract, ctxt)
    | Fresh_internal_nonce ->
      Lwt.return (fresh_internal_nonce ctxt) >>=? fun (ctxt, nonce) ->
      Error_monad.return (nonce, ctxt)
    | Gas_check_enough cost ->
      Lwt.return (Gas.check_enough ctxt cost) >>=? fun () ->
      Error_monad.return ((), ctxt)
    | Gas_consume cost ->
      Lwt.return (Gas.consume ctxt cost) >>=? fun ctxt ->
      Error_monad.return ((), ctxt)
    | Gas_level ->
      let gas_level = Gas.level ctxt in
      Error_monad.return (gas_level, ctxt)
    | Get_balance contract ->
      Contract.get_balance ctxt contract >>=? fun balance ->
      Error_monad.return (balance, ctxt)
    | Legacy_add_do (manager_pkh, script_code, script_storage) ->
      Legacy_support.add_do ~manager_pkh ~script_code ~script_storage >>=? fun (code, storage) ->
      Error_monad.return ((code, storage), ctxt)
    | Legacy_add_root_entrypoint script_code ->
      Legacy_support.add_root_entrypoint script_code >>=? fun code ->
      Error_monad.return (code, ctxt)
    | Legacy_add_set_delegate (manager_pkh, script_code, script_storage) ->
      Legacy_support.add_set_delegate ~manager_pkh ~script_code ~script_storage >>=? fun (code, storage) ->
      Error_monad.return ((code, storage), ctxt)
    | Lift_tz_result tz_result ->
      Lwt.return tz_result >>=? fun result ->
      Error_monad.return (result, ctxt)
    | Log (location, gas, stack) ->
      begin
        match log with
        | None -> ()
        | Some log -> log := (location, gas, stack) :: !log
      end;
      Error_monad.return ((), ctxt)
    | Now ->
      let now = Script_timestamp.now ctxt in
      Error_monad.return (now, ctxt)
    | Parse_contract_for_script (legacy, location, typ, contract, entrypoint) ->
      Script_ir_translator.parse_contract_for_script
        ~legacy
        ctxt
        location
        typ
        contract
        ~entrypoint >>=? fun (ctxt, maybe_contract) ->
      Error_monad.return (maybe_contract, ctxt)
    | Parse_data (type_logger, legacy, typ, script_data) ->
      Script_ir_translator.parse_data ~type_logger ctxt ~legacy typ script_data
    | Serialize_pack_data (typ, value) ->
      Script_ir_translator.pack_data ctxt typ value
    | Serialize_unpack_data (typ, script_data) ->
      Script_ir_translator.parse_data ctxt ~legacy:false typ script_data >>= fun result ->
      begin
        match result with
        | Ok (value, ctxt) -> Error_monad.return (Some value, ctxt)
        | Error _ -> Error_monad.return (None, ctxt)
      end
    | Unparse_data (mode, storage_typ, storage) ->
      Script_ir_translator.unparse_data ctxt mode storage_typ storage >>=? fun (storage, ctxt) ->
      Error_monad.return (storage, ctxt)
    | Unparse_ty typ ->
      Script_ir_translator.unparse_ty ctxt typ >>=? fun (typ_expr, ctxt) ->
      Error_monad.return (typ_expr, ctxt)
end

module Monad = struct
  type 'a t =
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Call : 'a Command.t -> 'a t
    | Gas_set_unlimited : 'a t -> 'a t
    | Return : 'a -> 'a t
    | Trace : Error.t * 'a t -> 'a t

  let rec eval
    : type a.
      a t ->
      context ->
      execution_trace ref option ->
      (a * context) tzresult Lwt.t
    = fun e ctxt log ->
    match e with
    | Bind (e', f) -> eval e' ctxt log >>=? fun (v, ctxt) -> eval (f v) ctxt log
    | Call command -> Command.eval command ctxt log
    | Gas_set_unlimited e' ->
      eval e' (Gas.set_unlimited ctxt) log >>=? fun (value, _) ->
      Error_monad.return (value, ctxt)
    | Return v -> Error_monad.return (v, ctxt)
    | Trace (error, e') -> trace (Script_interpreter_error error) (eval e' ctxt log)
end

let return_star (x : 'a) : 'a Monad.t =
  Monad.Return x

let (>>=*) (x : 'a Monad.t) (f : 'a -> 'b Monad.t) : 'b Monad.t =
  Monad.Bind (x, f)

let call (command : 'a Command.t) : 'a Monad.t =
  Monad.Call command

(* ---- interpreter ---------------------------------------------------------*)

type 'tys stack =
  | Item : 'ty * 'rest stack -> ('ty * 'rest) stack
  | Empty : end_of_stack stack

let unparse_stack (stack, stack_ty) : (Script.expr * string option) list Monad.t =
  let rec unparse_stack
    : type a. a stack * a stack_ty -> (Script.expr * string option) list Monad.t
    = function
      | Empty, Empty_t -> return_star []
      | Item (v, rest), Item_t (ty, rest_ty, annot) ->
        call (Unparse_data (Readable, ty, v)) >>=* fun data ->
        unparse_stack (rest, rest_ty) >>=* fun rest ->
        (match Script_ir_annot.unparse_var_annot annot with
        | [] -> return_star None
        | [ a ] -> return_star (Some a)
        | _ -> call Assert_false) >>=* fun annot ->
        let data = Micheline.strip_locations data in
        return_star ((data, annot) :: rest) in
  (* We drop the gas limit as this function is only used for debugging/errors. *)
  Gas_set_unlimited (unparse_stack (stack, stack_ty))

module Interp_costs = Michelson_v1_gas.Cost_of.Interpreter

let rec interp_stack_prefix_preserving_operation : type fbef bef faft aft result .
  (fbef stack -> (faft stack * result) Monad.t)
  -> (fbef, faft, bef, aft) stack_prefix_preservation_witness
  -> bef stack
  -> (aft stack * result) Monad.t =
  fun f n stk ->
    match n,stk with
    | Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix (Prefix n))))))))))))))),
      Item (v0, Item (v1, Item (v2, Item (v3, Item (v4, Item (v5, Item (v6, Item (v7, Item (v8, Item (v9, Item (va, Item (vb, Item (vc, Item (vd, Item (ve, Item (vf, rest)))))))))))))))) ->
        interp_stack_prefix_preserving_operation f n rest >>=* fun (rest', result) ->
        return_star (Item (v0, Item (v1, Item (v2, Item (v3, Item (v4, Item (v5, Item (v6, Item (v7, Item (v8, Item (v9, Item (va, Item (vb, Item (vc, Item (vd, Item (ve, Item (vf, rest')))))))))))))))), result)
    | Prefix (Prefix (Prefix (Prefix n))),
      Item (v0, Item (v1, Item (v2, Item (v3, rest)))) ->
        interp_stack_prefix_preserving_operation f n rest >>=* fun (rest', result) ->
        return_star (Item (v0, Item (v1, Item (v2, Item (v3, rest')))), result)
    | Prefix n, Item (v, rest) ->
        interp_stack_prefix_preserving_operation f n rest >>=* fun (rest', result) ->
        return_star (Item (v, rest'), result)
    | Rest, v -> f v

type step_constants =
  { source : Contract.t ;
    payer : Contract.t ;
    self : Contract.t ;
    amount : Tez.t ;
    chain_id : Chain_id.t }

let rec step_star
  : type b a. step_constants -> (b, a) descr -> b stack -> a stack Monad.t =
  fun step_constants ({ instr ; loc ; _ } as descr) stack ->
    call (Gas_consume Interp_costs.cycle) >>=* fun () ->
    let logged_return : type a b. (b, a) descr -> a stack -> a stack Monad.t =
      fun descr ret ->
        Trace (
          Cannot_serialize_log,
          (unparse_stack (ret, descr.aft))
         ) >>=* fun stack ->
        call Gas_level >>=* fun gas_level ->
        call (Log (descr.loc, gas_level, stack)) >>=* fun () ->
        return_star ret in
    let consume_gas_terop : type ret arg1 arg2 arg3 rest.
      (_ * (_ * (_ * rest)), ret * rest) descr ->
      ((arg1 -> arg2 -> arg3 -> ret) * arg1 * arg2 * arg3) ->
      (arg1 -> arg2 -> arg3 -> Gas.cost) ->
      rest stack ->
      (ret * rest) stack Monad.t =
      fun descr (op, x1, x2, x3) cost_func rest ->
        call (Gas_consume (cost_func x1 x2 x3)) >>=* fun () ->
        logged_return descr (Item (op x1 x2 x3, rest)) in
    let consume_gas_binop : type ret arg1 arg2 rest.
      (_ * (_ * rest), ret * rest) descr ->
      ((arg1 -> arg2 -> ret) * arg1 * arg2) ->
      (arg1 -> arg2 -> Gas.cost) ->
      rest stack ->
      (ret * rest) stack Monad.t =
      fun descr (op, x1, x2) cost_func rest ->
        call (Gas_consume (cost_func x1 x2)) >>=* fun () ->
        logged_return descr (Item (op x1 x2, rest)) in
    let consume_gas_unop : type ret arg rest.
      (_ * rest, ret * rest) descr ->
      ((arg -> ret) * arg) ->
      (arg -> Gas.cost) ->
      rest stack ->
      (ret * rest) stack Monad.t =
      fun descr (op, arg) cost_func rest ->
        call (Gas_consume (cost_func arg)) >>=* fun () ->
        logged_return descr (Item (op arg, rest)) in
    let logged_return : a stack -> a stack Monad.t =
      logged_return descr in
    match instr, stack with
    (* stack ops *)
    | Drop, Item (_, rest) ->
        call (Gas_consume Interp_costs.stack_op) >>=* fun () ->
        logged_return rest
    | Dup, Item (v, rest) ->
        call (Gas_consume Interp_costs.stack_op) >>=* fun () ->
        logged_return (Item (v, Item (v, rest)))
    | Swap, Item (vi, Item (vo, rest)) ->
        call (Gas_consume Interp_costs.stack_op) >>=* fun () ->
        logged_return (Item (vo, Item (vi, rest)))
    | Const v, rest ->
        call (Gas_consume Interp_costs.push) >>=* fun () ->
        logged_return (Item (v, rest))
    (* options *)
    | Cons_some, Item (v, rest) ->
        call (Gas_consume Interp_costs.wrap) >>=* fun () ->
        logged_return (Item (Some v, rest))
    | Cons_none _, rest ->
        call (Gas_consume Interp_costs.variant_no_data) >>=* fun () ->
        logged_return (Item (None, rest))
    | If_none (bt, _), Item (None, rest) ->
        call (Gas_consume Interp_costs.branch) >>=* fun () ->
        step_star step_constants bt rest
    | If_none (_, bf), Item (Some v, rest) ->
        call (Gas_consume Interp_costs.branch) >>=* fun () ->
        step_star step_constants bf (Item (v, rest))
    (* pairs *)
    | Cons_pair, Item (a, Item (b, rest)) ->
        call (Gas_consume Interp_costs.pair) >>=* fun () ->
        logged_return (Item ((a, b), rest))
    (* Peephole optimization for UNPAIR *)
    | Seq ({instr=Dup;_},
           {instr=Seq ({instr=Car;_},
                       {instr=Seq ({instr=Dip {instr=Cdr}},
                                   {instr=Nop;_});_});_}),
      Item ((a, b), rest) ->
        call (Gas_consume Interp_costs.pair_access) >>=* fun () ->
        logged_return (Item (a, Item (b, rest)))
    | Car, Item ((a, _), rest) ->
        call (Gas_consume Interp_costs.pair_access) >>=* fun () ->
        logged_return (Item (a, rest))
    | Cdr, Item ((_, b), rest) ->
        call (Gas_consume Interp_costs.pair_access) >>=* fun () ->
        logged_return (Item (b, rest))
    (* unions *)
    | Left, Item (v, rest) ->
        call (Gas_consume Interp_costs.wrap) >>=* fun () ->
        logged_return (Item (L v, rest))
    | Right, Item (v, rest) ->
        call (Gas_consume Interp_costs.wrap) >>=* fun () ->
        logged_return (Item (R v, rest))
    | If_left (bt, _), Item (L v, rest) ->
        call (Gas_consume Interp_costs.branch) >>=* fun () ->
        step_star step_constants bt (Item (v, rest))
    | If_left (_, bf), Item (R v, rest) ->
        call (Gas_consume Interp_costs.branch) >>=* fun () ->
        step_star step_constants bf (Item (v, rest))
    (* lists *)
    | Cons_list, Item (hd, Item (tl, rest)) ->
        call (Gas_consume Interp_costs.cons) >>=* fun () ->
        logged_return (Item (hd :: tl, rest))
    | Nil, rest ->
        call (Gas_consume Interp_costs.variant_no_data) >>=* fun () ->
        logged_return (Item ([], rest))
    | If_cons (_, bf), Item ([], rest) ->
        call (Gas_consume Interp_costs.branch) >>=* fun () ->
        step_star step_constants bf rest
    | If_cons (bt, _), Item (hd :: tl, rest) ->
        call (Gas_consume Interp_costs.branch) >>=* fun () ->
        step_star step_constants bt (Item (hd, Item (tl, rest)))
    | List_map body, Item (l, rest) ->
        let rec loop rest l acc =
          call (Gas_consume Interp_costs.loop_map) >>=* fun () ->
          match l with
          | [] -> return_star (Item (List.rev acc, rest))
          | hd :: tl ->
              step_star step_constants body (Item (hd, rest)) >>=* fun (Item (hd, rest)) ->
              loop rest tl (hd :: acc)
        in loop rest l [] >>=* fun res ->
        logged_return res
    | List_size, Item (l, rest) ->
        let rec loop l =
          match l with
          | [] -> return_star 0
          | _ :: l' ->
            call (Gas_consume Interp_costs.loop_size) >>=* fun () ->
            loop l' >>=* fun size ->
            return_star (size + 1) in
        loop l >>=* fun size ->
        logged_return (Item (Script_int.abs (Script_int.of_int size), rest))
    | List_iter body, Item (l, init) ->
        let rec loop l stack =
          call (Gas_consume Interp_costs.loop_iter) >>=* fun () ->
          match l with
          | [] -> return_star stack
          | hd :: tl ->
              step_star step_constants body (Item (hd, stack)) >>=* fun stack ->
              loop tl stack
        in loop l init >>=* fun res ->
        logged_return res
    (* sets *)
    | Empty_set t, rest ->
        call (Gas_consume Interp_costs.empty_set) >>=* fun () ->
        logged_return (Item (Script_ir_translator.empty_set t, rest))
    | Set_iter body, Item (set, init) ->
        call (Gas_consume (Interp_costs.set_to_list set)) >>=* fun () ->
        let l = List.rev (Script_ir_translator.set_fold (fun e acc -> e :: acc) set []) in
        let rec loop l stack =
          call (Gas_consume Interp_costs.loop_iter) >>=* fun () ->
          match l with
          | [] -> return_star stack
          | hd :: tl ->
              step_star step_constants body (Item (hd, stack)) >>=* fun stack ->
              loop tl stack
        in loop l init >>=* fun res ->
        logged_return res
    | Set_mem, Item (v, Item (set, rest)) ->
        consume_gas_binop descr (Script_ir_translator.set_mem, v, set) Interp_costs.set_mem rest
    | Set_update, Item (v, Item (presence, Item (set, rest))) ->
        consume_gas_terop descr (Script_ir_translator.set_update, v, presence, set) Interp_costs.set_update rest
    | Set_size, Item (set, rest) ->
        consume_gas_unop descr (Script_ir_translator.set_size, set) (fun _ -> Interp_costs.set_size) rest
    (* maps *)
    | Empty_map (t, _), rest ->
        call (Gas_consume Interp_costs.empty_map) >>=* fun () ->
        logged_return (Item (Script_ir_translator.empty_map t, rest))
    | Map_map body, Item (map, rest) ->
        call (Gas_consume (Interp_costs.map_to_list map)) >>=* fun () ->
        let l = List.rev (Script_ir_translator.map_fold (fun k v acc -> (k, v) :: acc) map []) in
        let rec loop rest l acc =
          call (Gas_consume Interp_costs.loop_map) >>=* fun () ->
          match l with
          | [] -> return_star acc
          | (k, _) as hd :: tl ->
              step_star step_constants body (Item (hd, rest)) >>=* fun (Item (hd, rest)) ->
              loop rest tl (Script_ir_translator.map_update k (Some hd) acc)
        in loop rest l (Script_ir_translator.empty_map (Script_ir_translator.map_key_ty map)) >>=* fun res ->
        logged_return (Item (res, rest))
    | Map_iter body, Item (map, init) ->
        call (Gas_consume (Interp_costs.map_to_list map)) >>=* fun () ->
        let l = List.rev (Script_ir_translator.map_fold (fun k v acc -> (k, v) :: acc) map []) in
        let rec loop l stack =
          call (Gas_consume Interp_costs.loop_iter) >>=* fun () ->
          match l with
          | [] -> return_star stack
          | hd :: tl ->
              step_star step_constants body (Item (hd, stack)) >>=* fun stack ->
              loop tl stack
        in loop l init >>=* fun res ->
        logged_return res
    | Map_mem, Item (v, Item (map, rest)) ->
        consume_gas_binop descr (Script_ir_translator.map_mem, v, map) Interp_costs.map_mem rest
    | Map_get, Item (v, Item (map, rest)) ->
        consume_gas_binop descr (Script_ir_translator.map_get, v, map) Interp_costs.map_get rest
    | Map_update, Item (k, Item (v, Item (map, rest))) ->
        consume_gas_terop descr (Script_ir_translator.map_update, k, v, map) Interp_costs.map_update rest
    | Map_size, Item (map, rest) ->
        consume_gas_unop descr (Script_ir_translator.map_size, map) (fun _ -> Interp_costs.map_size) rest
    (* Big map operations *)
    | Empty_big_map (tk, tv), rest ->
        call (Gas_consume Interp_costs.empty_map) >>=* fun () ->
        logged_return (Item (Script_ir_translator.empty_big_map tk tv, rest))
    | Big_map_mem, Item (key, Item (map, rest)) ->
        call (Gas_consume (Interp_costs.map_mem key map.diff)) >>=* fun () ->
        call (Big_map_mem (key, map)) >>=* fun res ->
        logged_return (Item (res, rest))
    | Big_map_get, Item (key, Item (map, rest)) ->
        call (Gas_consume (Interp_costs.map_get key map.diff)) >>=* fun () ->
        call (Big_map_get (key, map)) >>=* fun res ->
        logged_return (Item (res, rest))
    | Big_map_update, Item (key, Item (maybe_value, Item (map, rest))) ->
        consume_gas_terop descr
          (Script_ir_translator.big_map_update, key, maybe_value, map)
          (fun k v m -> Interp_costs.map_update k (Some v) m.diff) rest
    (* timestamp operations *)
    | Add_seconds_to_timestamp, Item (n, Item (t, rest)) ->
        consume_gas_binop descr
          (Script_timestamp.add_delta, t, n)
          Interp_costs.add_timestamp rest
    | Add_timestamp_to_seconds, Item (t, Item (n, rest)) ->
        consume_gas_binop descr (Script_timestamp.add_delta, t, n)
          Interp_costs.add_timestamp rest
    | Sub_timestamp_seconds, Item (t, Item (s, rest)) ->
        consume_gas_binop descr (Script_timestamp.sub_delta, t, s)
          Interp_costs.sub_timestamp rest
    | Diff_timestamps, Item (t1, Item (t2, rest)) ->
        consume_gas_binop descr (Script_timestamp.diff, t1, t2)
          Interp_costs.diff_timestamps rest
    (* string operations *)
    | Concat_string_pair, Item (x, Item (y, rest)) ->
        call (Gas_consume (Interp_costs.concat_string [x; y])) >>=* fun () ->
        let s = String.concat "" [x; y] in
        logged_return (Item (s, rest))
    | Concat_string, Item (ss, rest) ->
        call (Gas_consume (Interp_costs.concat_string ss)) >>=* fun () ->
        let s = String.concat "" ss in
        logged_return (Item (s, rest))
    | Slice_string, Item (offset, Item (length, Item (s, rest))) ->
        let s_length = Z.of_int (String.length s) in
        let offset = Script_int.to_zint offset in
        let length = Script_int.to_zint length in
        if Compare.Z.(offset < s_length && Z.add offset length <= s_length) then
          call (Gas_consume (Interp_costs.slice_string (Z.to_int length))) >>=* fun () ->
          logged_return (Item (Some (String.sub s (Z.to_int offset) (Z.to_int length)), rest))
        else
          call (Gas_consume (Interp_costs.slice_string 0)) >>=* fun () ->
          logged_return (Item (None, rest))
    | String_size, Item (s, rest) ->
        call (Gas_consume Interp_costs.push) >>=* fun () ->
        logged_return (Item (Script_int.(abs (of_int (String.length s))), rest))
    (* bytes operations *)
    | Concat_bytes_pair, Item (x, Item (y, rest)) ->
        call (Gas_consume (Interp_costs.concat_bytes [x; y])) >>=* fun () ->
        let s = MBytes.concat "" [x; y] in
        logged_return (Item (s, rest))
    | Concat_bytes, Item (ss, rest) ->
        call (Gas_consume (Interp_costs.concat_bytes ss)) >>=* fun () ->
        let s = MBytes.concat "" ss in
        logged_return (Item (s, rest))
    | Slice_bytes, Item (offset, Item (length, Item (s, rest))) ->
        let s_length = Z.of_int (MBytes.length s) in
        let offset = Script_int.to_zint offset in
        let length = Script_int.to_zint length in
        if Compare.Z.(offset < s_length && Z.add offset length <= s_length) then
          call (Gas_consume (Interp_costs.slice_string (Z.to_int length))) >>=* fun () ->
          logged_return (Item (Some (MBytes.sub s (Z.to_int offset) (Z.to_int length)), rest))
        else
          call (Gas_consume (Interp_costs.slice_string 0)) >>=* fun () ->
          logged_return (Item (None, rest))
    | Bytes_size, Item (s, rest) ->
        call (Gas_consume Interp_costs.push) >>=* fun () ->
        logged_return (Item (Script_int.(abs (of_int (MBytes.length s))), rest))
    (* currency operations *)
    | Add_tez, Item (x, Item (y, rest)) ->
        call (Gas_consume Interp_costs.int64_op) >>=* fun () ->
        call (Lift_tz_result Tez.(x +? y)) >>=* fun res ->
        logged_return (Item (res, rest))
    | Sub_tez, Item (x, Item (y, rest)) ->
        call (Gas_consume Interp_costs.int64_op) >>=* fun () ->
        call (Lift_tz_result Tez.(x -? y)) >>=* fun res ->
        logged_return (Item (res, rest))
    | Mul_teznat, Item (x, Item (y, rest)) ->
        call (Gas_consume Interp_costs.int64_op) >>=* fun () ->
        call (Gas_consume Interp_costs.z_to_int64) >>=* fun () ->
        begin
          match Script_int.to_int64 y with
          | None -> call (Fail_overflow loc)
          | Some y ->
              call (Lift_tz_result Tez.(x *? y)) >>=* fun res ->
              logged_return (Item (res, rest))
        end
    | Mul_nattez, Item (y, Item (x, rest)) ->
        call (Gas_consume Interp_costs.int64_op) >>=* fun () ->
        call (Gas_consume Interp_costs.z_to_int64) >>=* fun () ->
        begin
          match Script_int.to_int64 y with
          | None -> call (Fail_overflow loc)
          | Some y ->
              call (Lift_tz_result Tez.(x *? y)) >>=* fun res ->
              logged_return (Item (res, rest))
        end
    (* boolean operations *)
    | Or, Item (x, Item (y, rest)) ->
        consume_gas_binop descr ((||), x, y) Interp_costs.bool_binop rest
    | And, Item (x, Item (y, rest)) ->
        consume_gas_binop descr ((&&), x, y) Interp_costs.bool_binop rest
    | Xor, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Compare.Bool.(<>), x, y) Interp_costs.bool_binop rest
    | Not, Item (x, rest) ->
        consume_gas_unop descr (not, x) Interp_costs.bool_unop rest
    (* integer operations *)
    | Is_nat, Item (x, rest) ->
        consume_gas_unop descr (Script_int.is_nat, x) Interp_costs.abs rest
    | Abs_int, Item (x, rest) ->
        consume_gas_unop descr (Script_int.abs, x) Interp_costs.abs rest
    | Int_nat, Item (x, rest) ->
        consume_gas_unop descr (Script_int.int, x) Interp_costs.int rest
    | Neg_int, Item (x, rest) ->
        consume_gas_unop descr (Script_int.neg, x) Interp_costs.neg rest
    | Neg_nat, Item (x, rest) ->
        consume_gas_unop descr (Script_int.neg, x) Interp_costs.neg rest
    | Add_intint, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest
    | Add_intnat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest
    | Add_natint, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest
    | Add_natnat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.add_n, x, y) Interp_costs.add rest
    | Sub_int, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.sub, x, y) Interp_costs.sub rest
    | Mul_intint, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest
    | Mul_intnat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest
    | Mul_natint, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest
    | Mul_natnat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.mul_n, x, y) Interp_costs.mul rest
    | Ediv_teznat, Item (x, Item (y, rest)) ->
        call (Gas_consume Interp_costs.int64_to_z) >>=* fun () ->
        let x = Script_int.of_int64 (Tez.to_mutez x) in
        consume_gas_binop descr
          ((fun x y ->
              match Script_int.ediv x y with
              | None -> None
              | Some (q, r) ->
                  match Script_int.to_int64 q,
                        Script_int.to_int64 r with
                  | Some q, Some r ->
                      begin
                        match Tez.of_mutez q, Tez.of_mutez r with
                        | Some q, Some r -> Some (q,r)
                        (* Cannot overflow *)
                        | _ -> None
                      end
                  (* Cannot overflow *)
                  | _ -> None),
           x, y)
          Interp_costs.div
          rest
    | Ediv_tez, Item (x, Item (y, rest)) ->
        call (Gas_consume Interp_costs.int64_to_z) >>=* fun () ->
        call (Gas_consume Interp_costs.int64_to_z) >>=* fun () ->
        let x = Script_int.abs (Script_int.of_int64 (Tez.to_mutez x)) in
        let y = Script_int.abs (Script_int.of_int64 (Tez.to_mutez y)) in
        consume_gas_binop descr
          ((fun x y -> match Script_int.ediv_n x y with
              | None -> None
              | Some (q, r) ->
                  match Script_int.to_int64 r with
                  | None -> None (* Cannot overflow *)
                  | Some r ->
                      match Tez.of_mutez r with
                      | None -> None (* Cannot overflow *)
                      | Some r -> Some (q, r)),
           x, y)
          Interp_costs.div
          rest
    | Ediv_intint, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.ediv, x, y) Interp_costs.div rest
    | Ediv_intnat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.ediv, x, y) Interp_costs.div rest
    | Ediv_natint, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.ediv, x, y) Interp_costs.div rest
    | Ediv_natnat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.ediv_n, x, y) Interp_costs.div rest
    | Lsl_nat, Item (x, Item (y, rest)) ->
        call (Gas_consume (Interp_costs.shift_left x y)) >>=* fun () ->
        begin
          match Script_int.shift_left_n x y with
          | None -> call (Fail_overflow loc)
          | Some x -> logged_return (Item (x, rest))
        end
    | Lsr_nat, Item (x, Item (y, rest)) ->
        call (Gas_consume (Interp_costs.shift_right x y)) >>=* fun () ->
        begin
          match Script_int.shift_right_n x y with
          | None -> call (Fail_overflow loc)
          | Some r -> logged_return (Item (r, rest))
        end
    | Or_nat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.logor, x, y) Interp_costs.logor rest
    | And_nat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.logand, x, y) Interp_costs.logand rest
    | And_int_nat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.logand, x, y) Interp_costs.logand rest
    | Xor_nat, Item (x, Item (y, rest)) ->
        consume_gas_binop descr (Script_int.logxor, x, y) Interp_costs.logxor rest
    | Not_int, Item (x, rest) ->
        consume_gas_unop descr (Script_int.lognot, x) Interp_costs.lognot rest
    | Not_nat, Item (x, rest) ->
        consume_gas_unop descr (Script_int.lognot, x) Interp_costs.lognot rest
    (* control *)
    | Seq (hd, tl), stack ->
        step_star step_constants hd stack >>=* fun trans ->
        step_star step_constants tl trans
    | If (bt, _), Item (true, rest) ->
        call (Gas_consume Interp_costs.branch) >>=* fun () ->
        step_star step_constants bt rest
    | If (_, bf), Item (false, rest) ->
        call (Gas_consume Interp_costs.branch) >>=* fun () ->
        step_star step_constants bf rest
    | Loop body, Item (true, rest) ->
        call (Gas_consume Interp_costs.loop_cycle) >>=* fun () ->
        step_star step_constants body rest >>=* fun trans ->
        step_star step_constants descr trans
    | Loop _, Item (false, rest) ->
        logged_return rest
    | Loop_left body, Item (L v, rest) ->
        call (Gas_consume Interp_costs.loop_cycle) >>=* fun () ->
        step_star step_constants body (Item (v, rest)) >>=* fun trans ->
        step_star step_constants descr trans
    | Loop_left _, Item (R v, rest) ->
        call (Gas_consume Interp_costs.loop_cycle) >>=* fun () ->
        logged_return (Item (v, rest))
    | Dip b, Item (ign, rest) ->
        call (Gas_consume Interp_costs.stack_op) >>=* fun () ->
        step_star step_constants b rest >>=* fun res ->
        logged_return (Item (ign, res))
    | Exec, Item (arg, Item (lam, rest)) ->
        call (Gas_consume Interp_costs.exec) >>=* fun () ->
        interp_star step_constants lam arg >>=* fun res ->
        logged_return (Item (res, rest))
    | Apply capture_ty, Item (capture, Item (lam, rest)) -> (
        call (Gas_consume Interp_costs.apply) >>=* fun () ->
        let (Lam (descr, expr)) = lam in
        let (Item_t (full_arg_ty , _ , _)) = descr.bef in
        call (Unparse_data (Optimized, capture_ty, capture)) >>=* fun const_expr ->
        call (Unparse_ty capture_ty) >>=* fun ty_expr ->
        match full_arg_ty with
        | Pair_t ((capture_ty, _, _), (arg_ty, _, _), _, _) -> (
            let arg_stack_ty = Item_t (arg_ty, Empty_t, None) in
            let const_descr = ({
                loc = descr.loc ;
                bef = arg_stack_ty ;
                aft = Item_t (capture_ty, arg_stack_ty, None) ;
                instr = Const capture ;
              } : (_, _) descr) in
            let pair_descr = ({
                loc = descr.loc ;
                bef = Item_t (capture_ty, arg_stack_ty, None) ;
                aft = Item_t (full_arg_ty, Empty_t, None) ;
                instr = Cons_pair ;
              } : (_, _) descr) in
            let seq_descr = ({
                loc = descr.loc ;
                bef = arg_stack_ty ;
                aft = Item_t (full_arg_ty, Empty_t, None) ;
                instr = Seq (const_descr, pair_descr) ;
              } : (_, _) descr) in
            let full_descr = ({
                loc = descr.loc ;
                bef = arg_stack_ty ;
                aft = descr.aft ;
                instr = Seq (seq_descr, descr) ;
              } : (_, _) descr) in
            let full_expr = Micheline.Seq (0, [
                Prim (0, I_PUSH, [ ty_expr ; const_expr ], []) ;
                Prim (0, I_PAIR, [], []) ;
                expr ]) in
            let lam' = Lam (full_descr, full_expr) in
            logged_return (Item (lam', rest))
          )
        | _ -> call Assert_false
      )
    | Lambda lam, rest ->
        call (Gas_consume Interp_costs.push) >>=* fun () ->
        logged_return (Item (lam, rest))
    | Failwith tv, Item (v, _) ->
        Trace (Cannot_serialize_failure, call (Unparse_data (Optimized, tv, v))) >>=* fun v ->
        let v = Micheline.strip_locations v in
        call (Fail_reject (loc, v))
    | Nop, stack ->
        logged_return stack
    (* comparison *)
    | Compare ty, Item (a, Item (b, rest)) ->
        call (Gas_consume (Interp_costs.compare ty a b)) >>=* fun () ->
        logged_return (Item (Script_int.of_int @@ Script_ir_translator.compare_comparable ty a b, rest))
    (* comparators *)
    | Eq, Item (cmpres, rest) ->
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres = 0) in
        call (Gas_consume Interp_costs.compare_res) >>=* fun () ->
        logged_return (Item (cmpres, rest))
    | Neq, Item (cmpres, rest) ->
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres <> 0) in
        call (Gas_consume Interp_costs.compare_res) >>=* fun () ->
        logged_return (Item (cmpres, rest))
    | Lt, Item (cmpres, rest) ->
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres < 0) in
        call (Gas_consume Interp_costs.compare_res) >>=* fun () ->
        logged_return (Item (cmpres, rest))
    | Le, Item (cmpres, rest) ->
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres <= 0) in
        call (Gas_consume Interp_costs.compare_res) >>=* fun () ->
        logged_return (Item (cmpres, rest))
    | Gt, Item (cmpres, rest) ->
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres > 0) in
        call (Gas_consume Interp_costs.compare_res) >>=* fun () ->
        logged_return (Item (cmpres, rest))
    | Ge, Item (cmpres, rest) ->
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres >= 0) in
        call (Gas_consume Interp_costs.compare_res) >>=* fun () ->
        logged_return (Item (cmpres, rest))
    (* packing *)
    | Pack t, Item (value, rest) ->
        call (Serialize_pack_data (t, value)) >>=* fun bytes ->
        logged_return (Item (bytes, rest))
    | Unpack t, Item (bytes, rest) ->
        call (Gas_check_enough (Script.serialized_cost bytes)) >>=* fun () ->
        if Compare.Int.(MBytes.length bytes >= 1) &&
           Compare.Int.(MBytes.get_uint8 bytes 0 = 0x05) then
          let bytes = MBytes.sub bytes 1 (MBytes.length bytes - 1) in
          match Data_encoding.Binary.of_bytes Script.expr_encoding bytes with
          | None ->
              call (Gas_consume (Interp_costs.unpack_failed bytes)) >>=* fun () ->
              logged_return (Item (None, rest))
          | Some expr ->
              call (Gas_consume (Script.deserialized_cost expr)) >>=* fun () ->
              call (Serialize_unpack_data (t, (Micheline.root expr))) >>=* fun value ->
              match value with
              | Some value -> logged_return (Item (Some value, rest))
              | None ->
                call (Gas_consume (Interp_costs.unpack_failed bytes)) >>=* fun () ->
                logged_return (Item (None, rest))
        else
          logged_return (Item (None, rest))
    (* protocol *)
    | Address, Item ((_, address), rest) ->
        call (Gas_consume Interp_costs.address) >>=* fun () ->
        logged_return (Item (address, rest))
    | Contract (t, entrypoint), Item (contract, rest) ->
        call (Gas_consume Interp_costs.contract) >>=* fun () ->
        begin match contract, entrypoint with
          | (contract, "default"), entrypoint | (contract, entrypoint), "default" ->
              call (Parse_contract_for_script (false, loc, t, contract, entrypoint)) >>=* fun maybe_contract ->
              logged_return (Item (maybe_contract, rest))
          | _ -> logged_return (Item (None, rest))
        end
    | Transfer_tokens,
      Item (p, Item (amount, Item ((tp, (destination, entrypoint)), rest))) ->
        call (Gas_consume Interp_costs.transfer) >>=* fun () ->
        call (Big_map_collect (tp, p)) >>=* fun to_duplicate ->
        let to_update = Script_ir_translator.no_big_map_id in
        call (Big_map_extract_diff (Optimized, true, to_duplicate, to_update, tp, p)) >>=* fun (p, big_map_diff) ->
        call (Unparse_data (Optimized, tp, p)) >>=* fun p ->
        let operation =
          Transaction
            { amount ; destination ; entrypoint ;
              parameters = Script.lazy_expr (Micheline.strip_locations p) } in
        call Fresh_internal_nonce >>=* fun nonce ->
        logged_return (Item ((Internal_operation { source = step_constants.self ; operation ; nonce }, big_map_diff), rest))
    | Create_account,
      Item (manager, Item (delegate, Item (_delegatable, Item (credit, rest)))) ->
        call (Gas_consume Interp_costs.create_account) >>=* fun () ->
        call Fresh_contract_from_current_nonce >>=* fun contract ->
        (* store in optimized binary representation - as unparsed with [Optimized]. *)
        let manager_bytes =
          Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding manager in
        let storage =
          Script_repr.lazy_expr @@ Micheline.strip_locations @@
          Micheline.Bytes (0, manager_bytes) in
        let script =
          { code = Legacy_support.manager_script_code ;
            storage ;
          } in
        let operation =
          Origination
            { credit ; delegate ; preorigination = Some contract ; script } in
        call Fresh_internal_nonce >>=* fun nonce ->
        logged_return (Item ((Internal_operation { source = step_constants.self ; operation ; nonce }, None),
                             Item ((contract, "default"), rest)))
    | Implicit_account, Item (key, rest) ->
        call (Gas_consume Interp_costs.implicit_account) >>=* fun () ->
        let contract = Contract.implicit_contract key in
        logged_return (Item ((Unit_t None, (contract, "default")), rest))
    | Create_contract (storage_type, param_type, Lam (_, code), root_name),
      Item (manager, Item
              (delegate, Item
                 (spendable, Item
                    (delegatable, Item
                       (credit, Item
                          (init, rest)))))) ->
        call (Gas_consume Interp_costs.create_contract) >>=* fun () ->
        call (Unparse_ty param_type) >>=* fun unparsed_param_type ->
        let unparsed_param_type =
          Script_ir_translator.add_field_annot (Option.map ~f:(fun n -> `Field_annot n) root_name) None unparsed_param_type in
          call (Unparse_ty storage_type) >>=* fun unparsed_storage_type ->
        let code =
          Script.lazy_expr @@
          Micheline.strip_locations
            (Seq (0, [ Prim (0, K_parameter, [ unparsed_param_type ], []) ;
                       Prim (0, K_storage, [ unparsed_storage_type ], []) ;
                       Prim (0, K_code, [ code ], []) ])) in
        call (Big_map_collect (storage_type, init)) >>=* fun to_duplicate ->
        let to_update = Script_ir_translator.no_big_map_id in
        call (Big_map_extract_diff (
          Optimized, true, to_duplicate, to_update, storage_type, init
        )) >>=* fun (init, big_map_diff) ->
        call (Unparse_data (Optimized, storage_type, init)) >>=* fun storage ->
        let storage = Script.lazy_expr @@ Micheline.strip_locations storage in
        begin
          if spendable then
            call (Legacy_add_do (manager, code, storage))
          else if delegatable then
            call (Legacy_add_set_delegate (manager, code, storage))
          else if Legacy_support.has_default_entrypoint code then
            call (Legacy_add_root_entrypoint code) >>=* fun code ->
            return_star (code, storage)
          else return_star (code, storage)
        end >>=* fun (code, storage) ->
        call Fresh_contract_from_current_nonce >>=* fun contract ->
        let operation =
          Origination
            { credit ; delegate ; preorigination = Some contract ;
              script = { code ; storage } } in
        call Fresh_internal_nonce >>=* fun nonce ->
        logged_return
          (Item ((Internal_operation { source = step_constants.self ; operation ; nonce }, big_map_diff),
                 Item ((contract, "default"), rest)))
    | Create_contract_2 (storage_type, param_type, Lam (_, code), root_name),
      (* Removed the instruction's arguments manager, spendable and delegatable *)
      Item (delegate, Item
              (credit, Item
                 (init, rest))) ->
        call (Gas_consume Interp_costs.create_contract) >>=* fun () ->
        call (Unparse_ty param_type) >>=* fun unparsed_param_type->
        let unparsed_param_type =
          Script_ir_translator.add_field_annot (Option.map ~f:(fun n -> `Field_annot n) root_name) None unparsed_param_type in
        call (Unparse_ty storage_type) >>=* fun unparsed_storage_type ->
        let code =
          Micheline.strip_locations
            (Seq (0, [ Prim (0, K_parameter, [ unparsed_param_type ], []) ;
                       Prim (0, K_storage, [ unparsed_storage_type ], []) ;
                       Prim (0, K_code, [ code ], []) ])) in
        call (Big_map_collect (storage_type, init)) >>=* fun to_duplicate ->
        let to_update = Script_ir_translator.no_big_map_id in
        call (Big_map_extract_diff (
          Optimized, true, to_duplicate, to_update, storage_type, init
        )) >>=* fun (init, big_map_diff) ->
        call (Unparse_data (Optimized, storage_type, init)) >>=* fun storage ->
        let storage = Micheline.strip_locations storage in
        call Fresh_contract_from_current_nonce >>=* fun contract ->
        let operation =
          Origination
            { credit ; delegate ; preorigination = Some contract ;
              script = { code = Script.lazy_expr code ;
                         storage = Script.lazy_expr storage } } in
        call Fresh_internal_nonce >>=* fun nonce ->
        logged_return
          (Item ((Internal_operation { source = step_constants.self ; operation ; nonce }, big_map_diff),
                 Item ((contract, "default"), rest)))
    | Set_delegate,
      Item (delegate, rest) ->
        call (Gas_consume Interp_costs.create_account) >>=* fun () ->
        let operation = Delegation delegate in
        call Fresh_internal_nonce >>=* fun nonce ->
        logged_return (Item ((Internal_operation { source = step_constants.self ; operation ; nonce }, None), rest))
    | Balance, rest ->
        call (Gas_consume Interp_costs.balance) >>=* fun () ->
        call (Get_balance step_constants.self) >>=* fun balance ->
        logged_return (Item (balance, rest))
    | Now, rest ->
        call (Gas_consume Interp_costs.now) >>=* fun () ->
        call Now >>=* fun now ->
        logged_return (Item (now, rest))
    | Check_signature, Item (key, Item (signature, Item (message, rest))) ->
        call (Gas_consume (Interp_costs.check_signature key message)) >>=* fun () ->
        let res = Signature.check key signature message in
        logged_return (Item (res, rest))
    | Hash_key, Item (key, rest) ->
        call (Gas_consume Interp_costs.hash_key) >>=* fun () ->
        logged_return (Item (Signature.Public_key.hash key, rest))
    | Blake2b, Item (bytes, rest) ->
        call (Gas_consume (Interp_costs.hash_blake2b bytes)) >>=* fun () ->
        let hash = Raw_hashes.blake2b bytes in
        logged_return (Item (hash, rest))
    | Sha256, Item (bytes, rest) ->
        call (Gas_consume (Interp_costs.hash_sha256 bytes)) >>=* fun () ->
        let hash = Raw_hashes.sha256 bytes in
        logged_return (Item (hash, rest))
    | Sha512, Item (bytes, rest) ->
        call (Gas_consume (Interp_costs.hash_sha512 bytes)) >>=* fun () ->
        let hash = Raw_hashes.sha512 bytes in
        logged_return (Item (hash, rest))
    | Steps_to_quota, rest ->
        call (Gas_consume Interp_costs.steps_to_quota) >>=* fun () ->
        call Gas_level >>=* fun gas_level ->
        let steps = match gas_level with
          | Limited { remaining } -> remaining
          | Unaccounted -> Z.of_string "99999999" in
        logged_return (Item (Script_int.(abs (of_zint steps)), rest))
    | Source, rest ->
        call (Gas_consume Interp_costs.source) >>=* fun () ->
        logged_return (Item ((step_constants.payer, "default"), rest))
    | Sender, rest ->
        call (Gas_consume Interp_costs.source) >>=* fun () ->
        logged_return (Item ((step_constants.source, "default"), rest))
    | Self (t, entrypoint), rest ->
        call (Gas_consume Interp_costs.self) >>=* fun () ->
        logged_return (Item ((t, (step_constants.self, entrypoint)), rest))
    | Amount, rest ->
        call (Gas_consume Interp_costs.amount) >>=* fun () ->
        logged_return (Item (step_constants.amount, rest))
    | Dig (n, n'), stack ->
        call (Gas_consume (Interp_costs.stack_n_op n)) >>=* fun () ->
        interp_stack_prefix_preserving_operation (fun (Item (v, rest)) -> return_star (rest, v)) n' stack
        >>=* fun (aft, x) -> logged_return (Item (x, aft))
    | Dug (n, n'), Item (v, rest) ->
        call (Gas_consume (Interp_costs.stack_n_op n)) >>=* fun () ->
        interp_stack_prefix_preserving_operation (fun stk -> return_star (Item (v, stk), ())) n' rest
        >>=* fun (aft, ()) -> logged_return (aft)
    | Dipn (n, n', b), stack ->
        call (Gas_consume (Interp_costs.stack_n_op n)) >>=* fun () ->
        interp_stack_prefix_preserving_operation (fun stk ->
          step_star step_constants b stk >>=* fun res ->
          return_star (res, ())
        ) n' stack >>=* fun (aft, ()) ->
        logged_return aft
    | Dropn (n, n'), stack ->
        call (Gas_consume (Interp_costs.stack_n_op n)) >>=* fun () ->
        interp_stack_prefix_preserving_operation (fun stk -> return_star (stk, stk)) n' stack
        >>=* fun (_, rest) -> logged_return rest
    | ChainId, rest ->
        call (Gas_consume Interp_costs.chain_id) >>=* fun () ->
        logged_return (Item (step_constants.chain_id, rest))

and interp_star
  : type p r. step_constants -> (p, r) lambda -> p -> r Monad.t
  = fun step_constants (Lam (code, _)) arg ->
    let stack = (Item (arg, Empty)) in
    Trace (Cannot_serialize_log, unparse_stack (stack, code.bef)) >>=* fun serialized_stack ->
    call Gas_level >>=* fun gas_level ->
    call (Log (code.loc, gas_level, serialized_stack)) >>=* fun () ->
    step_star step_constants code stack >>=* fun (Item (ret, Empty)) ->
    return_star ret

let step ?log ctxt step_constants descr stack =
  Monad.eval (step_star step_constants descr stack) ctxt log

let interp ?log ctxt step_constants code (arg, storage) =
  Monad.eval (interp_star step_constants code (arg, storage)) ctxt log

let execute ?log ctxt mode step_constants ~entrypoint unparsed_script arg :
  (Script.expr * packed_internal_operation list * context * Contract.big_map_diff option) tzresult Lwt.t =
  Script_ir_translator.parse_script ctxt unparsed_script ~legacy:true
  >>=? fun (Ex_script { code ; arg_type ; storage ; storage_type ; root_name }, ctxt) ->
  trace
    (Script_interpreter_error (Bad_contract_parameter step_constants.self))
    (Lwt.return (Script_ir_translator.find_entrypoint arg_type ~root_name entrypoint)) >>=? fun (box, _) ->
  trace
    (Script_interpreter_error (Bad_contract_parameter step_constants.self))
    (Script_ir_translator.parse_data ctxt ~legacy:false arg_type (box arg))  >>=? fun (arg, ctxt) ->
  Script.force_decode ctxt unparsed_script.code >>=? fun (script_code, ctxt) ->
  Script_ir_translator.collect_big_maps ctxt arg_type arg >>=? fun (to_duplicate, ctxt) ->
  Script_ir_translator.collect_big_maps ctxt storage_type storage >>=? fun (to_update, ctxt) ->
  trace
    (Script_interpreter_error (Runtime_contract_error (step_constants.self, script_code)))
    (interp ?log ctxt step_constants code (arg, storage))
  >>=? fun ((ops, storage), ctxt) ->
  Script_ir_translator.extract_big_map_diff ctxt mode
    ~temporary:false ~to_duplicate ~to_update storage_type storage
  >>=? fun (storage, big_map_diff, ctxt) ->
  trace (Script_interpreter_error Cannot_serialize_storage)
    (Script_ir_translator.unparse_data ctxt mode storage_type storage) >>=? fun (storage, ctxt) ->
  let ops, op_diffs = List.split ops in
  let big_map_diff = match
      List.flatten (List.map (Option.unopt ~default:[]) (op_diffs @ [ big_map_diff ]))
    with
    | [] -> None
    | diff -> Some diff in
  return (Micheline.strip_locations storage, ops, ctxt, big_map_diff)

type execution_result =
  { ctxt : context ;
    storage : Script.expr ;
    big_map_diff : Contract.big_map_diff option ;
    operations : packed_internal_operation list }

let trace ctxt mode step_constants ~script ~entrypoint ~parameter =
  let log = ref [] in
  execute ~log ctxt mode step_constants ~entrypoint script (Micheline.root parameter)
  >>=? fun (storage, operations, ctxt, big_map_diff) ->
  let trace = List.rev !log in
  return ({ ctxt ; storage ; big_map_diff ; operations }, trace)

let execute ctxt mode step_constants ~script ~entrypoint ~parameter =
  execute ctxt mode step_constants ~entrypoint script (Micheline.root parameter)
  >>=? fun (storage, operations, ctxt, big_map_diff) ->
  return { ctxt ; storage ; big_map_diff ; operations }
