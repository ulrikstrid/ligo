open Main_errors
open Tezos_utils
open Proto_alpha_utils
open Trace

(* should preserve locations, currently wipes them *)
let build_contract : ?disable_typecheck:bool -> Stacking.compiled_expression -> (_ Michelson.michelson , _) result =
  fun ?(disable_typecheck= false) compiled ->
  let* (param_ty, storage_ty) = trace_option (entrypoint_not_a_function) @@
    Self_michelson.fetch_contract_inputs compiled.expr_ty in
  let open Tezos_micheline.Micheline in
  let param_ty = inject_locations (fun _ -> ()) (strip_locations param_ty) in
  let storage_ty = inject_locations (fun _ -> ()) (strip_locations storage_ty) in
  let expr = inject_locations (fun _ -> ()) (strip_locations compiled.expr) in
  let contract = Michelson.contract param_ty storage_ty expr in
  if disable_typecheck then
    ok contract
  else
    let* contract' =
      Trace.trace_tzresult_lwt (typecheck_contract_tracer contract)
        (Memory_proto_alpha.prims_of_strings contract) in
    let* _ = Trace.trace_tzresult_lwt (typecheck_contract_tracer contract) @@
      Proto_alpha_utils.Memory_proto_alpha.typecheck_contract contract' in
    ok contract

let measure = fun m ->
  Trace.trace_tzresult_lwt (could_not_serialize) @@
    Proto_alpha_utils.Measure.measure m