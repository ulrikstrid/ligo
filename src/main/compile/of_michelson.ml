open Main_errors
open Tezos_utils
open Proto_alpha_utils
open Trace

(* should preserve locations, currently wipes them *)
let build_contract : ?disable_typecheck:bool -> Stacking.compiled_expression -> (Location.t Michelson.michelson , _) result =
  fun ?(disable_typecheck= false) compiled ->
  let%bind (param_ty, storage_ty) = trace_option (entrypoint_not_a_function) @@
    Self_michelson.fetch_contract_inputs compiled.expr_ty in
  let expr = compiled.expr in
  let contract =
    Michelson.lcontract
      Location.dummy
      Location.dummy param_ty
      Location.dummy storage_ty
      Location.dummy expr in
  if disable_typecheck then
    ok contract
  else
    let%bind contract' =
      Trace.trace_tzresult_lwt (typecheck_contract_tracer contract)
        (Memory_proto_alpha.prims_of_strings contract) in
    let%bind _ = Trace.trace_tzresult_lwt (typecheck_contract_tracer contract) @@
      Proto_alpha_utils.Memory_proto_alpha.typecheck_contract contract' in
    ok contract

let measure = fun m ->
  Trace.trace_tzresult_lwt (could_not_serialize) @@
    Proto_alpha_utils.Measure.measure m

(* find pairs of canonical Michelson locations, and the original Ligo
   locations recorded there by the compiler *)
let source_map contract =
  let open Tezos_micheline in
  let (_, locs) = Micheline.extract_locations contract in
  let module LocSet = Set.Make(struct type t = Location.t ;; let compare = Location.compare end) in
  let ignored = LocSet.of_list [Location.dummy; Location.generated] in
  List.filter (fun (_, loc) -> not (LocSet.mem loc ignored)) locs

(* find pairs of "canonical" and concrete Michelson locations by
   printing and then parsing again *)
let michelson_location_map contract =
  let open Tezos_micheline in
  let contract = Tezos_micheline.Micheline_printer.printable (fun s -> s) (Tezos_micheline.Micheline.strip_locations contract) in
  let contract = Format.asprintf "%a" Micheline_printer.print_expr contract in
  match Micheline_parser.(no_parsing_error (tokenize contract)) with
  | Error _ -> Stdlib.failwith (Format.asprintf "TODO error tokenizing Michelson %s" __LOC__)
  | Ok contract ->
    match Micheline_parser.(no_parsing_error (parse_expression contract)) with
    | Error _ -> Stdlib.failwith (Format.asprintf "TODO error parsing Michelson %s" __LOC__)
    | Ok contract ->
      let (_, clocs) = Micheline.extract_locations contract in
      clocs
