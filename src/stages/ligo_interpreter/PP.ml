open Types
open Simple_utils.PP_helpers

let pp_ct : Format.formatter -> constant_val -> unit = fun ppf c ->
  match c with
  | C_unit -> Format.fprintf ppf "()"
  | C_bool t -> Format.fprintf ppf "%b" t
  | C_int z -> Format.fprintf ppf "%s" (Int.to_string z)
  | C_nat n -> Format.fprintf ppf "%sn" (Int.to_string n)
  | C_timestamp t -> Format.fprintf ppf "timestamp(%a)" Z.pp_print t
  | C_string s -> Format.fprintf ppf "\"%s\"" s
  | C_bytes b -> Format.fprintf ppf "0x%a" Hex.pp (Hex.of_bytes b)
  | C_address c -> Format.fprintf ppf "%a" Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.pp c

let rec pp_value : Format.formatter -> value -> unit = fun ppf v ->
  match v with
  | V_Ct c -> Format.fprintf ppf "%a" pp_ct c
  | V_Func_val _ -> Format.fprintf ppf "<fun>"
  | V_Func_rec _ -> Format.fprintf ppf "<rec fun>"
  | V_Construct (name,v) -> Format.fprintf ppf "%s (%a)" name pp_value v
  | V_List vl -> Format.fprintf ppf "[%a]" (list_sep pp_value (tag " ; ")) vl
  | V_Set sl -> Format.fprintf ppf "{%a}" (list_sep pp_value (tag " ; ")) sl
  | V_Map vmap ->
    let aux : Format.formatter -> (value * value) -> unit = fun ppf (k, v) ->
      Format.fprintf ppf "%a -> %a" pp_value k pp_value v
    in
    Format.fprintf ppf "[%a]" (list_sep aux (tag " ; ")) vmap
  | V_Record recmap  ->
    if (Ast_typed.Helpers.is_tuple_lmap recmap) then
      let aux : Format.formatter -> value -> unit = fun ppf v ->
        Format.fprintf ppf "%a" pp_value v
      in
      Format.fprintf ppf "(%a)" (list_sep aux (tag " , ")) (LMap.to_list recmap)
    else
      let aux : Format.formatter -> (label * value) -> unit = fun ppf (Label l, v) ->
        Format.fprintf ppf "%s = %a" l pp_value v
      in
      Format.fprintf ppf "{%a}" (list_sep aux (tag " ; ")) (LMap.to_kv_list recmap)
  | V_Michelson (Ty_code (code,_,_) | Contract code (* | Subst_code (code,_) *) ) ->
    Format.fprintf ppf "%a" Tezos_utils.Michelson.pp code
  | V_Ligo (_syntax , code) ->
    Format.fprintf ppf "%s" code

let pp_env : Format.formatter -> env -> unit = fun ppf env ->
  let aux : Format.formatter -> expression_variable * value -> unit = fun ppf (var,v) ->
    Format.fprintf ppf "%a -> %a" Var.pp var.wrap_content pp_value v in
  Format.fprintf ppf "@[<v 2>%i bindings in environment:@ %a@]"
    (Env.cardinal env)
    (list_sep aux (tag "@ "))
    (Env.to_kv_list env)
