(* The contents of this file should be auto-generated. *)

open Ast_typed.Types
module T = Ast_typed.Types

(* Using a pretty-printer from the PP.ml module creates a dependency
   loop, so the one that we need temporarily for debugging purposes
   has been copied here. *)
(* FIXME: Something is wrong with LIGO's copy of deriving that is
   preventing deriving.show from working. Once we figure that out,
   the rest of this file can be removed, along with a few others. *)
let debug_pp_constant : _ -> constant_tag -> unit = fun ppf c_tag ->
  let ct = match c_tag with
    | T.C_arrow     -> "arrow"
    | T.C_option    -> "option"
    | T.C_map       -> "map"
    | T.C_big_map   -> "big_map"
    | T.C_list      -> "list"
    | T.C_set       -> "set"
    | T.C_unit      -> "unit"
    | T.C_string    -> "string"
    | T.C_nat       -> "nat"
    | T.C_mutez     -> "mutez"
    | T.C_timestamp -> "timestamp"
    | T.C_int       -> "int"
    | T.C_address   -> "address"
    | T.C_bytes     -> "bytes"
    | T.C_key_hash  -> "key_hash"
    | T.C_key       -> "key"
    | T.C_signature -> "signature"
    | T.C_operation -> "operation"
    | T.C_contract  -> "contract"
    | T.C_chain_id  -> "chain_id"
  in
  Format.fprintf ppf "%s" ct

let debug_pp_c_constructor_simpl ppf { tv; c_tag; tv_list } =
  Format.fprintf ppf "CTOR %a %a(%a)" Var.pp tv debug_pp_constant c_tag PP_helpers.(list_sep Var.pp (const " , ")) tv_list
