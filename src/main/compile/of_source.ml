open Trace
open Helpers

let just_compile (source_filename:string) syntax : Ast_simplified.program result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify syntax source_filename in
  ok simplified

let just_compile_expression : v_syntax -> string -> Ast_simplified.expression result =
    fun syntax exp ->
  parsify_expression syntax exp

let just_compile_contract_input : string -> string -> v_syntax -> Ast_simplified.expression result =
    fun storage parameter syntax ->
  let%bind contract_input = bind_map_pair (just_compile_expression syntax) (storage,parameter) in
  Of_simplified.just_pair_storage_parameter @@ contract_input 