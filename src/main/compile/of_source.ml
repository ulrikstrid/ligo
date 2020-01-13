open Trace
open Helpers

let compile ~loc_form (source_filename:string) syntax : Ast_simplified.program result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify syntax ~loc_form source_filename in
  ok simplified

let compile_string ~loc_form (source:string) syntax : Ast_simplified.program result =
  let%bind simplified = parsify_string ~loc_form syntax source in
  ok simplified

let compile_expression ~loc_form : v_syntax -> string -> Ast_simplified.expression result =
    fun syntax exp ->
  parsify_expression ~loc_form syntax exp

let compile_contract_input ~loc_form_st ~loc_form_p : string -> string -> v_syntax -> Ast_simplified.expression result =
    fun storage parameter syntax ->
  let%bind storage'   = compile_expression ~loc_form:loc_form_st syntax storage in
  let%bind parameter' = compile_expression ~loc_form:loc_form_p syntax parameter in
  ok @@ Ast_simplified.e_pair storage' parameter'
