open Trace
open Helpers

(*here*)
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
(**)

let parse_file_program (source_filename:string) syntax : Ast_simplified.program result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simplified = parsify syntax source_filename in
  ok simplified

let compile_file_entry : string -> string -> s_syntax -> _ result =
  fun source_filename entry_point syntax ->
  let%bind simplified = parse_file_program source_filename syntax in
  Of_simplified.compile_function_entry simplified entry_point

let compile_file_contract_entry : string -> string -> s_syntax -> _ result =
  fun source_filename entry_point syntax ->
  let%bind simplified = parse_file_program source_filename syntax in
  let%bind compiled_contract = Of_simplified.compile_contract_entry simplified entry_point in
  ok compiled_contract

let compile_expression_as_function : string -> s_syntax -> _ result =
  fun expression syntax ->
  let%bind syntax = syntax_to_variant syntax None in
  let%bind simplified = parsify_expression syntax expression in
  Of_simplified.compile_expression_as_function ~state:Typer.Solver.initial_state (* TODO: thread state or start with initial? *) simplified

let type_file ?(debug_simplify = false) ?(debug_typed = false)
    syntax (source_filename:string) : (Ast_typed.program * Typer.Solver.state) result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind simpl = parsify syntax source_filename in
  (if debug_simplify then
     Format.(printf "Simplified : %a\n%!" Ast_simplified.PP.program simpl)
  ) ;
  let%bind (typed, state) =
    trace (simple_error "typing") @@
    Typer.type_program simpl in
  (if debug_typed then (
      Format.(printf "Typed : %a\n%!" Ast_typed.PP.program typed)
    )) ;
  ok (typed, state)
