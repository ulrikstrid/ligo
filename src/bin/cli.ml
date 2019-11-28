open Cmdliner
open Trace
open Cli_helpers

let main =
  let term = Term.(const print_endline $ const "Ligo needs a command. Do ligo --help") in
  (term , Term.info "ligo")

let source_file n =
  let open Arg in
  let info =
    let docv = "SOURCE_FILE" in
    let doc = "$(docv) is the path to the .ligo or .mligo file of the contract." in
    info ~docv ~doc [] in
  required @@ pos n (some string) None info

let entry_point n =
  let open Arg in
  let info =
    let docv = "ENTRY_POINT" in
    let doc = "$(docv) is entry-point that will be compiled." in
    info ~docv ~doc [] in
  required @@ pos n (some string) (Some "main") info

let expression purpose n =
  let open Arg in
  let docv = purpose ^ "_EXPRESSION" in
  let doc = "$(docv) is the expression that will be compiled." in
  let info = info ~docv ~doc [] in
  required @@ pos n (some string) None info

let syntax =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\" and \"cameligo\". By default, the syntax is guessed from the extension (.ligo and .mligo, respectively)." in
    info ~docv ~doc ["syntax" ; "s"] in
  value @@ opt string "auto" info

let req_syntax n =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\" and \"cameligo\". By default, the syntax is guessed from the extension (.ligo and .mligo, respectively)." in
    info ~docv ~doc [] in
  required @@ pos n (some string) None info

let amount =
  let open Arg in
  let info =
    let docv = "AMOUNT" in
    let doc = "$(docv) is the amount the dry-run transaction will use." in
    info ~docv ~doc ["amount"] in
  value @@ opt string "0" info

let sender =
  let open Arg in
  let info =
    let docv = "SENDER" in
    let doc = "$(docv) is the sender the dry-run transaction will use." in
    info ~docv ~doc ["sender"] in
  value @@ opt (some string) None info

let source =
  let open Arg in
  let info =
    let docv = "SOURCE" in
    let doc = "$(docv) is the source the dry-run transaction will use." in
    info ~docv ~doc ["source"] in
  value @@ opt (some string) None info

let display_format =
  let open Arg in
  let info  =
    let docv = "DISPLAY_FORMAT" in
    let doc = "$(docv) is the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile." in
    info ~docv ~doc ["format" ; "display-format"] in
  value @@
  opt
    (enum [("human-readable", `Human_readable); ("dev", `Dev); ("json", `Json)])
    `Human_readable
    info

let michelson_code_format =
  let open Arg in
  let info  =
    let docv = "MICHELSON_FORMAT" in
    let doc = "$(docv) is the format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'." in
    info ~docv ~doc ["michelson-format"] in
  value @@
  opt
    (enum [("text", `Text); ("json", `Json); ("hex", `Hex)])
    `Text info

let compile_file_contract_args_bis ~env ~state storage parameter syntax =
  let%bind simplified = Ligo.Compile.Of_source.just_compile_contract_input storage parameter syntax in
  let%bind typed,_    = Ligo.Compile.Of_simplified.just_compile_expression ~env ~state simplified in
  let%bind mini_c     = Ligo.Compile.Of_typed.just_compile_expression typed in
  let%bind michelson  = Ligo.Compile.Of_mini_c.compile_expression_as_function mini_c in

  let%bind michelson_val = Ligo.Run.Of_michelson.evaluate_michelson michelson in
  ok michelson_val

let compile_file_function_arg ~env ~state parameter syntax =
  let%bind simplified = Ligo.Compile.Of_source.just_compile_expression syntax parameter in
  let%bind typed,_    = Ligo.Compile.Of_simplified.just_compile_expression ~env ~state simplified in
  let%bind mini_c     = Ligo.Compile.Of_typed.just_compile_expression typed in
  let%bind michelson  = Ligo.Compile.Of_mini_c.compile_expression_as_function mini_c in

  let%bind michelson_val = Ligo.Run.Of_michelson.evaluate_michelson michelson in
  ok michelson_val

let source_to_michelson_program source_file entry_point syntax =
  let%bind simplified = Ligo.Compile.Of_source.just_compile           source_file syntax in
  let%bind typed,_    = Ligo.Compile.Of_simplified.just_compile       simplified         in
  let%bind mini_c     = Ligo.Compile.Of_typed.just_compile            typed              in
  let%bind michelson  = Ligo.Compile.Of_mini_c.compile_contract_entry mini_c entry_point in
  ok michelson

let typed_to_michelson_program typed entry_point =
  let%bind mini_c     = Ligo.Compile.Of_typed.just_compile            typed              in
  let%bind michelson_compiled_contract = Ligo.Compile.Of_mini_c.compile_function_entry mini_c entry_point in
  ok michelson_compiled_contract
(*
*)

let compile_file =
  let f source_file entry_point syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind contract = source_to_michelson_program source_file entry_point (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) contract
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ syntax $ display_format $ michelson_code_format) in
  let cmdname = "compile-contract" in
  let docs = "Subcommand: compile a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let dry_run =
  let f source_file entry_point storage input amount sender source syntax display_format =
    toplevel ~display_format @@
    let%bind (typed_program,state,env) = Ligo.Compile.Wrapper.source_to_typed (Syntax_name syntax) source_file in 
    let%bind v_syntax = Ligo.Compile.Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind args_michelson = compile_file_contract_args_bis ~env ~state  storage input v_syntax in
    let%bind michelson = typed_to_michelson_program typed_program entry_point in (*TODO: Use that in the run command instead of the compiled_program*)
    let%bind options = Ligo.Run.Of_source.make_dry_run_options {amount ; sender ; source } in
    let%bind michelson_output = Ligo.Run.Of_michelson.run ~options michelson args_michelson in
    let%bind simplified_output = Compile.Of_simplified.uncompile_typed_program_entry_function_result typed_program entry_point michelson_output in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression simplified_output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ expression "STORAGE" 3 $ amount $ sender $ source $ syntax $ display_format) in
  let cmdname = "dry-run" in
  let docs = "Subcommand: run a smart-contract with the given storage and input." in
  (term , Term.info ~docs cmdname)

let run_function =
  let f source_file entry_point parameter amount sender source syntax display_format =
    toplevel ~display_format @@
    let%bind (typed_program,state,env) = Ligo.Compile.Wrapper.source_to_typed (Syntax_name syntax) source_file in 
    let%bind v_syntax = Ligo.Compile.Helpers.syntax_to_variant (Syntax_name syntax) (Some source_file) in
    let%bind args_michelson = compile_file_function_arg ~env ~state parameter v_syntax in
    let%bind michelson = typed_to_michelson_program typed_program entry_point in (*TODO: Use that in the run command instead of the compiled_program*)
    let%bind options = Ligo.Run.Of_source.make_dry_run_options {amount ; sender ; source } in
    let%bind michelson_output = Ligo.Run.Of_michelson.run ~options michelson args_michelson in
    let%bind simplified_output = Compile.Of_simplified.uncompile_typed_program_entry_function_result typed_program entry_point michelson_output in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression simplified_output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ amount $ sender $ source $ syntax $ display_format) in
  let cmdname = "run-function" in
  let docs = "Subcommand: run a function with the given parameter." in
  (term , Term.info ~docs cmdname)

let compile_parameter =
  let f expression syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind v_syntax = Ligo.Compile.Helpers.syntax_to_variant (Syntax_name syntax) (None) in
    let%bind value = compile_file_function_arg ~env:(Ast_typed.Environment.full_empty) ~state:(Typer.Solver.initial_state)  expression v_syntax in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) value
  in
  let term =
    Term.(const f $ expression "PARAMETER" 0 $ req_syntax 1 $ display_format $ michelson_code_format) in
  let cmdname = "compile-parameter" in
  let docs = "Subcommand: compile parameters to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which calls a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

let compile_storage =
(*-------------------------------------------------------------------------------------------------------------------------------------
TODO:
This function does not typecheck anything, add the typecheck against the given entrypoint. For now: does the same as compile_parameter
--------------------------------------------------------------------------------------------------------------------------*----------- *)
  let f _source_file _entry_point expression syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind v_syntax = Ligo.Compile.Helpers.syntax_to_variant (Syntax_name syntax) (None) in
    let%bind value = compile_file_function_arg ~env:(Ast_typed.Environment.full_empty) ~state:(Typer.Solver.initial_state)  expression v_syntax in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) value
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "STORAGE" 2 $ syntax $ display_format $ michelson_code_format) in
  let cmdname = "compile-storage" in
  let docs = "Subcommand: compile an initial storage in ligo syntax to a michelson expression. The resulting michelson expression can be passed as an argument in a transaction which originates a contract. See `ligo " ^ cmdname ^ " --help' for a list of options specific to this subcommand." in
  (term , Term.info ~docs cmdname)

(* ??
let evaluate_value =
  let f source_file entry_point amount sender source syntax display_format =
    toplevel ~display_format @@
    let%bind output =
      Ligo.Run.Of_source.evaluate_entry
        ~options:{ amount ; sender ; source }
        source_file entry_point (Syntax_name syntax) in
    ok @@ Format.asprintf "%a\n" Ast_simplified.PP.expression output
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ amount $ sender $ source $ syntax $ display_format) in
  let cmdname = "evaluate-value" in
  let docs = "Subcommand: evaluate a given definition." in
  (term , Term.info ~docs cmdname)
  *)

let compile_expression =
  let f expression syntax display_format michelson_format =
    toplevel ~display_format @@
    let%bind v_syntax = Ligo.Compile.Helpers.syntax_to_variant (Syntax_name syntax) (None) in
    let%bind value = compile_file_function_arg ~env:(Ast_typed.Environment.full_empty) ~state:(Typer.Solver.initial_state)  expression v_syntax in
    ok @@ Format.asprintf "%a\n" (Main.Display.michelson_pp michelson_format) value
  in
  let term =
    Term.(const f $ expression "" 0 $ req_syntax 1 $ display_format $ michelson_code_format) in
  let cmdname = "compile-expression" in
  let docs = "Subcommand: compile to a michelson value." in
  (term , Term.info ~docs cmdname)

let run ?argv () =
  Term.eval_choice ?argv main [
    compile_file ;
    compile_parameter ;
    compile_storage ;
    compile_expression ;
    dry_run ;
    run_function ;
    (* evaluate_value ; *)
  ]
