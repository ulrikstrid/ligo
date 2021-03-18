open Trace

(* Helpers *)

let variant_to_syntax v =
  match v with
  | Compile.Helpers.PascaLIGO -> "pascaligo"
  | Compile.Helpers.CameLIGO -> "cameligo"
  | Compile.Helpers.ReasonLIGO -> "reasonligo"

let get_declarations_core core_prg =
     let func_declarations  = Compile.Of_core.list_declarations core_prg in
     let type_declarations  = Compile.Of_core.list_type_declarations core_prg in
     let mod_declarations  = Compile.Of_core.list_mod_declarations core_prg in
     func_declarations @ type_declarations @ mod_declarations

let get_declarations_typed typed_prg =
     let func_declarations  = Compile.Of_typed.list_declarations typed_prg in
     let type_declarations  = Compile.Of_typed.list_type_declarations typed_prg in
     let mod_declarations  = Compile.Of_typed.list_mod_declarations typed_prg in
     func_declarations @ type_declarations @ mod_declarations

(* REPL logic *)

type repl_result =
    Expression_value of Ast_core.expression
  | Defined_values_core of Ast_core.module_
  | Defined_values_typed of Ast_typed.module'
  | Just_ok

open Display

let repl_result_ppformat ~display_format f = function
    Expression_value expr ->
     (match display_format with
      | Human_readable | Dev -> Ast_core.PP.expression f expr)
  | Defined_values_core module_ ->
     (match display_format with
      | Human_readable | Dev -> Simple_utils.PP_helpers.list_sep_d
                                  Simple_utils.PP_helpers.string f
                                  (get_declarations_core module_))
  | Defined_values_typed module' ->
     (match display_format with
      | Human_readable | Dev -> Simple_utils.PP_helpers.list_sep_d
                                  Simple_utils.PP_helpers.string f
                                  (get_declarations_typed module'))
  | Just_ok -> Simple_utils.PP_helpers.string f "Done."

let repl_result_jsonformat = function
    Expression_value expr ->
     let value = Format.asprintf "%a" Ast_core.PP.expression expr in
     `Assoc [("value", `String value)]
  | Defined_values_core module_ ->
     let func_declarations  = Compile.Of_core.list_declarations module_ in
     let type_declarations  = Compile.Of_core.list_type_declarations module_ in
     let name n = `Assoc [("name", `String n)] in
     let defs = List.map name (func_declarations @ type_declarations) in
     `Assoc [("definitions", `List defs)]
  | Defined_values_typed module' ->
     let func_declarations  = Compile.Of_typed.list_declarations module' in
     let type_declarations  = Compile.Of_typed.list_type_declarations module' in
     let name n = `Assoc [("name", `String n)] in
     let defs = List.map name (func_declarations @ type_declarations) in
     `Assoc [("definitions", `List defs)]
  | Just_ok -> `Assoc []

let repl_result_format : 'a Display.format = {
    pp = repl_result_ppformat ;
    to_json = repl_result_jsonformat ;
}

module Run = Ligo.Run.Of_michelson

type state = { env : Ast_typed.environment;
               typer : Typer.Errors.typer_error Typer.Solver.typer_state;
               syntax : Compile.Helpers.v_syntax;
               typer_switch : Ast_typed.typer_switch;
               protocol : Environment.Protocols.t;
               decl_list : Mini_c.program;
               dry_run_opts : Run.options;
               mod_types : Ast_typed.type_expression Ast_core.SMap.t}

let try_eval state s =
  let options = Compiler_options.make ~init_env:state.env ~typer_switch:state.typer_switch ~protocol_version:state.protocol () in

  let%bind typed_exp,env,typer = Compile.Utils.type_expression_string ~options:options state.syntax s state.env state.typer in
  let%bind mini_c_exp = Compile.Of_typed.compile_expression ~module_env:state.mod_types typed_exp in
  let%bind compiled_exp = Compile.Of_mini_c.aggregate_and_compile_expression ~options:options state.decl_list mini_c_exp in
  let options = state.dry_run_opts in
  let%bind runres = Run.run_expression ~options:options compiled_exp.expr compiled_exp.expr_ty in
  match%bind (Decompile.Of_michelson.decompile_expression state.typer_switch typed_exp.type_expression runres) with
  | Success expr ->
     let state = { state with env = env; typer = typer; decl_list = state.decl_list } in
     ok (state, Expression_value expr)
  | Fail _ ->
     fail @@ `Repl_unexpected

let try_contract state s =
  let options = Compiler_options.make ~init_env:state.env ~typer_switch:state.typer_switch ~protocol_version:state.protocol () in
  let%bind c =
    generic_try (`Repl_unexpected : Main_errors.all) @@ fun _ ->
      let%bind typed_prg,core_prg,env,typer =
        Compile.Utils.type_contract_string ~options:options state.syntax s state.env in
      let%bind mini_c,mods =
        Compile.Of_typed.compile_with_modules ~module_env:state.mod_types typed_prg in
      let mod_types = Ast_core.SMap.union (fun _ _ a -> Some a) state.mod_types mods in
      let state = { state with env = env;
                               typer = typer;
                               decl_list = state.decl_list @ mini_c;
                               mod_types = mod_types; } in
      ok @@ (state, Defined_values_core core_prg) in
  try_catch (function
        (`Main_parser _ : Main_errors.all) -> try_eval state s
      | e -> fail e) c

let import_file state file_name module_name =
  let options = Compiler_options.make ~init_env:state.env ~typer_switch:state.typer_switch ~protocol_version:state.protocol () in
  let%bind mini_c,mod_types,_,env = Build.build_contract_module ~options (variant_to_syntax state.syntax) Compile.Of_core.Env file_name module_name in
  let env = Ast_typed.Environment.add_module module_name env state.env in
  let mod_env = Ast_core.SMap.find module_name mod_types in
  let mod_types = Ast_core.SMap.add module_name mod_env state.mod_types in
  let state = { state with env = env; decl_list = state.decl_list @ mini_c; mod_types = mod_types } in
  ok @@ (state, Just_ok)

let use_file state s =
  let options = Compiler_options.make ~init_env:state.env ~typer_switch:state.typer_switch ~protocol_version:state.protocol () in
  (* Missing typer environment? *)
  let%bind mini_c,mod_types,(Ast_typed.Module_Fully_Typed module'),env = Build.build_contract_use ~options (variant_to_syntax state.syntax) s in
  let mod_types = Ast_core.SMap.union (fun _ _ a -> Some a) state.mod_types mod_types in
  let state = { state with env = env;
                           decl_list = state.decl_list @ mini_c;
                           mod_types = mod_types } in
  ok @@ (state, Defined_values_typed module')

(* REPL "parsing" *)

type repl_directive = Use of string
                    | Import of string * string
                    | Expr of string

let parse s =
  let whitespace = "[ \n\r\x0c\t]" in
  let re_use = "^" ^ (whitespace ^ "*") ^ "#use" ^ (whitespace ^ "+") ^ "\"\\(.*\\)\"" ^ (whitespace ^ "*") ^ "$" in
  let re_import = "^" ^ (whitespace ^ "*") ^ "#import" ^ (whitespace ^ "+") ^ "\"\\(.*\\)\"" ^ (whitespace ^ "+") ^ "\"\\(.*\\)\"" ^ (whitespace ^ "*") ^ "$" in
  if Str.(string_match (regexp re_use) s 0) then
    Use (Str.matched_group 1 s)
  else if Str.(string_match (regexp re_import) s 0)  then
    Import (Str.matched_group 1 s, Str.matched_group 2 s)
  else
    Expr s

(* REPL main and loop *)

let eval display_format state c =
  let (Ex_display_format t) = display_format in
  match Trace.to_stdlib_result c with
    Ok ((state, out), _) ->
     let disp = (Displayable {value = out; format = repl_result_format }) in
     let out : string =
       match t with
       | Human_readable -> convert ~display_format:t disp ;
       | Dev -> convert ~display_format:t disp ;
       | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp in
     (1, state, out)
  | Error (e, _) ->
     let disp = (Displayable {value = e; format = Main_errors.Formatter.error_format }) in
     let out : string =
       match t with
       | Human_readable -> convert ~display_format:t disp ;
       | Dev -> convert ~display_format:t disp ;
       | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp in
     (0, state, out)

let parse_and_eval display_format state s =
  let c = match parse s with
    | Use s -> use_file state s
    | Import (fn, mn) -> import_file state fn mn
    | Expr s -> try_contract state s in
  eval display_format state c

let welcome_msg = "Welcome to LIGO's interpreter!
Included directives:
  #use \"file_path\";;
  #import \"file_path\" \"module_name\";;"

let make_initial_state syntax protocol typer_switch dry_run_opts =
  { env = Environment.default protocol;
    typer = Typer.Solver.initial_state;
    decl_list = [];
    syntax = syntax;
    typer_switch = typer_switch;
    protocol = protocol;
    dry_run_opts = dry_run_opts;
    mod_types = Ast_core.SMap.empty }

let rec read_input prompt delim =
  let module Let_syntax = struct
      let bind m ~f = Option.(>>=) m f
      module Open_on_rhs_bind = struct end
    end in
  let open Option in
  let s = LNoise.linenoise prompt in
  match s with
  | None -> none
  | Some s -> LNoise.history_add s |> ignore;
              let result = Str.split_delim (Str.regexp delim) s in
              match result with
              | [] | [_] ->
                 let%bind i = read_input "" delim in
                 some @@ s ^ "\n" ^ i
              | hd :: _ -> some @@ hd

let rec loop syntax display_format state n =
  let prompt = Format.sprintf "In  [%d]: " n in
  let s = read_input prompt ";;" in
  match s with
  | Some s ->
     let k, state, out = parse_and_eval display_format state s in
     let out = Format.sprintf "Out [%d]: %s" n out in
     print_endline out;
     loop syntax display_format state (n + k)
  | None -> ()

let main syntax display_format protocol typer_switch dry_run_opts init_file =
  print_endline welcome_msg;
  let state = make_initial_state syntax protocol typer_switch dry_run_opts in
  let state = match init_file with
    | None -> state
    | Some file_name -> let c = use_file state file_name in
                        let _, state, _ = eval (Ex_display_format Dev) state c in
                        state in
  LNoise.set_multiline true;
  loop syntax display_format state 1
