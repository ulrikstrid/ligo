open Trace
open Test_helpers
open Main_errors

let () = Unix.putenv "LIGO_FORCE_NEW_TYPER" "false"
type syntax = string
type group_name = string
type lang = Meta | Object (* Object = normal LIGO code ; Meta = ligo test framework code *)
module SnippetsGroup = Map.Make(struct type t = (syntax * group_name) let compare a b = compare a b end)


type snippetsmap = (lang * string) SnippetsGroup.t

(**
  Binds the snippets by (syntax, group_name).
  Syntax and group_name being retrieved from the .md file header & arguments
  e.g. in the .md file:
    ```syntax group=group_name
      <some code>
    ```
**)
let get_groups md_file : snippetsmap =
  let channel = open_in md_file in
  let lexbuf = Lexing.from_channel channel in
  let code_blocks = Md.token lexbuf in
  let aux : snippetsmap -> Md.block -> snippetsmap =
    fun grp_map el ->
      match el.header  with
      | Some ("pascaligo" as s) | Some ("cameligo" as s) | Some ("reasonligo" as s) | Some ("jsligo" as s) -> (
        let () = (*sanity check*)
          List.iter
            (fun arg ->
              match arg with
              | Md.Field "" | Md.Field "skip" | Md.NameValue ("group",_) | Md.Field "test-ligo" -> ()
              | Md.Field _ | Md.NameValue _ -> failwith "unknown argument"
            )
            el.arguments
        in
        match el.arguments with
        | [Md.Field ""] -> (
          SnippetsGroup.update (s,"ungrouped")
            (fun arg_content ->
              match arg_content with
              | Some (lang,ct) -> Some (lang, String.concat "\n" (ct::el.contents))
              | None -> Some (Object, String.concat "\n" el.contents)
            )
            grp_map
        )
        | [Md.Field "skip"] -> grp_map
        | [Md.Field "test-ligo" ; Md.NameValue ("group", name)] -> (
          let lang = Meta in
          SnippetsGroup.update (s,name)
            (fun arg_content ->
              match arg_content with
              | Some (lang',ct) when lang = lang' -> Some (lang, String.concat "\n" (ct::el.contents))
              | _ -> Some (lang, String.concat "\n" el.contents)
            )
            grp_map
        )
        | [Md.NameValue ("group", name)] -> (
          let lang = Object in
          SnippetsGroup.update (s,name)
            (fun arg_content ->
              match arg_content with
              | Some (lang',ct) when lang = lang' -> Some (lang, String.concat "\n" (ct::el.contents))
              | _ -> Some (lang, String.concat "\n" el.contents)
            )
            grp_map
        )
        | args ->
          let () = List.iter (function Md.NameValue (x,y) -> Format.printf "NamedValue %s %s\n" x y | Md.Field x -> Format.printf "%s\n" x) args in
          failwith "Block arguments (above) not supported"
      )
      | None | Some _ -> grp_map
  in
  List.fold_left aux SnippetsGroup.empty code_blocks

(**
  if Meta : evaluate each expression in each programs from the snippets group map
  if Object : run the ligo test framework for entrypoint "test"
**)
let compile_groups filename grp_list =
  let aux : (syntax * group_name) * (lang * string) -> (unit, all) result =
    fun ((syntax , grp) , (lang , contents)) ->
      trace (test_md_file filename syntax grp contents) @@
      let options         = Compiler_options.make () in
      let* meta       = Ligo_compile.Of_source.make_meta syntax None in
      let* c_unit,_   = Ligo_compile.Of_source.compile_string ~options ~meta contents in
      let* imperative = Ligo_compile.Of_c_unit.compile ~meta c_unit filename in
      let* sugar      = Ligo_compile.Of_imperative.compile imperative in
      let* core       = Ligo_compile.Of_sugar.compile sugar in
      let* inferred   = Ligo_compile.Of_core.infer ~options core in
      match lang with
      | Meta ->
        let init_env = Environment.default_with_test options.protocol_version in
        let options = { options with init_env } in
        let* typed,_    = Ligo_compile.Of_core.typecheck ~options Env inferred in
        let* _ = Interpreter.eval_test typed "test" in
        ok ()
      | Object ->
        let* typed,_    = Ligo_compile.Of_core.typecheck ~options Env inferred in
        let* mini_c     = Ligo_compile.Of_typed.compile typed in
        let* (_michelsons : Stacking.compiled_expression list) =
          bind_map_list
            (fun ((_, _, exp),_) -> Ligo_compile.Of_mini_c.aggregate_and_compile_expression ~options mini_c exp)
            mini_c
        in
        ok ()
  in
  let* () = bind_iter_list aux grp_list in
  ok ()

let compile filename () =
  let groups = get_groups filename in
  let groups_map = SnippetsGroup.bindings groups in
  let* () = compile_groups filename groups_map in
  ok ()

let get_all_md_files () =
  let exclude_files = [
    "./gitlab-pages/docs/demo/ligo-snippet.md" ;
  ] in
  let ic = Unix.open_process_in "find ./gitlab-pages/docs -iname \"*.md\"" in
  let all_input = ref [] in
  let () =
    try
      while true do
        let md_file = input_line ic in
        if not (List.exists (String.equal md_file) exclude_files) then
          let grp = get_groups md_file in
          if not (SnippetsGroup.is_empty grp) then
            all_input := md_file :: !all_input
      done
    with
      End_of_file ->
      close_in ic
  in
  !all_input

let main =
  Sys.chdir "../.." ;
  test_suite "Markdown files" @@
    List.map
      (fun md_file ->
        let test_name = "File : "^md_file^"\"" in
        test test_name (compile md_file)
      )
      (get_all_md_files ())
