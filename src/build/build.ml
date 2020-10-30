module Errors = Errors
module PP = PP
module To_yojson = To_yojson
module Formatter = Formatter

open Trace
open Errors
open Types

type file_name = string
type graph = G.t * (Compile.Helpers.meta * Compile.Of_core.form * Buffer.t * (string * string) list) SMap.t

(* Build system *)

let dependency_graph : options:Compiler_options.t -> string -> Compile.Of_core.form -> file_name -> (graph, _) result =
  fun ~options syntax form file_name ->
  let vertices = SMap.empty in
  let dep_g = G.empty in
  let rec dfs acc (dep_g,vertices) (file_name,form) =
    if not @@ SMap.mem file_name vertices then
      let%bind meta = trace compiler_error @@ Compile.Of_source.extract_meta syntax file_name in
      let%bind c_unit, deps = trace compiler_error @@ Compile.Of_source.compile ~options ~meta file_name in
      let vertices = SMap.add file_name (meta,form,c_unit,deps) vertices in
      let dep_g = G.add_vertex dep_g file_name in
      let dep_g =
        (* Don't add a loop on the first element *)
        if String.equal acc file_name then dep_g
        else G.add_edge dep_g acc file_name
      in
      let files = List.map (fun (a,_) -> (a,Compile.Of_core.Env)) deps in
      let%bind dep_g,vertices = bind_fold_list (dfs file_name) (dep_g,vertices) files in
      ok @@ (dep_g,vertices)
    else
      let dep_g = G.add_edge dep_g acc file_name in
      ok @@ (dep_g,vertices)
  in
  dfs file_name (dep_g,vertices) @@ (file_name,form)

let solve_graph : graph -> file_name -> (_ list,_) result =
  fun (dep_g,vertices) file_name ->
  if Dfs.has_cycle dep_g
  then (
    let graph = Format.asprintf "%a" PP.graph (dep_g,file_name) in
    fail @@ dependency_cycle @@ graph
  )
  else
    let aux v order =
      let elem = SMap.find v vertices in
      (v,elem)::order
    in
    let order = Dfs.fold_component aux [] dep_g file_name in
    ok @@ order

let add_modules_in_env env deps =
  let aux env (module_name, (_,ast_typed_env)) =
    Ast_typed.Environment.add_module module_name ast_typed_env env
  in
  List.fold_left aux env deps

let build_michelson order_deps asts_typed entry_point =
  let aggregate_deps ast_typed file_set deps_lst =
    let make_vars set (file_name,module_name) =
      let module_var = Location.wrap @@ Var.of_name module_name in
      let file_var,set = match SMap.find_opt file_name set with
      | Some var -> var,set
      | None ->
        let file_var = Location.wrap @@ Var.of_name file_name in
        let set = SMap.add file_name file_var set in
        file_var,set
      in
      (set),(file_var,module_var)
    in
    let file_set,deps_lst = List.fold_map_acc make_vars file_set deps_lst in
    let aux (file_name,module_name) ast_typed =
      let expr = Ast_typed.(make_e @@ E_variable file_name) @@ Ast_typed.t_unit () in
      (Location.wrap @@ Ast_typed.Declaration_constant {binder=module_name;expr;inline=true})
      :: ast_typed
    in
    let ast_typed = List.fold_right aux deps_lst ast_typed in
    ast_typed,file_set
  in
  let aux (_,(file_set,contracts)) (file_name, (_,_,_, deps_lst)) =
    let%bind (Ast_typed.Program_Fully_Typed ast_typed,_) =
      trace_option (corner_case ~loc:__LOC__ "Fail to find typed module") @@
      SMap.find_opt file_name asts_typed in
    let contract,file_vars = aggregate_deps ast_typed file_set deps_lst in
    let contracts = SMap.add file_name contract contracts in
    ok @@ (contract,(file_vars,contracts))
  in
  let%bind contract,(file_set,contracts)   = bind_fold_list aux ([],(SMap.empty,SMap.empty)) order_deps in
  let add_header contracts contract file_name file_var =
    let%bind ast_typed =
      trace_option (corner_case ~loc:__LOC__ "Fail to find aggregated module") @@
      SMap.find_opt file_name contracts in
    let aux decl decls = match Location.unwrap decl with
    | Ast_typed.Declaration_constant dc -> dc :: decls
    | Ast_typed.Declaration_type _ -> decls
    in
    let ast_typed = List.fold_right aux ast_typed [] in
    let record = List.map Ast_typed.(
      fun {binder;expr;inline=_} ->
      Label (Var.to_name binder.wrap_content),expr
    ) ast_typed in
    let record = Ast_typed.(E_record (LMap.of_list record)) in
    let record = Ast_typed.(make_e record @@ t_unit ()) in
    ok @@
    (Location.wrap @@ Ast_typed.Declaration_constant {binder=file_var;expr=record;inline=false})
    :: contract
  in
  let%bind contract   = bind_fold_smap (add_header contracts) (ok @@ contract) file_set in
  let%bind mini_c     = trace compiler_error @@ Compile.Of_typed.compile @@ Ast_typed.Program_Fully_Typed contract in
  let%bind michelson  = trace compiler_error @@ Compile.Of_mini_c.aggregate_and_compile_contract mini_c entry_point in
  ok michelson

let build_contract : options:Compiler_options.t -> string -> string -> _ -> file_name -> (_, _) result =
  fun ~options syntax entry_point protocol_version file_name ->
    let%bind deps = dependency_graph syntax ~options (Contract entry_point) file_name in
    let%bind order_deps = solve_graph deps file_name in
    let type_file_with_dep asts_typed (file_name, (meta,form,c_unit,deps)) =
      let%bind ast_core = trace compiler_error @@ Compile.Utils.to_core ~options ~meta c_unit file_name in
      let aux (file_name,module_name) =
        let%bind ast_typed =
          trace_option (corner_case ~loc:__LOC__
          "File compiled before dependency. The build system is broken, contact the devs")
          @@ SMap.find_opt file_name asts_typed
        in
        ok @@ (module_name, ast_typed)
      in
      let%bind deps = bind_map_list aux deps in
      let%bind init_env   = trace compiler_error @@ Compile.Helpers.get_initial_env protocol_version in
      let init_env = add_modules_in_env init_env deps in
      let%bind ast_typed,ast_typed_env,_ = trace compiler_error @@ Compile.Of_core.compile ~typer_switch:options.typer_switch ~init_env form ast_core in
      ok @@ SMap.add file_name (ast_typed,ast_typed_env) asts_typed
    in
    let%bind asts_typed = bind_fold_list type_file_with_dep (SMap.empty) order_deps in
    let%bind contract   = build_michelson order_deps asts_typed entry_point in
    ok contract
