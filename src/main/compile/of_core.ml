open Main_errors
open Trace

type form =
  | Contract of string
  | Env

let infer ~(options: Compiler_options.t) (m : Ast_core.module_) =
  let env_inf = Option.unopt_exn @@ Trace.to_option @@ Checking.decompile_env options.init_env in
  match options.infer with
    | true  -> let%bind (_,e,_,_) = trace inference_tracer @@ Inference.type_module ~init_env:env_inf m in ok @@ e
    | false -> ok @@ m

let typecheck ~(options: Compiler_options.t) (cform : form) (m : Ast_core.module_) : (Ast_typed.module_fully_typed * Ast_typed.environment , _) result =
  let%bind e,typed = trace checking_tracer @@ Checking.type_module ~init_env:options.init_env m in
  let%bind applied = trace self_ast_typed_tracer @@
    let%bind selfed = Self_ast_typed.all_module typed in
    match cform with
    | Contract entrypoint -> Self_ast_typed.all_contract entrypoint selfed
    | Env -> ok selfed in
  ok @@ (applied,e)

let compile_expression ?(infer = false) ~(env : Ast_typed.environment) (e : Ast_core.expression)
    : (Ast_typed.expression * Ast_typed.environment , _) result =
  let env_inf = Option.unopt_exn @@ Trace.to_option @@ Checking.decompile_env env in
  let%bind inferred = match infer with 
    | true  -> let%bind (_,e,_,_) =
      trace inference_tracer @@ Inference.type_expression_subst env_inf Inference.Solver.initial_state e in
      ok e
    | false -> ok e 
  in
  (* let%bind inferred = trace self_Ast_core_tracer @@ Self_Ast_core.all_expression inferred in *)
  let%bind e,typed = trace checking_tracer @@ Checking.type_expression env inferred in
  let%bind applied = trace self_ast_typed_tracer @@ Self_ast_typed.all_expression typed in
  ok @@ (applied, e)

let apply (entry_point : string) (param : Ast_core.expression) : (Ast_core.expression , _) result =
  let name = Location.wrap @@ Var.of_name entry_point in
  let entry_point_var : Ast_core.expression =
    { expression_content  = Ast_core.E_variable name ;
      sugar    = None ;
      location = Virtual "generated entry-point variable" } in
  let applied : Ast_core.expression =
    { expression_content  = Ast_core.E_application {lamb=entry_point_var; args=param} ;
      sugar    = None ;
      location = Virtual "generated application" } in
  ok applied

let list_declarations (m : Ast_core.module_) : string list =
  List.fold_left
    (fun prev el ->
      let open Location in
      let open Ast_core in
      match (el.wrap_content : Ast_core.declaration) with
      | Declaration_constant {binder;_} -> (Var.to_name binder.var.wrap_content)::prev
      | _ -> prev)
    [] m

let list_type_declarations (m : Ast_core.module_) : string list =
  List.fold_left
    (fun prev el ->
      let open Location in
      let open Ast_core in
      match (el.wrap_content : Ast_core.declaration) with
      | Declaration_type {type_binder;_} -> (Var.to_name type_binder)::prev
      | _ -> prev)
    [] m

let list_mod_declarations (m : Ast_core.module_) : string list =
  List.fold_left
    (fun prev el ->
      let open Location in
      let open Ast_core in
      match (el.wrap_content : Ast_core.declaration) with
      | Declaration_module {module_binder;_} -> (module_binder)::prev
      | Module_alias {alias;_} -> (alias)::prev
      | _ -> prev)
    [] m

let evaluate_type (env : Ast_typed.Environment.t) (t: Ast_core.type_expression) = trace checking_tracer @@ Checking.evaluate_type env t
