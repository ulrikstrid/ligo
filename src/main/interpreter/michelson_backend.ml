open Trace
open Ligo_interpreter_exc

(* returns the name of the prepended variable definition for $-substitutions in Test.compile_expression *)
let subst_vname s = "test_gen_"^s

let int_of_mutez t = Z.of_int64 @@ Memory_proto_alpha.Protocol.Alpha_context.Tez.to_mutez t
let compile_contract source_file entry_point =
  let open Ligo_compile in
  let syntax = "auto" in
  let options = Compiler_options.make () in
  let* michelson = Build.build_contract ~options syntax entry_point source_file in
  Of_michelson.build_contract ~disable_typecheck:false michelson

let simple_val_insertion ~loc michelson_ty michelson_value ligo_obj_ty : (Ast_typed.expression,_) result =
  let open Tezos_micheline in
  let open Ast_typed in
  let cano (x: unit Tezos_utils.Michelson.michelson) =
    let open Tezos_micheline.Micheline in
    let x = inject_locations (fun _ -> 0) (strip_locations x) in
    let x = strip_locations x in
    let* x = Proto_alpha_utils.Trace.trace_alpha_tzresult (throw_obj_exc loc) @@
      Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.prims_of_strings x (* feels wrong ... *)
    in
    ok x
  in
  let code = Tezos_utils.Michelson.(seq [i_drop ; (i_push michelson_ty michelson_value)]) in
  let* expr = cano code in
  let u = Format.asprintf "%a" Micheline_printer.print_expr
    (Micheline_printer.printable Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.string_of_prim expr)
  in
  let type_annotation = t_function (t_unit ()) ligo_obj_ty () in
  let code_block = make_e (e_string (Ligo_string.verbatim u)) type_annotation in
  let insertion = e_a_raw_code Stage_common.Backends.michelson code_block type_annotation in
  let applied = e_a_application insertion e_a_unit ligo_obj_ty in 
  ok applied

let simple_val_insertion_core ~loc michelson_ty michelson_value ligo_obj_ty : (Ast_core.expression,_) result =
  let open Tezos_micheline in
  let open Ast_core in
  let* ligo_obj_ty = trace Main_errors.checking_tracer @@ Checking__.Untyper.untype_type_expression ligo_obj_ty in
  let cano (x: unit Tezos_utils.Michelson.michelson) =
    let open Tezos_micheline.Micheline in
    let x = inject_locations (fun _ -> 0) (strip_locations x) in
    let x = strip_locations x in
    let* x = Proto_alpha_utils.Trace.trace_alpha_tzresult (throw_obj_exc loc) @@
      Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.prims_of_strings x (* feels wrong ... *)
    in
    ok x
  in
  let code = Tezos_utils.Michelson.(seq [i_drop ; (i_push michelson_ty michelson_value)]) in
  let* expr = cano code in
  let u = Format.asprintf "%a" Micheline_printer.print_expr
    (Micheline_printer.printable Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.string_of_prim expr)
  in
  let code_block = (e_string (Ligo_string.verbatim u)) in
  let type_annotation = t_function (t_unit ()) ligo_obj_ty in
  let ascripted = e_ascription code_block type_annotation in
  let insertion = e_raw_code Stage_common.Backends.michelson ascripted in
  let applied = e_application insertion (e_unit ()) in
  let ascripted = e_ascription applied ligo_obj_ty in
  ok ascripted

let compile_function typed_exp =
  let open Ligo_compile in
  let options = Compiler_options.make () in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  let* compiled_exp   = Of_mini_c.aggregate_and_compile_expression ~options [] mini_c_exp in
  ok compiled_exp

let compile_value typed_exp =
  let open Ligo_compile in
  let options = Compiler_options.make () in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  let* compiled_exp   = Of_mini_c.aggregate_and_compile_expression ~options [] mini_c_exp in
  ok compiled_exp  

let compile_function' ~loc in_ty out_ty arg_binder body subst_lst =
  let open Ligo_compile in
  let options = Compiler_options.make () in
  let* typed_exp =
    let open Ast_typed in
    let aux exp (s,(mv,mt,t)) : (expression,_) result =
      let let_binder = Location.wrap @@ Var.of_name s in
      if Var.compare let_binder.wrap_content arg_binder.Location.wrap_content <> 0 then
        let* applied = simple_val_insertion ~loc mt mv t in
        ok @@ e_a_let_in let_binder applied exp false
      else
        ok @@ exp
    in
    let* typed_exp' = bind_fold_right_list aux body subst_lst in
    let typed_exp' = e_a_lambda {result=typed_exp'; binder=arg_binder} in_ty out_ty in
    ok typed_exp'
  in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  let* compiled_exp   = Of_mini_c.aggregate_and_compile_expression ~options [] mini_c_exp in
  ok compiled_exp

let compile_expression' ~loc _typed_env typed_exp subst_lst =
  let open Ligo_compile in
  let type_expr = typed_exp.Ast_typed.type_expression in
  let options = Compiler_options.make () in
  let* ligo_obj_ty = trace Main_errors.checking_tracer @@ Checking__.Untyper.untype_type_expression type_expr in
  let* core_expr = trace Main_errors.checking_tracer @@ Checking.untype_expression typed_exp in
  let core_expr = Ast_core.e_ascription core_expr ligo_obj_ty in
  let* core_expr =
    let open Ast_core in
    let aux exp (s,(mv,mt,t)) : (expression,_) result =
      let let_binder = Location.wrap @@ Var.of_name s in
      let* applied = simple_val_insertion_core ~loc mt mv t in
      ok @@ e_let_in {var=let_binder;ascr=None} applied exp false
    in
    bind_fold_right_list aux core_expr subst_lst
  in
  let core_expr = Ast_core.e_ascription core_expr ligo_obj_ty in
  let* typed_exp,_ = Of_core.compile_expression ~infer:false ~env:_typed_env core_expr in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  let* compiled_exp   = Of_mini_c.aggregate_and_compile_expression ~options [] mini_c_exp in
  ok compiled_exp

let compile_expression ~loc syntax exp_as_string source_file subst_lst =
  let open Ligo_compile in
  let options = Compiler_options.make () in
  let* (decl_list,env) = match source_file with
    | Some init_file -> Build.build_mini_c ~options syntax Env init_file
    | None -> ok ([],options.init_env)
  in
  let* typed_exp =
    match subst_lst with
    | [] ->
      let* typed_exp,_  = Utils.type_expression ~options source_file syntax exp_as_string env in
      ok typed_exp
    (* this handle $-substitution by prepeding `let test_gen_x = [%Micheson {| some_code |}] () in ..` to the compiled expression *) 
    | lst ->
      let open Ast_typed in
      let aux env (s,(_,_,t)) : environment = (* adds substituted value types to the env, feels wrong ... *)
        let s = subst_vname s in
        let v = Location.wrap @@ Var.of_name s in
        (* let (_,t) = get_t_function_exn t in *)
        Ast_typed.Environment.add_ez_binder v t env
      in
      let env' = List.fold_left aux env lst in
      let* (typed_exp,_) = Utils.type_expression ~options source_file syntax exp_as_string env' in
      let aux exp (s,(mv,mt,t)) : (expression,_) result =
        let s = subst_vname s in
        let let_binder = Location.wrap @@ Var.of_name s in
        let* applied = simple_val_insertion ~loc mt mv t in
        ok @@ e_a_let_in let_binder applied exp false
      in
      let* typed_exp' = bind_fold_right_list aux typed_exp lst in
      ok typed_exp'
  in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  let* compiled_exp   = Of_mini_c.aggregate_and_compile_expression ~options decl_list mini_c_exp in
  let* options        = Run.Of_michelson.make_dry_run_options {now=None ; amount="" ; balance="" ; sender=None ; source=None ; parameter_ty = None } in
  let* runres         = Run.Of_michelson.run_expression ~options compiled_exp.expr compiled_exp.expr_ty in
  let* (expr_ty,expr) = match runres with | Success x -> ok x | Fail _ -> fail @@ Errors.generic_error loc "evaluation of storage failed" in
  let expr = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr) in
  let expr_ty = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr_ty) in
  ok (expr, expr_ty, typed_exp.type_expression)

let rec val_to_ast ~loc : Ligo_interpreter.Types.value ->
                          Ast_typed.type_expression ->
                          (_, _ ) result =
  fun v ty ->
    let open Ligo_interpreter.Types in
    match v with
    | V_Ct C_unit -> ok Ast_typed.e_a_unit
    | V_Ct (C_bool b) -> ok @@ Ast_typed.e_a_bool b
    | V_Ct (C_int x) -> ok @@ Ast_typed.e_a_int x
    | V_Ct (C_nat x) -> ok @@ Ast_typed.e_a_nat x
    | V_Ct (C_timestamp t) -> ok @@ Ast_typed.e_a_timestamp t
    | V_Ct (C_string s) -> ok @@ Ast_typed.e_a_string (Simple_utils.Ligo_string.standard s)
    | V_Ct (C_bytes b) -> ok @@ Ast_typed.e_a_bytes b
    | V_Ct (C_address a) -> let x = Format.asprintf "%a" Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.pp a in ok @@ Ast_typed.e_a_address x
    | V_Construct (ctor, arg) ->
       let map_ty = Ast_typed.get_t_sum_exn ty in
       let {associated_type=ty'} = LMap.find (Label ctor) map_ty.content in
       let* arg = val_to_ast ~loc arg ty' in
       ok @@ Ast_typed.e_a_constructor ctor arg ty
    | V_Func_val v -> ok v.orig_lambda
    | V_Michelson (Ty_code (expr, expr_ty, ty_exp)) ->
       let* mini_c = trace Main_errors.decompile_michelson @@ Stacking.Decompiler.decompile_value expr_ty expr in
       let* typed = trace Main_errors.decompile_mini_c @@ Spilling.decompile mini_c ty_exp in
       ok @@ typed
    | V_Record map ->
       begin
         match Ast_typed.get_t_record ty with
         | Some map_ty -> 
            let* inner_vals = Ast_typed.Helpers.bind_map_lmapi (fun l v ->
                                  let ty = LMap.find l map_ty.content in
                                  val_to_ast ~loc v ty.associated_type) map in
            ok @@ Ast_typed.e_a_record ~layout:map_ty.layout inner_vals
         | None ->
         match Ast_typed.get_t_tuple ty with
         | Some _list_ty -> failwith "es tupla"
         | None ->
         match Ast_typed.get_t_pair ty with
         | Some (_left_ty, _right_ty) -> failwith "es par"
         | None -> fail @@ Errors.generic_error loc
                             (Format.asprintf "%a" Ast_typed.PP.type_expression ty)
       end
    | V_List l ->
       begin
         match Ast_typed.get_t_list ty with
         | Some ty -> let* l = bind_map_list (fun v -> val_to_ast ~loc v ty) l in
                      ok @@ List.fold_right Ast_typed.e_a_cons l (Ast_typed.e_a_nil ty)
         | None -> fail @@ Errors.generic_error loc "Expected list"
       end
    | V_Set l ->
       begin
         match Ast_typed.get_t_set ty with
         | Some ty -> let* l = bind_map_list (fun v -> val_to_ast ~loc v ty) l in
                      let l = List.sort_uniq compare l in
                      ok @@ List.fold_right Ast_typed.e_a_set_add l (Ast_typed.e_a_set_empty ty)
         | None -> fail @@ Errors.generic_error loc "Expected set"
       end
    | V_Map kv ->
       begin
         match Ast_typed.get_t_map ty with
         | Some (key_ty, value_ty) ->
            let* kv = bind_map_list (fun (k, v) ->
                         let* k = val_to_ast ~loc k key_ty in
                         let* v = val_to_ast ~loc v value_ty in
                         ok (k, v)) kv in
            let kv = List.sort_uniq compare kv in
            ok @@ List.fold_right (fun (k, v) r -> Ast_typed.e_a_map_add k v r) kv
                    (Ast_typed.e_a_map_empty key_ty value_ty)
         | None -> fail @@ Errors.generic_error loc "Expected map"
       end
    | V_Ligo _ -> fail @@ Errors.generic_error loc "Cannot be abstracted: ligo"
    | V_Michelson (Contract _) -> fail @@ Errors.generic_error loc "Cannot be abstracted: michelson-contract"
    | V_Func_rec _ -> fail @@ Errors.generic_error loc "Cannot be abstracted: func rec"

let compile_simple_val ~loc : Ligo_interpreter.Types.value ->
                              Ast_typed.type_expression ->
                              (_, _ ) result =
  fun v ty ->
    let open Ligo_interpreter.Types in
    let* typed_exp = val_to_ast ~loc v ty in
    let* compiled_exp = compile_value typed_exp in
    let* options        = Run.Of_michelson.make_dry_run_options {now=None ; amount="" ; balance="" ; sender=None ; source=None ; parameter_ty = None } in
    let* runres = Run.Of_michelson.run_expression ~options compiled_exp.expr compiled_exp.expr_ty in
    let* (expr_ty,expr) = match runres with | Success x -> ok x | Fail _ -> fail @@ Errors.generic_error loc "Running failed" in
    let expr = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr) in
    let expr_ty = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr_ty) in
    ok (expr, expr_ty, typed_exp.type_expression)

let storage_retreival_dummy_ty = Tezos_utils.Michelson.prim "unit"
