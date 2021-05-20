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
  (* let _ = print_endline (Format.asprintf "prog: %a" Ast_typed.PP.expression typed_exp) in *)
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  let* compiled_exp   = Of_mini_c.aggregate_and_compile_expression ~options [] mini_c_exp in
  ok compiled_exp
  (* let* options        = Run.Of_michelson.make_dry_run_options {now=None ; amount="" ; balance="" ; sender=None ; source=None ; parameter_ty = None } in
   * let* runres         = Run.Of_michelson.run_expression ~options compiled_exp.expr compiled_exp.expr_ty in
   * let* (expr_ty,expr) = match runres with | Success x -> ok x | Fail _ -> fail @@ Errors.generic_error loc "evaluation of storage failed" in
   * let expr = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr) in
   * let expr_ty = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr_ty) in
   * ok (expr, expr_ty, typed_exp.type_expression) *)

let compile_expression' ~loc _typed_env typed_exp subst_lst type_expr =
  let open Ligo_compile in
  let options = Compiler_options.make () in
  let* ligo_obj_ty = trace Main_errors.checking_tracer @@ Checking__.Untyper.untype_type_expression type_expr in
  let* core_expr = trace Main_errors.checking_tracer @@ Checking.untype_expression typed_exp in
  let core_expr = Ast_core.e_ascription core_expr ligo_obj_ty in
  let* core_expr =
    let open Ast_core in
    let aux exp (s,(mv,mt,t)) : (expression,_) result =
      print_endline (Format.asprintf "name: %s" s);
      print_endline (Format.asprintf "type: %a" Ast_typed.PP.type_expression t);
      print_endline (Format.asprintf "value: %a" Tezos_utils.Michelson.pp mv);
      print_endline (Format.asprintf "type-mich: %a" Tezos_utils.Michelson.pp mt);
      let let_binder = Location.wrap @@ Var.of_name s in
      let* applied = simple_val_insertion_core ~loc mt mv t in
      ok @@ e_let_in {var=let_binder;ascr=None} applied exp false
    in
    bind_fold_right_list aux core_expr subst_lst
  in
  let core_expr = Ast_core.e_ascription core_expr ligo_obj_ty in
  (* let _ = print_endline (Format.asprintf "prog: %a" Ast_core.PP.expression core_expr) in *)
  let* typed_exp,_ = Of_core.compile_expression ~infer:false ~env:_typed_env core_expr in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  (* let _ = print_endline (Format.asprintf "mini: %a" Mini_c.PP.expression mini_c_exp) in *)
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

let rec compile_simple_val ~loc : Ligo_interpreter.Types.value -> (_, _ ) result =
  fun v ->
    (* TODO, this should take a type expression *)
    let open Ligo_interpreter.Types in
    let open Tezos_utils.Michelson in
    match v with
    | V_Ct (C_int x)    -> ok (int x    , prim "int"    , Ast_typed.t_int ())
    | V_Ct (C_string x) -> ok (string x , prim "string" , Ast_typed.t_string ())
    | V_Ct (C_nat x)    -> ok (int x    , prim "nat"    , Ast_typed.t_nat ())
    | V_Ct (C_bytes x)  -> ok (bytes x  , prim "bytes"  , Ast_typed.t_bytes ())
    | V_Ct (C_unit)     -> ok (d_unit   , prim "unit"   , Ast_typed.t_unit ())
    | V_Ct (C_address x) ->
      let x = Format.asprintf "%a" Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.pp x in
      ok (string x, prim "address" , Ast_typed.t_address ())
    | V_Record m when LMap.cardinal m = 2 -> 
      let a = LMap.find (Label "0") m in
      let b = LMap.find (Label "1") m in
      let* (mva,mta,lta) = compile_simple_val ~loc a in
      let* (mvb,mtb,ltb) = compile_simple_val ~loc b in
      ok (d_pair mva mvb , t_pair mta mtb, Ast_typed.t_pair lta ltb)
    (* | V_Construct (ctor, value) ->
     *    let* value,_,_ = compile_simple_val ~loc value in
     *    ok (prim ~children:[value] ctor, prim "int") *)
       (* ok @@ Ast_typed.e_a_constructor ctor arg ty *)
       (* let _ = print_endline (Format.asprintf "%a" Ligo_interpreter.PP.pp_value value) in
        * ok (prim ctor, prim "int", ty) *)
    | V_Michelson (Ty_code t) -> ok t
    | _ -> fail @@ Errors.generic_error loc "The value in meta langugage is too complex to be compiled to michelson"
(*  *)

(* let rec val_to_ast ~loc : Ligo_interpreter.Types.value -> (_, _ ) result =
 *   fun v ->
 *     let open Ligo_interpreter.Types in
 *     match v with
 *     | V_Ct C_unit -> ok Ast_typed.e_a_unit
 *     | V_Ct (C_bool b) -> ok @@ Ast_typed.e_a_bool b
 *     | V_Ct (C_int x) -> ok @@ Ast_typed.e_a_int x
 *     | V_Ct (C_nat x) -> ok @@ Ast_typed.e_a_nat x
 *     | V_Ct (C_timestamp t) -> ok @@ Ast_typed.e_a_timestamp t
 *     | V_Ct (C_string s) -> ok @@ Ast_typed.e_a_string (Simple_utils.Ligo_string.standard s)
 *     | V_Ct (C_bytes b) -> ok @@ Ast_typed.e_a_bytes b
 *     | V_Ct (C_address a) -> let x = Format.asprintf "%a" Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.pp a in ok @@ Ast_typed.e_a_address x
 *     | V_Construct ("None", _value, ty) -> ok @@ Ast_typed.e_a_none ty
 *     | V_Construct (ctor, arg, ty) ->
 *        let* arg = val_to_ast ~loc arg in
 *        ok @@ Ast_typed.e_a_constructor ctor arg ty
 *     | V_Func_val v -> ok v.orig_lambda
 *     | V_Michelson (Ty_code (expr, expr_ty, ty_exp)) ->
 *        let* mini_c = trace Main_errors.decompile_michelson @@ Stacking.Decompiler.decompile_value expr_ty expr in
 *        let* typed = trace Main_errors.decompile_mini_c @@ Spilling.decompile mini_c ty_exp in
 *        ok @@ typed
 *     | v -> let _ = print_endline (Format.asprintf "val: %a" Ligo_interpreter.PP.pp_value v) in
 *            fail @@ Errors.generic_error loc "Cannot be abstracted"
 *     (\* | _ -> ok @@ Ast_typed.e_a_unit *\)
 *     (\* | V_Func_val v -> ok @@ v.orig_lambda
 *      *  *\)
 * 
 * let compile_simple_val ~loc : Ligo_interpreter.Types.value -> (_, _ ) result =
 *   fun v ->
 *     let open Ligo_interpreter.Types in
 *     let* typed_exp = val_to_ast ~loc v in
 *     let* compiled_exp = compile_value typed_exp in
 *     let* options        = Run.Of_michelson.make_dry_run_options {now=None ; amount="" ; balance="" ; sender=None ; source=None ; parameter_ty = None } in
 *     let* runres = Run.Of_michelson.run_expression ~options compiled_exp.expr compiled_exp.expr_ty in
 *     let* (expr_ty,expr) = match runres with | Success x -> ok x | Fail _ -> fail @@ Errors.generic_error loc "Running failed" in
 *     let expr = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr) in
 *     let expr_ty = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr_ty) in
 *     (\* let ret = V_Michelson (Ty_code (expr, expr_ty, typed_exp.type_expression)) in *\)
 *     ok (expr, expr_ty, typed_exp.type_expression) *)

(* let rec compile_val ~loc : Ligo_interpreter.Types.value -> (_, _ ) result =
 *   fun v ->
 *     (\* TODO, this should take a type expression *\)
 *     let open Ligo_interpreter.Types in
 *     let open Tezos_utils.Michelson in
 *     match v with
 *     | V_Ct (C_int x)    -> ok (int x    , prim "int"    , Ast_typed.t_int ())
 *     | V_Ct (C_string x) -> ok (string x , prim "string" , Ast_typed.t_string ())
 *     | V_Ct (C_nat x)    -> ok (int x    , prim "nat"    , Ast_typed.t_nat ())
 *     | V_Ct (C_bytes x)  -> ok (bytes x  , prim "bytes"  , Ast_typed.t_bytes ())
 *     | V_Ct (C_unit)     -> ok (d_unit   , prim "unit"   , Ast_typed.t_unit ())
 *     | V_Ct (C_address x) ->
 *       let x = Format.asprintf "%a" Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.pp x in
 *       ok (string x, prim "address" , Ast_typed.t_address ())
 *     | V_Record m when LMap.cardinal m = 2 -> 
 *       let a = LMap.find (Label "0") m in
 *       let b = LMap.find (Label "1") m in
 *       let* (mva,mta,lta) = compile_simple_val ~loc a in
 *       let* (mvb,mtb,ltb) = compile_simple_val ~loc b in
 *       ok (d_pair mva mvb , t_pair mta mtb, Ast_typed.t_pair lta ltb)
 *     | V_Construct (ctor, value) -> ok (
 *     | _ -> ok (d_unit   , prim "unit"   , Ast_typed.t_unit ())
 * (\* fail @@ Errors.generic_error loc "The value in meta langugage is too complex to be compiled t *\)(\* o michelson" *\) *)

let storage_retreival_dummy_ty = Tezos_utils.Michelson.prim "unit"
