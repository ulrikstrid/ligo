open Trace
module Errors = Errors
open Errors

module I = Ast_core
module O = Ast_typed
open O.Combinators

module Environment = O.Environment

type environment = Environment.t

let cast_var (orig: 'a Var.t Location.wrap) = { orig with wrap_content = Var.todo_cast orig.wrap_content}
let assert_type_expression_eq = Helpers.assert_type_expression_eq

let rec type_module ~init_env (p:I.module_) : (environment * O.module_fully_typed, typer_error) result =
  let aux (e, acc:(environment * O.declaration Location.wrap list)) (d:I.declaration Location.wrap) =
    let%bind (e', d') = type_declaration e d in
    ok (e', d' :: acc)
  in
  let%bind (e, lst) =
    trace (module_error_tracer p) @@
    bind_fold_list aux (init_env, []) p in
  let p = List.rev lst in
  (* the typer currently in use doesn't use unification variables, so there is no need to check for their absence. *)
  let p = O.Module_Fully_Typed p in
  ok @@ (e,p)


and type_declaration env : I.declaration Location.wrap -> (environment * O.declaration Location.wrap, typer_error) result =
fun d ->
let return ?(loc = d.location) e (d : O.declaration) = ok (e, Location.wrap ~loc d) in
match Location.unwrap d with
  | Declaration_type {type_binder ; type_expr} ->
    let type_binder = Var.todo_cast type_binder in
    let%bind tv = evaluate_type env type_expr in
    let env' = Environment.add_type type_binder tv env in
    return env' @@ Declaration_type { type_binder ; type_expr = tv }
  | Declaration_constant {name ; binder ; attr={inline} ; expr} -> (
    let%bind tv'_opt = bind_map_option (evaluate_type env) binder.ascr in
    let%bind expr =
      trace (constant_declaration_error_tracer binder.var expr tv'_opt) @@
      type_expression' ?tv_opt:tv'_opt env expr in
    let binder : O.expression_variable = cast_var binder.var in
    let post_env = Environment.add_ez_declaration binder expr env in
    return post_env @@ Declaration_constant { name ; binder ; expr ; inline}
  )
  | Declaration_module {module_binder;module_} -> (
    let%bind e,module_ = type_module ~init_env:env module_ in
    let post_env = Environment.add_module module_binder e env in
    return post_env @@ Declaration_module { module_binder; module_}
  )
  | Module_alias {alias;binders} -> (
    let aux env binder =
      trace_option (unbound_module_variable env binder d.location)
      @@ Environment.get_module_opt binder env in
    let%bind e = bind_fold_ne_list aux env binders in
    let post_env = Environment.add_module alias e env in
    return post_env @@ Module_alias { alias; binders}
  )

and evaluate_type (e:environment) (t:I.type_expression) : (O.type_expression, typer_error) result =
  let return tv' = ok (make_t ~loc:t.location tv' (Some t)) in
  match t.type_content with
  | T_arrow {type1;type2} ->
      let%bind type1 = evaluate_type e type1 in
      let%bind type2 = evaluate_type e type2 in
      return (T_arrow {type1;type2})
  | T_sum m -> (
    let%bind lmap =
      let aux ({associated_type;michelson_annotation;decl_pos} : I.row_element) =
        let%bind associated_type = evaluate_type e associated_type in
        ok @@ ({associated_type;michelson_annotation;decl_pos} : O.row_element)
      in
      Stage_common.Helpers.bind_map_lmap aux m.fields
    in
    let sum : O.rows  = match Environment.get_sum lmap e with
      | None ->
        let layout = Option.unopt ~default:default_layout m.layout in
        {content = lmap; layout}
      | Some r -> r
    in
    let ty = make_t (T_sum sum) None in
    let%bind () =
      let aux acc k _v = match Environment.get_constructor k e with
          | Some (_,type_) ->
            if Ast_typed.Misc.type_expression_eq (acc,type_) then ok type_
            else if I.LMap.mem (Label "M_left") m.fields || I.LMap.mem (Label "M_right") m.fields then ok type_
            else fail (redundant_constructor e k t.location)
          | None -> ok acc in
      let%bind _ = Stage_common.Helpers.bind_fold_lmap aux ty m.fields in ok ()
    in
    return @@ T_sum sum
  )
  | T_record m -> (
    let aux ({associated_type;michelson_annotation;decl_pos}: I.row_element) =
      let%bind associated_type = evaluate_type e associated_type in
      ok @@ ({associated_type;michelson_annotation;decl_pos} : O.row_element)
    in
    let%bind lmap = Stage_common.Helpers.bind_map_lmap aux m.fields in
    let record : O.rows = match Environment.get_record lmap e with
    | None ->
      let layout = Option.unopt ~default:default_layout m.layout in
      {content=lmap;layout}
    | Some (_,r) ->  r
    in
    return @@ T_record record
  )
  | T_variable variable ->
    (* Check that the variable is in the environment *)
    let name : O.type_variable = Var.todo_cast variable in
    trace_option (unbound_type_variable e name t.location) @@
      Environment.get_type_opt (name) e
  | T_app {type_operator;arguments} -> (
    let name : O.type_variable = Var.todo_cast type_operator in
    let%bind v = trace_option (unbound_type_variable e name t.location) @@
      Environment.get_type_opt name e in
    let aux : O.type_injection -> (O.type_expression, typer_error) result = fun inj ->
      (*handles converters*)
      let open Stage_common.Constant in
      let {language=_ ; injection ; parameters} : O.type_injection = inj in
      match Ligo_string.extract injection, parameters with
      | (i, [t]) when String.equal i michelson_pair_right_comb_name ->
        let%bind lmap = match t.type_content with
            | T_record lmap when (not (Ast_typed.Helpers.is_tuple_lmap lmap.content)) -> ok lmap
            | _ -> fail (michelson_comb_no_record t.location) in
        let record = Michelson_type_converter.convert_pair_to_right_comb (Ast_typed.LMap.to_kv_list_rev lmap.content) in
        return @@ record
      | (i, [t]) when String.equal i  michelson_pair_left_comb_name ->
          let%bind lmap = match t.type_content with
            | T_record lmap when (not (Ast_typed.Helpers.is_tuple_lmap lmap.content)) -> ok lmap
            | _ -> fail (michelson_comb_no_record t.location) in
          let record = Michelson_type_converter.convert_pair_to_left_comb (Ast_typed.LMap.to_kv_list_rev lmap.content) in
          return @@ record
      | (i, [t]) when String.equal i michelson_or_right_comb_name ->
        let%bind cmap = match t.type_content with
            | T_sum cmap -> ok cmap.content
            | _ -> fail (michelson_comb_no_variant t.location) in
          let pair = Michelson_type_converter.convert_variant_to_right_comb (Ast_typed.LMap.to_kv_list_rev cmap) in
          return @@ pair
      | (i, [t]) when String.equal i michelson_or_left_comb_name ->
        let%bind cmap = match t.type_content with
            | T_sum cmap -> ok cmap.content
            | _ -> fail (michelson_comb_no_variant t.location) in
          let pair = Michelson_type_converter.convert_variant_to_left_comb (Ast_typed.LMap.to_kv_list_rev cmap) in
          return @@ pair
      | _ -> return (T_constant inj)
    in
    match get_param_inj v with
    | Some (language,injection,parameters) ->
      let arg_env = List.length parameters in
      let arg_actual = List.length arguments in
      let%bind parameters =
        if arg_env <> arg_actual then fail @@ type_constant_wrong_number_of_arguments type_operator arg_env arg_actual t.location
        else bind_map_list (evaluate_type e) arguments
      in
      let inj : O.type_injection = {language ; injection ; parameters } in
      aux inj
    | None -> failwith "variable with parameters is not an injection"
  )
  | T_module_accessor {module_name; element} ->
    let%bind module_ = match Environment.get_module_opt module_name e with
      Some m -> ok m
    | None   -> fail @@ unbound_module_variable e module_name t.location
    in
    evaluate_type module_ element
  | T_singleton x -> return (T_singleton x)

and type_expression : environment -> ?tv_opt:O.type_expression -> I.expression -> (O.environment * O.expression, typer_error) result
  = fun e ?tv_opt ae ->
    let%bind res = type_expression' e ?tv_opt ae in
    ok (e, res)

and type_expression' : environment -> ?tv_opt:O.type_expression -> I.expression -> (O.expression, typer_error) result = fun e ?tv_opt ae ->
  let module L = Logger.Stateful() in
  let return expr tv =
    let%bind () =
      match tv_opt with
      | None -> ok ()
      | Some tv' -> assert_type_expression_eq ae.location (tv' , tv) in
    let location = ae.location in
    ok @@ make_e ~location expr tv in
  trace (expression_tracer ae) @@
  match ae.expression_content with
  (* Basic *)
  | E_variable name' ->
      let name = cast_var name' in
      let%bind tv' =
        trace_option (unbound_variable e name ae.location)
        @@ Environment.get_opt name e in
      return (E_variable name) tv'.type_value
  | E_literal Literal_unit ->
      return (E_literal (Literal_unit)) (t_unit ())
  | E_literal (Literal_string s) ->
      return (E_literal (Literal_string s)) (t_string ())
  | E_literal (Literal_key s) ->
      return (E_literal (Literal_key s)) (t_key ())
  | E_literal (Literal_key_hash s) ->
      return (E_literal (Literal_key_hash s)) (t_key_hash ())
  | E_literal (Literal_chain_id s) ->
      return (E_literal (Literal_chain_id s)) (t_chain_id ())
  | E_literal (Literal_signature s) ->
      return (E_literal (Literal_signature s)) (t_signature ())
  | E_literal (Literal_bytes s) ->
      return (E_literal (Literal_bytes s)) (t_bytes ())
  | E_literal (Literal_int n) ->
      return (E_literal (Literal_int n)) (t_int ())
  | E_literal (Literal_nat n) ->
      return (E_literal (Literal_nat n)) (t_nat ())
  | E_literal (Literal_timestamp n) ->
      return (E_literal (Literal_timestamp n)) (t_timestamp ())
  | E_literal (Literal_mutez n) ->
      return (E_literal (Literal_mutez n)) (t_mutez ())
  | E_literal (Literal_address s) ->
      return (e_address s) (t_address ())
  | E_literal (Literal_operation op) ->
      return (e_operation op) (t_operation ())
  | E_record_accessor {record;path} ->
      let%bind e' = type_expression' e record in
      let aux (prev:O.expression) (a:I.label) : (O.expression , typer_error) result =
            let property = a in
            let%bind r_tv = trace_option (bad_record_access property ae prev.type_expression ae.location) @@
             get_t_record prev.type_expression in
            let%bind tv =
              trace_option (bad_record_access property ae prev.type_expression ae.location) @@
              O.LMap.find_opt property r_tv.content in
            let location = ae.location in
            ok @@ make_e ~location (E_record_accessor {record=prev; path=property}) tv.associated_type
      in
      let%bind ae =
      trace (record_access_tracer e') @@ aux e' path in
      (* check type annotation of the final accessed element *)
      let%bind () =
        match tv_opt with
        | None -> ok ()
        | Some tv' -> assert_type_expression_eq ae.location (tv' , ae.type_expression) in
      ok(ae)
  | E_constructor {constructor = Label s ; element} when String.equal s "M_left" || String.equal s "M_right" -> (
    let%bind t = trace_option (michelson_or (Label s) ae.location) @@ tv_opt in
    let%bind expr' = type_expression' e element in
    ( match t.type_content with
      | T_sum c ->
        let {associated_type ; _} : O.row_element = O.LMap.find (Label s) c.content in
        let%bind () = assert_type_expression_eq expr'.location (expr'.type_expression, associated_type) in
        return (E_constructor {constructor = Label s; element=expr'}) t
      | _ -> fail (michelson_or (Label s) ae.location)
    )
  )
  (* Sum *)
  | E_constructor {constructor; element} ->
      let%bind (c_tv, sum_tv) = trace_option (unbound_constructor e constructor ae.location) @@
        Environment.get_constructor constructor e in
      let%bind expr' = type_expression' e element in
      let%bind _assert = assert_type_expression_eq expr'.location (expr'.type_expression, c_tv) in
      return (E_constructor {constructor; element=expr'}) sum_tv
  (* Record *)
  | E_record m ->
      let%bind m' = Stage_common.Helpers.bind_map_lmap (type_expression' e) m in
      let lmap = O.LMap.map (fun e -> ({associated_type = get_type_expression e; michelson_annotation = None; decl_pos=0}:O.row_element)) m' in
      let record_type = match Environment.get_record lmap e with
        | None -> t_record ~layout:default_layout lmap
        | Some (orig_var,r) -> make_t_orig_var (T_record r) None orig_var
      in
      return (E_record m') record_type
  | E_record_update {record; path; update} ->
    let%bind record = type_expression' e record in
    let%bind update = type_expression' e update in
    let wrapped = get_type_expression record in
    let%bind tv =
      match wrapped.type_content with
      | T_record {content;_} -> (
          let%bind {associated_type;_} = trace_option (bad_record_access path ae wrapped update.location) @@
            O.LMap.find_opt path content in
          ok associated_type
      )
      | _ -> failwith "Update an expression which is not a record"
    in
    let%bind () = assert_type_expression_eq update.location (tv, get_type_expression update) in
    return (E_record_update {record; path; update}) wrapped
  (* Data-structure *)
  | E_lambda lambda ->
     let lambda =
         match tv_opt with
         | None -> lambda
         | Some tv' ->
            match O.get_t_function tv' with
            | None -> lambda
            | Some (input_type,_ ) ->
               let input_type = Untyper.untype_type_expression_nofail input_type in
               match lambda.binder.ascr with
               | None -> let binder = {lambda.binder with ascr = Some input_type } in
                         { lambda with binder = binder }
               | Some _ -> lambda in
     let%bind (lambda,lambda_type) = type_lambda e lambda in
     return (E_lambda lambda ) lambda_type
  | E_constant {cons_name=( C_LIST_FOLD | C_MAP_FOLD | C_SET_FOLD | C_FOLD) as opname ;
                arguments=[
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ }) as _lambda ;
                    collect ;
                    init_record ;
                  ]} ->
      let open Stage_common.Constant in
      (* this special case is here to force annotation of the untyped lambda
         generated by pascaligo's for_collect loop *)
      let lname = cast_var lname in
      let%bind (v_col , v_initr ) = bind_map_pair (type_expression' e) (collect , init_record ) in
      let tv_col = get_type_expression v_col   in (* this is the type of the collection  *)
      let tv_out = get_type_expression v_initr in (* this is the output type of the lambda*)
      let%bind input_type = match tv_col.type_content with
        | O.T_constant {language=_ ; injection ; parameters=[t]}
            when String.equal (Ligo_string.extract injection) list_name
              || String.equal (Ligo_string.extract injection) set_name ->
          ok @@ make_t_ez_record (("0",tv_out)::[("1",t)])
        | O.T_constant {language=_ ; injection ; parameters=[k;v]}
          when String.equal (Ligo_string.extract injection) map_name
            || String.equal (Ligo_string.extract injection) big_map_name ->
          ok @@ make_t_ez_record (("0",tv_out)::[("1",make_t_ez_record [("0",k);("1",v)])])
        | _ -> fail @@ bad_collect_loop tv_col ae.location in
      let e' = Environment.add_ez_binder lname input_type e in
      let%bind body = type_expression' ?tv_opt:(Some tv_out) e' result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_function input_type output_type ()) in
      let lst' = [lambda'; v_col; v_initr] in
      let tv_lst = List.map get_type_expression lst' in
      let%bind (opname', tv) =
        type_constant opname ae.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_FOLD_WHILE as opname;
                arguments = [
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ }) as _lambda ;
                    init_record ;
                ]} ->
      let%bind v_initr = type_expression' e init_record in
      let tv_out = get_type_expression v_initr in
      let input_type  = tv_out in
      let lname = cast_var lname in
      let e' = Environment.add_ez_binder lname input_type e in
      let%bind body = type_expression' e' result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_function input_type output_type ()) in
      let lst' = [lambda';v_initr] in
      let tv_lst = List.map get_type_expression lst' in
      let%bind (opname',tv) = type_constant opname ae.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_CREATE_CONTRACT as cons_name;arguments} ->
      let%bind lst' = bind_list @@ List.map (type_expression' e) arguments in
      let%bind () = match lst' with
        | { expression_content = O.E_lambda l ; _ } :: _ ->
          let open Ast_typed.Misc in
          let fvs = Free_variables.lambda [] l in
          if List.length fvs = 0 then ok ()
          else fail @@ fvs_in_create_contract_lambda ae (List.hd fvs)
        | _ -> fail @@ create_contract_lambda C_CREATE_CONTRACT ae
      in
      let tv_lst = List.map get_type_expression lst' in
      let%bind (name', tv) =
        type_constant cons_name ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name=C_SET_ADD|C_CONS as cst;arguments=[key;set]} ->
      let%bind key' =  type_expression' e key in
      let tv_key = get_type_expression key' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> match cst with
            C_SET_ADD -> t_set tv_key
          | C_CONS -> t_list tv_key
          | _ -> failwith "Only C_SET_ADD and C_CONS are possible because those were the two cases matched above"
      in
      let%bind set' =  type_expression' e ~tv_opt:tv set in
      let tv_set = get_type_expression set' in
      let tv_lst = [tv_key;tv_set] in
      let%bind (name', tv) = type_constant cst ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';set']}) tv
  | E_constant {cons_name=C_MAP_ADD as cst; arguments=[key;value;map]} ->
      let%bind key' = type_expression' e key in
      let%bind val' = type_expression' e value in
      let tv_key = get_type_expression key' in
      let tv_val = get_type_expression val' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> t_map_or_big_map tv_key tv_val
      in
      let%bind map' =  type_expression' e ~tv_opt:tv map in
      let tv_map = get_type_expression map' in
      let tv_lst = [tv_key;tv_val;tv_map] in
      let%bind (name', tv) = type_constant cst ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';val';map']}) tv
  | E_constant {cons_name = C_POLYMORPHIC_ADD;arguments} ->
      let%bind lst' = bind_list @@ List.map (type_expression' e) arguments in
      let tv_lst = List.map get_type_expression lst' in
      let cst = (match lst' with 
        | {expression_content = E_literal (Literal_string _); _ } :: _ -> S.C_CONCAT
        | {expression_content = E_constant {cons_name = C_ADD; _ }; _ } :: _ -> C_ADD
        | {expression_content = E_constant {cons_name = C_CONCAT; _ }; _ } :: _ -> C_CONCAT
        | {expression_content = E_literal (Literal_int _); _ } :: _ -> C_ADD
        | {expression_content = E_variable _; type_expression = texpr } :: _ ->
                if is_t_string texpr then
                  C_CONCAT
                else
                  C_ADD
      | _ -> C_ADD
      ) in
      let%bind (name', tv) =
        type_constant cst ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name;arguments} ->
      let%bind lst' = bind_list @@ List.map (type_expression' e) arguments in
      let tv_lst = List.map get_type_expression lst' in
      let%bind (name', tv) =
        type_constant cons_name ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_application {lamb; args} ->
      let%bind lamb' = type_expression' e lamb in
      let%bind args' = type_expression' e args in
      let%bind tv = match lamb'.type_expression.type_content with
        | T_arrow {type1;type2} ->
            let%bind _ = assert_type_expression_eq args'.location (type1, args'.type_expression) in
            ok type2
        | _ ->
          fail @@ type_error_approximate
            ~expression:lamb
            ~actual:lamb'.type_expression
      in
      return (E_application {lamb=lamb'; args=args'}) tv
  (* Advanced *)
  | E_matching {matchee;cases} -> (
    let%bind matchee' = type_expression' e matchee in
    let matcheevar = Location.wrap (Var.fresh ()) in
    let aux : (I.expression, I.type_expression) I.match_case -> ((I.type_expression I.pattern * O.type_expression) list * (I.expression * O.environment)) =
      fun {pattern ; body} -> ([(pattern,matchee'.type_expression)], (body,e))
    in
    let eqs = List.map aux cases in
    let%bind case_exp = Pattern_matching.compile_matching ~err_loc:ae.location ~type_f:(type_expression') ~body_t:(tv_opt) matcheevar eqs in
    let case_exp = { case_exp with location = ae.location } in
    let x = O.e_let_in matcheevar matchee' case_exp false in
    return x case_exp.type_expression
  )
  | E_let_in {let_binder = {var ; ascr} ; rhs ; let_result; inline} ->
    let%bind rhs_tv_opt = bind_map_option (evaluate_type e) ascr in
    let%bind rhs = type_expression' ?tv_opt:rhs_tv_opt e rhs in
    let binder = cast_var var in
    let e' = Environment.add_ez_declaration binder rhs e in
    let%bind let_result = type_expression' e' let_result in
    return (E_let_in {let_binder = binder; rhs; let_result; inline}) let_result.type_expression
  | E_type_in {type_binder; rhs ; let_result} ->
    let%bind rhs = evaluate_type e rhs in
    let e' = Environment.add_type type_binder rhs e in
    let%bind let_result = type_expression' e' let_result in
    return (E_type_in {type_binder; rhs; let_result}) let_result.type_expression
  | E_mod_in {module_binder; rhs; let_result} ->
    let%bind env,rhs = type_module ~init_env:e rhs in
    let e' = Environment.add_module module_binder env e in
    let%bind let_result = type_expression' e' let_result in
    return (E_mod_in {module_binder; rhs; let_result}) let_result.type_expression
  | E_mod_alias {alias; binders; result} ->
    let aux e binder =
      trace_option (unbound_module_variable e binder ae.location) @@
      Environment.get_module_opt binder e in
    let%bind env = bind_fold_ne_list aux e binders in
    let e' = Environment.add_module alias env e in
    let%bind result = type_expression' e' result in
    return (E_mod_alias {alias; binders; result}) result.type_expression
  | E_raw_code {language;code} ->
    let%bind (code,type_expression) = trace_option (expected_ascription code) @@
      I.get_e_ascription code.expression_content in
    let%bind code = type_expression' e code in
    let%bind type_expression = evaluate_type e type_expression in
    let code = {code with type_expression} in
    return (E_raw_code {language;code}) code.type_expression
  | E_recursive {fun_name; fun_type; lambda} ->
    let fun_name = cast_var fun_name in
    let%bind fun_type = evaluate_type e fun_type in
    let e' = Environment.add_ez_binder fun_name fun_type e in
    let%bind (lambda,_) = type_lambda e' lambda in
    return (E_recursive {fun_name;fun_type;lambda}) fun_type
  | E_ascription {anno_expr; type_annotation} ->
    let%bind tv = evaluate_type e type_annotation in
    let%bind expr' = type_expression' ~tv_opt:tv e anno_expr in
    let%bind type_annotation =
      trace_option (corner_case "merge_annotations (Some ...) (Some ...) failed") @@
      O.merge_annotation
        (Some tv)
        (Some expr'.type_expression)
        O.assert_type_expression_eq in
    (* check type annotation of the expression as a whole (e.g. let x : t = (v : t') ) *)
    let%bind () =
      match tv_opt with
      | None -> ok ()
      | Some tv' -> assert_type_expression_eq anno_expr.location (tv' , type_annotation) in
    ok {expr' with type_expression=type_annotation}
  | E_module_accessor {module_name; element} ->
    let%bind module_env = match Environment.get_module_opt module_name e with
      Some m -> ok m
    | None   -> fail @@ unbound_module_variable e module_name ae.location
    in
    let%bind element = type_expression' ?tv_opt module_env element in
    return (E_module_accessor {module_name; element}) element.type_expression


and type_lambda e {
      binder ;
      output_type ;
      result ;
    } =
      let%bind input_type =
        bind_map_option (evaluate_type e) binder.ascr in
      let%bind output_type =
        bind_map_option (evaluate_type e) output_type
      in
      let binder = cast_var binder.var in
      let%bind input_type = trace_option (missing_funarg_annotation binder) input_type in
      let e' = Environment.add_ez_binder binder input_type e in
      let%bind body = type_expression' ?tv_opt:output_type e' result in
      let output_type = body.type_expression in
      ok (({binder; result=body}:O.lambda),(t_function input_type output_type ()))



and type_constant (name:I.constant') (loc:Location.t) (lst:O.type_expression list) (tv_opt:O.type_expression option) : (O.constant' * O.type_expression , typer_error) result =
  let%bind typer = Constant_typers.constant_typers loc name in
  let%bind tv = typer lst tv_opt in
  ok (name, tv)

let untype_literal (l:O.literal) : (I.literal , typer_error) result =
  let open I in
  match l with
  | Literal_unit -> ok Literal_unit
  | Literal_nat n -> ok (Literal_nat n)
  | Literal_timestamp n -> ok (Literal_timestamp n)
  | Literal_mutez n -> ok (Literal_mutez n)
  | Literal_int n -> ok (Literal_int n)
  | Literal_string s -> ok (Literal_string s)
  | Literal_signature s -> ok (Literal_signature s)
  | Literal_key s -> ok (Literal_key s)

  | Literal_key_hash s -> ok (Literal_key_hash s)
  | Literal_chain_id s -> ok (Literal_chain_id s)
  | Literal_bytes b -> ok (Literal_bytes b)
  | Literal_address s -> ok (Literal_address s)
  | Literal_operation s -> ok (Literal_operation s)

let rec untype_type_expression (t:O.type_expression) : (I.type_expression, typer_error) result =
  let self = untype_type_expression in
  let return t = ok @@ I.make_t t in
  match t.type_content with
  | O.T_sum {content ; layout} ->
     let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
       let%bind associated_type = untype_type_expression associated_type in
       let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
       ok @@ v' in
     let%bind x' = Stage_common.Helpers.bind_map_lmap aux content in
     return @@ I.T_sum { fields = x' ; layout = Some layout }
  | O.T_record {content;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let%bind associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      ok @@ v' in
    let%bind x' = Stage_common.Helpers.bind_map_lmap aux content in
    return @@ I.T_record {fields = x' ; layout = Some layout}
  )
  | O.T_variable name -> return @@ I.T_variable (Var.todo_cast name)
  | O.T_arrow arr ->
    let%bind arr = Stage_common.Maps.arrow self arr in
    return @@ I.T_arrow arr
  | O.T_constant {language=_;injection;parameters} ->
    let%bind arguments = bind_map_list self parameters in
    let type_operator = Var.of_name (Ligo_string.extract injection) in
    return @@ I.T_app {type_operator;arguments}
  | O.T_module_accessor ma ->
    let%bind ma = Stage_common.Maps.module_access self ma in
    return @@ I.T_module_accessor ma
  | O.T_singleton l ->
    return @@ I.T_singleton l

let rec untype_expression (e:O.expression) : (I.expression , typer_error) result =
  untype_expression_content e.type_expression e.expression_content
and untype_expression_content ty (ec:O.expression_content) : (I.expression , typer_error) result =
  let open I in
  let return e = ok e in
  match ec with
  | E_literal l ->
      let%bind l = untype_literal l in
      return (e_literal l)
  | E_constant {cons_name;arguments} ->
      let%bind lst' = bind_map_list untype_expression arguments in
      return (e_constant cons_name lst')
  | E_variable n ->
      let n = cast_var n in
      return (e_variable (n))
  | E_application {lamb;args} ->
      let%bind f' = untype_expression lamb in
      let%bind arg' = untype_expression args in
      return (e_application f' arg')
  | E_lambda {binder ; result} -> (
      let binder = cast_var binder in
      let io = O.get_t_function_exn ty in
      let%bind (input_type , output_type) =
        bind_map_pair Untyper.untype_type_expression io in
      let%bind result = untype_expression result in
      return (e_lambda {var=binder;ascr=Some input_type} (Some output_type) result)
    )
  | E_constructor {constructor; element} ->
      let%bind p' = untype_expression element in
      return (e_constructor constructor p')
  | E_record r ->
    let%bind r' = Helpers.bind_map_lmap untype_expression r in
    return (e_record r')
  | E_record_accessor {record; path} ->
      let%bind r' = untype_expression record in
      let Label s = path in
      return (e_record_accessor r' (Label s))
  | E_record_update {record=r; path=Label l; update=e} ->
    let%bind r' = untype_expression r in
    let%bind e = untype_expression e in
    return (e_record_update r' (I.Label l) e)
  | E_matching {matchee;cases} -> (
    let%bind matchee = untype_expression matchee in
    match cases with
    | Match_variant {cases ; tv} ->
      (*
        If one day this code is actually executed, and if the list type is still not a tuple type.
        A special case for lists might be required here
      *)
      let aux : Ast_typed.matching_content_case -> (_ match_case, _) result =
        fun { constructor ; pattern ; body } -> (
          let pattern =
            match tv with
            | _ ->
              let proj = Location.wrap @@ P_var { ascr = None ; var = (cast_var pattern) } in
              Location.wrap @@ P_variant (constructor, Some proj)
          in
          let%bind body = untype_expression body in
          ok @@ ({pattern ; body } : (Ast_core.expression, Ast_core.type_expression) match_case)
        )
      in
      let%bind cases = bind_map_list aux cases in
      return (e_matching matchee cases)
    | Match_record {fields ; body ; tv=_} -> (
      let aux : (Ast_typed.label * (Ast_typed.expression_variable * _)) -> label * Ast_core.type_expression pattern =
        fun (Ast_typed.Label label, (proj,_)) -> (
          let proj = Location.wrap @@ P_var { ascr = None ; var = (cast_var proj) } in
          (Label label, proj)
        )
      in
      let (labels,patterns) = List.split @@ List.map aux (LMap.to_kv_list fields) in
      let%bind body = untype_expression body in
      let case = match Ast_typed.Helpers.is_tuple_lmap fields with
        | false ->
          let pattern = Location.wrap (P_record (labels,patterns)) in
          ({ pattern ; body } : _ Ast_core.match_case)
        | true ->
          let pattern = Location.wrap (P_tuple patterns) in
          ({ pattern ; body } : _ Ast_core.match_case)
      in
      return (e_matching matchee [case])
    )
  )
  | E_let_in {let_binder;rhs;let_result; inline} ->
      let let_binder = cast_var let_binder in
      let%bind tv = Untyper.untype_type_expression rhs.type_expression in
      let%bind rhs = untype_expression rhs in
      let%bind result = untype_expression let_result in
      return (e_let_in {var=let_binder ; ascr=(Some tv)} rhs result inline)
  | E_type_in ti ->
    let%bind ti = Stage_common.Maps.type_in untype_expression Untyper.untype_type_expression ti in
    return @@ make_e @@ E_type_in ti
  | E_mod_in {module_binder;rhs;let_result} ->
      let%bind rhs = untype_module_fully_typed rhs in
      let%bind result = untype_expression let_result in
      return @@ e_mod_in module_binder rhs result
  | E_mod_alias ma ->
      let%bind ma = Stage_common.Maps.mod_alias untype_expression ma in 
      return @@ make_e @@ E_mod_alias ma
  | E_raw_code {language; code} ->
      let%bind code = untype_expression code in
      return (e_raw_code language code)
  | E_recursive {fun_name;fun_type; lambda} ->
      let fun_name = cast_var fun_name in
      let%bind fun_type = Untyper.untype_type_expression fun_type in
      let%bind unty_expr= untype_expression_content ty @@ E_lambda lambda in
      let lambda = match unty_expr.expression_content with I.E_lambda l -> l | _ -> failwith "impossible case" in
      return @@ e_recursive fun_name fun_type lambda
  | E_module_accessor ma ->
    let%bind ma = Stage_common.Maps.module_access untype_expression ma in
    return @@ I.make_e @@ E_module_accessor ma

and untype_declaration : O.declaration -> (I.declaration, typer_error) result =
let return (d: I.declaration) = ok @@ d in
function
  Declaration_type {type_binder; type_expr} ->
  let%bind type_expr = untype_type_expression type_expr in
  return @@ Declaration_type {type_binder; type_expr}
| Declaration_constant {name; binder;expr;inline} ->
  let%bind ty = untype_type_expression expr.type_expression in
  let var = Location.map Var.todo_cast binder in
  let%bind expr = untype_expression expr in
  return @@ Declaration_constant {name; binder={var;ascr=Some ty};expr;attr={inline}}
| Declaration_module {module_binder;module_} ->
  let%bind module_ = untype_module_fully_typed module_ in
  return @@ Declaration_module {module_binder;module_}
| Module_alias ma ->
  return @@ Module_alias ma

and untype_module_fully_typed : O.module_fully_typed -> (I.module_, typer_error) result = fun (Module_Fully_Typed m) ->
  let%bind m = bind_map_list (bind_map_location untype_declaration) m in
  ok @@ m

let rec decompile_env (env : Ast_typed.environment) = 
  let%bind expression_environment = bind_map_list decompile_binding env.expression_environment in
  let%bind type_environment       = bind_map_list decompile_type_binding env.type_environment in
  let%bind module_environment     = bind_map_list decompile_module_binding env.module_environment in
  ok @@ Ast_core.{expression_environment; type_environment; module_environment}

and decompile_binding Ast_typed.{expr_var;env_elt} =
  let%bind type_value = untype_type_expression env_elt.type_value in
  let%bind definition = match env_elt.definition with
    ED_binder -> ok @@ Ast_core.ED_binder
  | ED_declaration {expression;free_variables} ->
    let%bind expression = untype_expression expression in
    ok @@ Ast_core.ED_declaration {expression;free_variables} 
  in
  ok @@ Ast_core.{expr_var;env_elt = {type_value;definition}}

and decompile_type_binding Ast_typed.{type_variable;type_} =
  let%bind type_ = untype_type_expression type_ in
  ok @@ Ast_core.{type_variable;type_}

and decompile_module_binding Ast_typed.{module_variable;module_} =
  let%bind module_ = decompile_env module_ in
  ok @@ Ast_core.{module_variable;module_}
