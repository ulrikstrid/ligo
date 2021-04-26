open Ast_core
open Trace
open Errors

let is_not_shadowable attrs = not attrs.shadowable
let in_vars var vars = List.mem ~compare:(Location.compare_content ~compare:Var.compare) var vars

let rec get_all_pattern_vars : type_expression pattern -> expression_variable list = fun pattern ->
  match pattern.wrap_content with
  | P_var {var;attributes;} when is_not_shadowable attributes -> [var]
  | P_list (List l) ->
     List.concat @@ List.map get_all_pattern_vars l
  | P_list (Cons (hd, tl)) ->
     let hd = get_all_pattern_vars hd in
     let tl = get_all_pattern_vars tl in
     hd @ tl
  | P_variant (_, Some p) ->
     get_all_pattern_vars p
  | P_tuple l ->
     List.concat @@ List.map get_all_pattern_vars l
  | P_record (_, ps) ->
     List.concat @@ List.map get_all_pattern_vars ps
  | _ -> []

let rec shadowing_match_case : expression_variable list -> (expression, type_expression) match_case -> (unit, _) result = fun vars {pattern;body} ->
  let vars_in_pattern = get_all_pattern_vars pattern in
  shadowing_expr (vars_in_pattern @ vars) body
and shadowing_expr : expression_variable list -> expression -> (unit, _) result = fun vars expr ->
  match expr.expression_content with
  | E_literal _ | E_constant _ | E_variable _ -> ok @@ ()
  | E_application {lamb;args} ->
     let%bind _ = shadowing_expr vars lamb in
     shadowing_expr vars args
  | E_lambda {binder={var;_};_} when in_vars var vars ->
     fail @@ shadowing var
  | E_lambda {binder={var;attributes;_};result} when is_not_shadowable attributes ->
     shadowing_expr (var :: vars) result
  | E_lambda {result;_} ->
     shadowing_expr vars result
  | E_recursive {fun_name;_} when in_vars fun_name vars ->
     fail @@ shadowing fun_name
  | E_recursive {lambda={result;_};_} ->
     shadowing_expr vars result
  | E_let_in {let_binder={var;_};_} when in_vars var vars ->
     fail @@ shadowing var
  | E_let_in {let_binder={var;attributes;_};rhs;let_result} when is_not_shadowable attributes ->
     let%bind _ = shadowing_expr vars rhs in
     shadowing_expr (var :: vars) let_result
  | E_let_in {rhs;let_result;_} ->
     let%bind _ = shadowing_expr vars rhs in
     shadowing_expr vars let_result
  | E_type_in {let_result;_} ->
     shadowing_expr vars let_result
  | E_mod_in {let_result;_} ->
     shadowing_expr vars let_result
  | E_mod_alias {result;_} ->
     shadowing_expr vars result
  | E_raw_code _ ->
     ok @@ ()
  | E_constructor {element;_} ->
     shadowing_expr vars element
  | E_matching {matchee;cases} ->
     let%bind _ = shadowing_expr vars matchee in
     let%bind _ = bind_map_list (shadowing_match_case vars) cases in
     ok @@ ()
  | E_record rows ->
     let%bind _ = Helpers.bind_map_lmap (fun expr -> shadowing_expr vars expr) rows in
     ok @@ ()
  | E_record_accessor _ -> ok @@ ()
  | E_record_update _ -> ok @@ ()
  | E_ascription {anno_expr} ->
     shadowing_expr vars anno_expr
  | E_module_accessor _ -> ok @@ ()

let rec shadowing_map_module : module' -> (module', _) result = fun m' ->
  let _self = shadowing_map_module in
  let aux = fun (l : expression_variable list) (x : declaration Location.wrap) ->
    match x.wrap_content with
    | Declaration_constant {expr;binder={var;attributes;_};_} ->
       if in_vars var l then
         fail @@ shadowing var
       else
         begin
           let%bind _ = shadowing_expr l expr in
           if is_not_shadowable attributes then
             ok @@ var :: l
           else 
             ok @@ l
         end
    | _ -> ok @@ l
  in
  let%bind _ = bind_fold_list (fun a d -> aux a d) [] m' in
  ok @@ m'
