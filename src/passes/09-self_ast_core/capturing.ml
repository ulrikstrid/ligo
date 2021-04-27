open Ast_core
open Trace
open Errors

let compare_vars : expression_variable -> expression_variable -> int =
  Location.compare_content ~compare:Var.compare

let is_not_capturable attrs = not attrs.capturable
let in_vars var vars = List.mem ~compare:compare_vars var vars

let remove_from var vars =
  if List.mem ~compare:compare_vars var vars then
    let ith = List.find_index (fun v -> compare_vars var v = 0) vars in
    List.remove ith vars
  else
    vars

let rec get_pattern_vars : type_expression pattern -> expression_variable list = fun pattern ->
  match pattern.wrap_content with
  | P_var {var} -> [var]
  | P_list (List l) ->
     List.concat @@ List.map get_pattern_vars l
  | P_list (Cons (hd, tl)) ->
     let hd = get_pattern_vars hd in
     let tl = get_pattern_vars tl in
     hd @ tl
  | P_variant (_, Some p) ->
     get_pattern_vars p
  | P_tuple l ->
     List.concat @@ List.map get_pattern_vars l
  | P_record (_, ps) ->
     List.concat @@ List.map get_pattern_vars ps
  | _ -> []

let rec get_fv : expression -> expression_variable list = fun expr ->
  match expr.expression_content with
  | E_literal _ -> []
  | E_variable v -> [v]
  | E_constant {arguments} ->
     List.concat @@ List.map get_fv arguments
  | E_application {lamb;args} ->
     get_fv lamb @ get_fv args
  | E_lambda {binder={var;_};result} ->
     remove_from var @@ get_fv result
  | E_recursive {lambda={result;binder={var;_}};fun_name} ->
     remove_from fun_name @@ remove_from var @@ get_fv result
  | E_let_in {let_binder={var;_};rhs;let_result} ->
     let rhs_vars = get_fv rhs in
     let result_vars = remove_from var @@ get_fv let_result in
     rhs_vars @ result_vars
  | E_type_in {let_result;_} ->
     get_fv let_result
  | E_mod_in {let_result;_} ->
     get_fv let_result
  | E_mod_alias {result;_} ->
     get_fv result
  | E_raw_code _ ->
     []
  | E_constructor {element;_} ->
     get_fv element
  | E_matching {matchee;cases} ->
     let matchee_vars = get_fv matchee in
     let cases_vars = List.map get_fv_case cases in
     matchee_vars @ List.concat cases_vars
  | E_record rows ->
     let rows = LMap.to_list rows in
     List.concat @@ List.map get_fv rows
  | E_ascription {anno_expr} ->
     get_fv anno_expr
  | E_record_accessor _ -> []
  | E_record_update _ -> []
  | E_module_accessor _ -> []
and get_fv_case : (expression, type_expression) match_case -> expression_variable list
  = fun {pattern;body} ->
  let vars_in_pattern = get_pattern_vars pattern in
  let body_vars = get_fv body in
  List.fold_right (fun a b -> remove_from a b) vars_in_pattern body_vars

  
let rec get_some_pattern_vars : type_expression pattern -> expression_variable list = fun pattern ->
  match pattern.wrap_content with
  | P_var {var;attributes;} when is_not_capturable attributes -> [var]
  | P_list (List l) ->
     List.concat @@ List.map get_some_pattern_vars l
  | P_list (Cons (hd, tl)) ->
     let hd = get_some_pattern_vars hd in
     let tl = get_some_pattern_vars tl in
     hd @ tl
  | P_variant (_, Some p) ->
     get_some_pattern_vars p
  | P_tuple l ->
     List.concat @@ List.map get_some_pattern_vars l
  | P_record (_, ps) ->
     List.concat @@ List.map get_some_pattern_vars ps
  | _ -> []

let are_any_of m l =
  List.exists (fun v -> List.mem ~compare:compare_vars v l) m

let rec capturing_match_case : expression_variable list -> (expression, type_expression) match_case -> (unit, _) result = fun vars {pattern;body} ->
  let vars_in_pattern = get_some_pattern_vars pattern in
  capturing_expr (vars_in_pattern @ vars) body
and capturing_expr : expression_variable list -> expression -> (unit, _) result = fun vars expr ->
  match expr.expression_content with
  | E_literal _ | E_variable _ -> ok @@ ()
  | E_constant {cons_name=C_FOLD_WHILE} | E_constant {cons_name=C_ITER}
  | E_constant {cons_name=C_FOLD} | E_constant {cons_name=C_FOLD_LEFT}
  | E_constant {cons_name=C_FOLD_RIGHT} | E_constant {cons_name=C_SET_ITER}
  | E_constant {cons_name=C_SET_FOLD} | E_constant {cons_name=C_SET_FOLD_DESC}
  | E_constant {cons_name=C_LIST_FOLD} | E_constant {cons_name=C_LIST_FOLD_LEFT}
  | E_constant {cons_name=C_LIST_FOLD_RIGHT} | E_constant {cons_name=C_LIST_ITER}
  | E_constant {cons_name=C_MAP_FOLD} | E_constant {cons_name=C_MAP_ITER}
    ->
     ok @@ ()
  | E_constant {arguments} ->
     let%bind _ = bind_map_list (capturing_expr vars) arguments in
     ok @@ ()
  | E_application {lamb;args} ->
     let%bind _ = capturing_expr vars lamb in
     capturing_expr vars args
  | E_lambda _ when are_any_of (get_fv expr) vars ->
     fail @@ capturing vars
  | E_lambda {binder={var;attributes;_};result} ->
     let vars = if is_not_capturable attributes then var :: vars else vars in
     capturing_expr vars result
  | E_recursive {lambda={result;_};_} ->
     capturing_expr [] result
  | E_let_in {let_binder={var;attributes};rhs;let_result} ->
     let%bind _ = capturing_expr vars rhs in
     let vars = if is_not_capturable attributes then var :: vars else vars in
     capturing_expr vars let_result
  | E_type_in {let_result;_} ->
     capturing_expr vars let_result
  | E_mod_in {let_result;_} ->
     capturing_expr vars let_result
  | E_mod_alias {result;_} ->
     capturing_expr vars result
  | E_raw_code _ ->
     ok @@ ()
  | E_constructor {element;_} ->
     capturing_expr vars element
  | E_matching {matchee;cases} ->
     let%bind _ = capturing_expr vars matchee in
     let%bind _ = bind_map_list (capturing_match_case vars) cases in
     ok @@ ()
  | E_record rows ->
     let%bind _ = Helpers.bind_map_lmap (fun expr -> capturing_expr vars expr) rows in
     ok @@ ()
  | E_record_accessor _ -> ok @@ ()
  | E_record_update _ -> ok @@ ()
  | E_ascription {anno_expr} ->
     capturing_expr vars anno_expr
  | E_module_accessor _ -> ok @@ ()

let rec capturing_map_module : module' -> (module', _) result = fun m' ->
  let _self = capturing_map_module in
  let aux = fun (l : expression_variable list) (x : declaration Location.wrap) ->
    match x.wrap_content with
    | Declaration_constant {expr;binder={var;attributes;_};_} ->
       if List.length l > 0 then
         fail @@ capturing l
       else
         begin
           let%bind _ = capturing_expr l expr in
           if is_not_capturable attributes then
             ok @@ var :: l
           else 
             ok @@ l
         end
    | _ -> ok @@ l
  in
  let%bind _ = bind_fold_list (fun a d -> aux a d) [] m' in
  ok @@ m'

