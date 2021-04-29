open Helpers
open Ast_core
open Errors
open Trace

let is_not_capturable attrs = not attrs.capturable
let is_not_looper attrs = not attrs.looper
let in_vars var vars = List.mem ~compare:compare_vars var vars

let are_any_of m l =
  List.exists (fun v -> List.mem ~compare:compare_vars v l) m

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

let rec capturing_match_case : expression_variable list -> (expression, type_expression) match_case -> (unit, _) result = fun vars {pattern;body} ->
  let vars_in_pattern = get_some_pattern_vars pattern in
  capturing_expr (vars_in_pattern @ vars) body
and capturing_expr : expression_variable list -> expression -> (unit, _) result = fun vars expr ->
  match expr.expression_content with
  | E_literal _ | E_variable _ -> ok @@ ()
  | E_constant {arguments} ->
     let%bind _ = bind_map_list (capturing_expr vars) arguments in
     ok @@ ()
  | E_application {lamb;args} ->
     let%bind _ = capturing_expr vars lamb in
     capturing_expr vars args
  | E_lambda {binder={attributes}} when are_any_of (get_fv expr) vars
                                        && is_not_looper attributes ->
     fail @@ capturing vars
  | E_lambda {binder={var;attributes;_};result} ->
     let vars = remove_from var vars in
     let vars = if is_not_capturable attributes then var :: vars else vars in
     capturing_expr vars result
  | E_recursive {lambda={result;_};_} ->
     capturing_expr [] result
  | E_let_in {let_binder={var;attributes};rhs;let_result} ->
     let%bind _ = capturing_expr vars rhs in
     let vars = remove_from var vars in
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
     let%bind _ = Helpers.bind_map_lmap (capturing_expr vars) rows in
     ok @@ ()
  | E_record_accessor _ -> ok @@ ()
  | E_record_update _ -> ok @@ ()
  | E_ascription {anno_expr} ->
     capturing_expr vars anno_expr
  | E_module_accessor _ -> ok @@ ()

let capturing_map_module : module' -> (module', _) result = fun m' ->
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
