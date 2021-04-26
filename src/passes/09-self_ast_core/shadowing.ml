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
  | E_lambda {binder={var;_};_} when in_vars var vars ->
     fail @@ shadowing var
  | E_lambda {binder={var;attributes;_};result} when is_not_shadowable attributes ->
     shadowing_expr (var :: vars) result
  | E_lambda {result;_} ->
     shadowing_expr vars result
  | E_let_in {let_binder={var;_};_} when in_vars var vars ->
     fail @@ shadowing var
  | E_let_in {let_binder={var;attributes;_};rhs;let_result} when is_not_shadowable attributes ->
     let%bind _ = shadowing_expr vars rhs in
     shadowing_expr (var :: vars) let_result
  | E_let_in {rhs;let_result;_} ->
     let%bind _ = shadowing_expr vars rhs in
     shadowing_expr vars let_result
  | E_application {lamb;args} ->
     let%bind _ = shadowing_expr vars lamb in
     shadowing_expr vars args
  | E_type_in {let_result;_} ->
     shadowing_expr vars let_result
  | E_mod_in {let_result;_} ->
     shadowing_expr vars let_result
  | E_constructor {element;_} ->
     shadowing_expr vars element
  | E_matching {matchee;cases} ->
     let%bind _ = shadowing_expr vars matchee in
     let%bind _ = bind_map_list (shadowing_match_case vars) cases in
     ok @@ ()
  | E_record _rows ->
     ok @@ ()
  | E_literal _ | E_constant _ | E_variable _ -> ok @@ ()
  | _ -> ok @@ ()

let rec shadowing_map_module : module' -> (module', _) result = fun m' ->
  let _self = shadowing_map_module in
  let aux = fun (x : declaration) ->
    match x with
    | Declaration_constant {expr;_} ->
       shadowing_expr [] expr
    | _ -> ok @@ ()
  in
  let%bind _ = bind_map_list (bind_map_location aux) m' in
  ok @@ m'

  (* let update_annotations annots c =
   *   List.fold_right (fun a r -> update_annotation a r) annots c in
   * let aux = fun (x : declaration) ->
   *   match x with
   *   | Declaration_constant {expr ; _} -> (
   *     let defuse,_ = defuse_neutral in
   *     let _,unused = defuse_of_expr defuse expr in
   *     let warn_var v =
   *       `Self_ast_typed_warning_unused
   *         (Location.get_location v, Format.asprintf "%a" Var.pp (Location.unwrap v)) in
   *     update_annotations (List.map warn_var unused) @@
   *       ok @@ ()
   *   )
   *   | Declaration_type _ -> ok @@ ()
   *   | Declaration_module {module_} ->
   *     let%bind _ = self module_ in
   *     ok @@ ()
   *   | Module_alias _ -> ok @@ ()
   * in
   * let%bind _ = bind_map_list (bind_map_location aux) p in
   * ok @@ (Module_Fully_Typed p) *)
