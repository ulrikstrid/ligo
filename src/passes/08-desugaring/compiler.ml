module I = Ast_sugar
module O = Ast_core

open Errors
open Trace
open Stage_common.Maps

let cast_var = Location.map Var.todo_cast

let is_michelson_annotation attr =
  if String.length attr > 6 && String.sub attr 0 6 = "annot:" then
    Some (String.sub attr 6 ((String.length attr)-6))
  else None

let is_layout attr =
  if String.length attr > 7 && String.sub attr 0 7 = "layout:" then
    Some (String.sub attr 7 ((String.length attr)-7))
  else None

let is_inline attr = String.equal "inline" attr

let get_michelson_annotation : (string list) -> string option = fun attributes ->
  let rec aux lst = match lst with
    | hd::tl -> ( match is_michelson_annotation hd with
      | Some ann -> Some ann
      | None -> aux tl
    )
    | [] -> None
  in
  aux attributes

let get_layout : (string list) -> O.layout option = fun attributes ->
  let rec aux lst = match lst with
    | hd::tl -> ( match is_layout hd with
      | Some "tree" -> Some O.L_tree
      | Some "comb" -> Some O.L_comb
      (*deal with wrong layout*)
      | None | Some _ -> aux tl
    )
    | [] -> None
  in
  aux attributes

let get_inline : (string list) -> bool = List.exists is_inline


let rec compile_type_expression : I.type_expression -> (O.type_expression , desugaring_error) result =
  fun te ->
  let self = compile_type_expression in
  let return tc = ok @@ O.make_t ~loc:te.location ~sugar:te tc in
  match te.type_content with
    | I.T_variable type_variable -> return @@ T_variable (Var.todo_cast type_variable)
    | I.T_app a ->
      let* a' = type_app compile_type_expression a in
      return @@ T_app a'
    | I.T_sum {fields ; attributes} ->
      let* fields =
        Stage_common.Helpers.bind_map_lmap (fun v ->
          let {associated_type ; attributes ; decl_pos} : _ I.row_element = v in
          let michelson_annotation = get_michelson_annotation attributes in
          let* associated_type = compile_type_expression associated_type in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          ok @@ v'
        ) fields
      in
      let layout = get_layout attributes in
      return @@ O.T_sum {fields ; layout }
    | I.T_record {fields ; attributes} ->
      let* fields =
        Stage_common.Helpers.bind_map_lmap (fun v ->
          let {associated_type ; attributes ; decl_pos} : _ I.row_element = v in
          let* associated_type = compile_type_expression associated_type in
          let michelson_annotation = get_michelson_annotation attributes in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          ok @@ v'
        ) fields
      in
      let layout = get_layout attributes in
      return @@ O.T_record {fields ; layout }
    | I.T_tuple tuple ->
      let aux (i,acc) el =
        let* el = self el in
        ok @@ (i+1,(O.Label (string_of_int i), ({associated_type=el;michelson_annotation=None;decl_pos=0}:_ O.row_element_mini_c))::acc) in
      let* (_, lst ) = bind_fold_list aux (0,[]) tuple in
      let record = O.LMap.of_list lst in
      return @@ O.T_record {fields = record ; layout = None}
    | I.T_arrow arr ->
      let* arr = arrow self arr in
      return @@ T_arrow arr
    | I.T_module_accessor ma ->
      let* ma = module_access self ma in
      return @@ O.T_module_accessor ma
    | I.T_singleton x ->
      return @@ O.T_singleton x

let compile_binder = binder compile_type_expression

let rec compile_expression : I.expression -> (O.expression , desugaring_error) result =
  fun sugar ->
  let self = compile_expression in
  let self_type = compile_type_expression in
  let return expr = ok @@ O.make_e ~loc:sugar.location ~sugar expr in
  match sugar.expression_content with
    | I.E_literal literal -> return @@ O.E_literal literal
    | I.E_constant cons ->
      let* cons = constant self cons in
      return @@ O.E_constant cons
    | I.E_variable name -> return @@ O.E_variable (cast_var name)
    | I.E_application app ->
      let* app = application self app in
      return @@ O.E_application app
    | I.E_lambda lamb ->
      let* lamb = lambda self self_type lamb in
      return @@ O.E_lambda lamb
    | I.E_recursive recs ->
      let* recs = recursive self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;attributes;rhs;let_result} ->
      let* let_binder = binder self_type let_binder in
      let* rhs = self rhs in
      let* let_result = self let_result in
      let inline = get_inline attributes in
      return @@ O.E_let_in {let_binder;inline;rhs;let_result}
    | I.E_type_in ti ->
      let* ti = type_in self self_type ti in
      return @@ O.E_type_in ti
    | I.E_mod_in {module_binder;rhs;let_result} ->
      let* rhs = compile_module rhs in
      let* let_result = self let_result in
      return @@ O.E_mod_in {module_binder;rhs;let_result}
    | I.E_mod_alias ma ->
      let* ma = mod_alias self ma in
      return @@ O.E_mod_alias ma
    | I.E_raw_code rc ->
      let* rc = raw_code self rc in
      return @@ O.E_raw_code rc
    | I.E_constructor const ->
      let* const = constructor self const in
      return @@ O.E_constructor const
    | I.E_matching {matchee; cases} ->
      let* matchee = compile_expression matchee in
      let* cases =
        bind_map_list
          (fun ({pattern ; body} : (I.expression, I.type_expression) I.match_case) -> 
            let* pattern = Stage_common.Helpers.map_pattern_t (binder compile_type_expression) pattern in
            let* body = compile_expression body in
            ok ({pattern ; body} : (O.expression, O.type_expression) I.match_case)
          )
          cases
      in
      return @@ O.E_matching {matchee ; cases}
    | I.E_record recd ->
      let* recd = record self recd in
      return @@ O.E_record recd
    | I.E_accessor {record;path} ->
      let* record = self record in
      let accessor ~loc expr a =
        match a with
          I.Access_tuple  i -> ok @@ O.e_record_accessor ~loc expr (Label (Z.to_string i))
        | I.Access_record a -> ok @@ O.e_record_accessor ~loc expr (Label a)
        | I.Access_map k ->
          let* k = self k in
          ok @@ O.e_constant ~loc C_MAP_FIND_OPT [k;expr]
      in
      bind_fold_list (accessor ~loc:sugar.location) record path
    | I.E_update {record;path;update} ->
      let* record = self record in
      let* update = self update in
      let accessor ~loc expr a =
        match a with
          I.Access_tuple  i -> ok @@ O.e_record_accessor ~loc expr (Label (Z.to_string i))
        | I.Access_record a -> ok @@ O.e_record_accessor ~loc expr (Label a)
        | I.Access_map k ->
          let* k = self k in
          ok @@ O.e_constant ~loc C_MAP_FIND_OPT [k;expr]
      in
      let updator ~loc (s:O.expression) a expr =
        match a with
          I.Access_tuple  i -> ok @@ O.e_record_update ~loc s (Label (Z.to_string i)) expr
        | I.Access_record a -> ok @@ O.e_record_update ~loc s (Label a) expr
        | I.Access_map k ->
          let* k = self k in
          ok @@ O.e_constant ~loc C_MAP_ADD [k;expr;s]
      in
      let aux (s, e : O.expression * _) lst =
        let* s' = accessor ~loc:s.location s lst in
        let e' = fun expr ->
          let* u = updator ~loc:s.location s lst (expr)
          in e u
        in
        ok @@ (s',e')
      in
      let* (_,rhs) = bind_fold_list aux (record, fun e -> ok @@ e) path in
      rhs @@ update
    | I.E_map map -> (
      let map = List.sort_uniq compare map in
      let aux = fun prev (k, v) ->
        let* (k', v') = bind_map_pair (self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let* init = return @@ E_constant {cons_name=C_MAP_EMPTY;arguments=[]} in
      bind_fold_right_list aux init map
    )
    | I.E_big_map big_map -> (
      let big_map = List.sort_uniq compare big_map in
      let aux = fun prev (k, v) ->
        let* (k', v') = bind_map_pair (self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let* init = return @@ E_constant {cons_name=C_BIG_MAP_EMPTY;arguments=[]} in
      bind_fold_right_list aux init big_map
    )
    | I.E_list lst ->
      let* lst' = bind_map_list (self) lst in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
      let* init  = return @@ E_constant {cons_name=C_LIST_EMPTY;arguments=[]} in
      bind_fold_right_list aux init lst'
    | I.E_set set -> (
      let* lst' = bind_map_list (self) set in
      let lst' = List.sort_uniq compare lst' in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
      let* init = return @@ E_constant {cons_name=C_SET_EMPTY;arguments=[]} in
      bind_fold_list aux init lst'
      )
    | I.E_ascription {anno_expr; type_annotation} ->
      let* anno_expr = self anno_expr in
      let* type_annotation = self_type type_annotation in
      return @@ O.E_ascription {anno_expr; type_annotation}
    | I.E_module_accessor ma ->
      let* ma = module_access self ma in
      return @@ O.E_module_accessor ma
    | I.E_cond {condition; then_clause; else_clause} ->
      let* matchee = self condition in
      let* match_true = self then_clause in
      let* match_false = self else_clause in
      return @@ O.E_matching {
          matchee ;
          cases = [
            { pattern = Location.wrap @@ O.P_variant (Label "true" , Location.wrap O.P_unit) ; body = match_true  } ;
            { pattern = Location.wrap @@ O.P_variant (Label "false", Location.wrap O.P_unit) ; body = match_false } ;
          ]
        }
    | I.E_sequence {expr1; expr2} ->
      let* expr1 = self expr1 in
      let* expr2 = self expr2 in
      let let_binder : _ O.binder = {var = Location.wrap @@ Var.of_name "_" ; ascr = Some (O.t_unit ())} in
      return @@ O.E_let_in {let_binder; rhs=expr1;let_result=expr2; inline=false}
    | I.E_skip -> ok @@ O.e_unit ~loc:sugar.location ~sugar ()
    | I.E_tuple t ->
      let aux (i,acc) el =
        let* el = self el in
        ok @@ (i+1,(O.Label (string_of_int i), el)::acc) in
      let* (_, lst ) = bind_fold_list aux (0,[]) t in
      let m = O.LMap.of_list lst in
      return @@ O.E_record m

and compile_declaration : I.declaration -> (O.declaration , desugaring_error) result =
  fun declaration ->
  let return (decl: O.declaration) = ok @@ decl in
  match declaration with
  | I.Declaration_type dt ->
    let* dt = declaration_type compile_type_expression dt in
    return @@ O.Declaration_type dt
  | I.Declaration_constant {name; binder;attr;expr} ->
    let* binder = compile_binder binder in
    let* expr = compile_expression expr in
    let inline = get_inline attr in
    return @@ O.Declaration_constant {name; binder; attr={inline}; expr}
  | I.Declaration_module {module_binder;module_} ->
    let* module_ = compile_module module_ in
    return @@ O.Declaration_module {module_binder;module_}
  | I.Module_alias ma ->
    let* ma = module_alias ma in
    return @@ O.Module_alias ma


and compile_module : I.module_ -> (O.module_ , desugaring_error) result = fun m ->
  bind_map_list (bind_map_location compile_declaration) m
