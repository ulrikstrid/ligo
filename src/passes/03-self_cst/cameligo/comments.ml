[@@@warning "-33"]

module Region = Simple_utils.Region
module CST    = Cst.Cameligo

open Region
open Trace
open CST
(* 
let promote_variant: variant -> (variant, 'err) result = fun v ->

let promote_field_decl: field_decl -> (field_decl, 'err) result = fun f ->
   *)


let promote_to_type: type_expr -> (type_expr,'err) result = function
  TProd {value; _} ->
    let region, item, col = cover_m (c_type_expr (fst value)) (c_nsepseq_last value c_type_expr) in
    let value  = (item, snd col) in
    ok @@ TProd {region; value} 
| TSum {value; _} ->
  let attributes = value.attributes in
  let lead_vbar = value.lead_vbar in
  let variants = value.variants in
  (match attributes, lead_vbar with 
    hd :: tl, _ -> 
      let region, hd, variants = cover_m (c_reg hd) (c_nsepseq_last variants c_reg) in
      let value = {value with attributes = hd :: tl; variants} in
      ok @@ TSum {region; value}
  | [], Some lead_vbar -> 
      let region, lead_vbar, variants = cover_m (c_token lead_vbar) (c_nsepseq_last variants c_reg) in
      let value = {value with lead_vbar = Some lead_vbar; variants} in
      ok @@ TSum {region; value}
  | [], None -> 
    let region, variants = cover_nsepseq variants c_reg in
    let value = {value with variants} in
    ok @@ TSum {region; value}
  )
| TRecord {value; _} ->
  let attributes = value.attributes in
  let compound = value.compound in
  (match attributes, compound with 
    hd :: tl, Some (Braces (lbrace, rbrace)) ->
      let region, hd, rbrace = cover_m (c_reg hd) (c_token rbrace) in
      let value = {value with attributes = hd :: tl; compound = Some (Braces(lbrace, rbrace))} in
      ok @@ TRecord {value; region}
  | [], Some (Braces (lbrace, rbrace)) -> 
    let region, lbrace, rbrace = cover_m (c_token lbrace) (c_token rbrace) in
    let value = {value with compound = Some (Braces(lbrace, rbrace))} in
    ok @@ TRecord {value; region}
  | _, _ -> failwith "should never happen" (* TODO: drop this case by modifying the CST *)
  );
| TApp {value; _} ->
  let constr, arg = value in
  let region, constr, arg = cover_m (c_reg constr) (c_reg arg) in
  let value = constr, arg in
  ok @@ TApp {region; value}
| TFun {value; _} ->
  let fun_a, arrow, fun_b = value in
  let region, fun_a, fun_b = cover_m (c_type_expr fun_a) (c_type_expr fun_b) in
  ok @@ TFun {region; value = fun_a, arrow, fun_b}
| TPar {value; _} -> 
  let region, lpar, rpar = cover_tokens value.lpar value.rpar in
  let value = {value with lpar; rpar} in
  ok @@ TPar {value; region }
| TModA {value; _} -> 
  let module_name = value.module_name in
  let field = value.field in
  let region, module_name, field = cover_m (c_reg module_name) (c_type_expr field) in
  let value = {value with module_name; field} in
  ok @@ TModA {value; region}
| _ as e -> ok e

(*
| TVar    of variable
| TWild   of wild
| TString of lexeme reg
| TInt    of (lexeme * Z.t) reg
| TModA   of type_expr module_access reg *)
  

let promote_to_expression: expr -> (expr,'err) result = 
  fun e -> ok e

let promote_to_declaration: declaration -> (declaration,'err) result = function
  Let {value = kwd_let, kwd_rec, binding, attributes; _} -> (
    match attributes with  
      hd :: tl  ->
        let region, hd_attr, binding_let_rhs = cover_m (c_reg hd) (c_expr binding.let_rhs) in
        let value = kwd_let, kwd_rec, {binding with let_rhs = binding_let_rhs}, (hd_attr :: tl) in
        ok @@ Let {region; value} 
    | [] ->
        let region, kwd_let, binding_let_rhs = cover_m (c_token kwd_let) (c_expr binding.let_rhs) in
        let value = kwd_let, kwd_rec, {binding with let_rhs = binding_let_rhs}, [] in
        ok @@ Let {region; value})
| TypeDecl {value; _} ->
    let region, kwd_type, type_expr = 
      cover_m (c_token value.kwd_type) (c_type_expr value.type_expr) in
    let value  = { value with kwd_type; type_expr} in 
    ok @@ TypeDecl {region; value}
| ModuleDecl {value; _} -> 
    let region, kwd_module, kwd_end = cover_m (c_token value.kwd_module) (c_token value.kwd_end) in
    let value = {value with kwd_module; kwd_end} in
    ok @@ ModuleDecl {value; region}
| ModuleAlias {value; _} ->
    let region, kwd_module, binders = cover_m (c_token value.kwd_module) (c_nsepseq_last value.binders c_reg) in
    let value  = {value with kwd_module; binders} in 
    ok @@ ModuleAlias {region; value}
| Directive _ as d -> ok d

let promote: 'err Helpers.mapper = {
  t = promote_to_type;
  e = promote_to_expression;
  d = promote_to_declaration;
}