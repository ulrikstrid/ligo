[@@@warning "-33"]

module Region = Simple_utils.Region
module CST    = Cst.Cameligo

open Region
open Errors
open Trace
open CST

let rec type_expr t = 
  let* t = promote_to_type t in
  ok @@ c_type_expr t

and pattern p = 
  let* p = promote_pattern p in
  ok @@ c_pattern p

and expr e = 
  let* e = promote_to_expression e in
  ok @@ c_expr e 

and reg r = 
  ok @@ c_reg r

and nsepseq_last: type a. (a, 'd) Utils.nsepseq -> (a -> (a CST.update_region, _) result) -> ((a, 'd) Utils.nsepseq update_region, _) Trace.result = 
  fun n func ->
    let n_rev = Utils.nsepseq_rev n in
    let last = fst n_rev in
    let* klast = func last in
    ok @@ {
      kregion = klast.kregion;
      kupdate = fun r -> (
        let last = klast.kupdate r in 
        Utils.nsepseq_rev (last, snd n_rev);
      )
    }

and promote_variant: variant reg -> (variant reg, 'err) result = fun v -> 
  let value = v.value in
  match value with 
    {attributes=hd::tl; arg=Some (kwd_of, arg); _} ->
      let region, fst_attr, arg = cover_m (c_reg hd) (c_type_expr arg) in
      let value = {value with arg = Some (kwd_of, arg); attributes = fst_attr :: tl} in 
      ok {region; value}
  | {attributes=hd::tl; constr; _} ->
      let region, fst_attr, constr = cover_m (c_reg hd) (c_reg constr) in
      let value = {value with constr; attributes = fst_attr :: tl} in 
      ok {region; value}
  | {constr; arg=Some(kwd_of, arg)} -> 
      let region, constr, arg = cover_m (c_reg constr) (c_type_expr arg) in
      let value = {value with constr; arg = Some(kwd_of, arg)} in
      ok {region; value}
  | {constr; arg=None } ->
      let c_region = constr.region in
      let region = Region.set_markup constr.region [] in
      let constr = {constr with region} in
      let value = {value with constr} in
      ok @@ {value; region = c_region}

and promote_field_decl: field_decl reg -> (field_decl reg, 'err) result = fun d ->
  let value = d.value in
  match value.attributes with 
    hd :: tl -> 
      let region, attr_hd, field_type = cover_m (c_reg hd) (c_type_expr value.field_type) in
      let value = {value with attributes = attr_hd :: tl; field_type} in
      ok @@ {region; value}
  | [] -> 
      let region, field_name, field_type = cover_m (c_reg value.field_name) (c_type_expr value.field_type) in
      let value = {value with field_name; field_type} in
      ok @@ {region; value}

and promote_field_assign: field_assign reg -> (field_assign reg, 'err) result = fun {value; _} ->
  let region, field_name, field_expr = cover_m (c_reg value.field_name) (c_expr value.field_expr) in
  let value = {value with field_name; field_expr} in
  ok {region; value}

and promote_to_type: type_expr -> (type_expr,'err) result = fun t ->
  match t with 
    TProd {value; _} ->
      let item, _ = value in
      let region, item, col = cover_m (c_type_expr item) (c_nsepseq_last value c_type_expr) in
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
  | TRecord {value; region} ->   
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
      | _, _ -> ok @@ TRecord {value; region}
      );
  | TApp {value; _} ->
      let constr, arg = value in
      let inside = arg.value.inside in
      let region, constr, arg_inside = cover_m (c_reg constr) (c_nsepseq_last inside c_type_expr) in
      let arg = {arg with value = {arg.value with inside = arg_inside}} in
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


and promote_pattern: pattern -> (pattern, 'err) result = fun p ->
  match p with
    PConstr (PConstrApp {value = constr, Some arg; _}) -> 
      let region, constr, arg = cover_m (c_reg constr) (c_pattern arg) in
      let value = constr, Some arg in
      ok @@ PConstr (PConstrApp {region;value})
  | PConstr (PSomeApp {value = constr, arg; _}) ->
      let region, constr, arg = cover_m (c_token constr) (c_pattern arg) in
      let value = constr, arg in
      ok @@ PConstr (PSomeApp {region;value})
  | PList (PCons {value = hd, sep, tl; _}) ->
      let region, hd, tl = cover_m (c_pattern hd) (c_pattern tl) in
      let value = hd, sep, tl in
      ok @@ PList (PCons {value; region})
  | PUnit {value; _} ->
      let region, lpar, rpar = cover_tokens (fst value) (snd value) in
      let value = lpar, rpar in 
      ok @@ PUnit {value; region}
  | PList (PListComp {value; region}) ->
      (match value.compound with 
        Some (Brackets(lbracket, rbracket)) ->
          let region, lbracket, rbracket = cover_tokens lbracket rbracket in
          let compound = Some (Brackets (lbracket, rbracket)) in
          let value = {value with compound} in 
          ok @@ PList (PListComp {value; region})
      | _ -> ok @@ PList (PListComp {value; region}) (* TODO: drop this case by modifying the CST *)
      )
  | PTuple {value; _} ->
      let region, value = cover_nsepseq value c_pattern in
      ok @@ PTuple {region; value}
  | PPar {value; _} ->
      let region, lpar, rpar = cover_tokens value.lpar value.rpar in
      let value = {value with lpar; rpar} in
      ok @@ PPar {value; region}
  | PRecord {value; region} -> (
      match value.compound with 
        Some (Braces (lbrace, rbrace)) ->
          let region, lbrace, rbrace = cover_tokens lbrace rbrace in 
          let compound = Some (Braces (lbrace, rbrace)) in
          let value = {value with compound} in 
          ok @@ PRecord {value; region}
      | _ -> ok @@ PRecord {value; region})
  | PTyped {value; _} -> 
    let region, pattern, type_expr = cover_m (c_pattern value.pattern) (c_type_expr value.type_expr) in
    let value = {value with pattern; type_expr} in
    ok @@ PTyped {value; region}
  | _ as p -> ok p

and promote_case_clause: ('a ->  ('a CST.update_region, _) result) -> 'a case_clause reg ->  ('a case_clause reg, _) result = fun func c -> 
  let value = c.value in
  let* rhs = func value.rhs in
  let region, pattern, rhs = cover_m (c_pattern value.pattern) rhs in
  let value = {value with pattern; rhs} in
  ok @@ {value; region}

and bin_op value =    
  let region, arg1, arg2 = cover_m (c_expr value.arg1) (c_expr value.arg2) in
  let value = {value with arg1; arg2} in
  ok {value; region}

and promote_to_expression: expr -> (expr,'err) result = 
  fun e -> 
    match e with 
      ECase {value; _} ->
        let cases = value.cases in
        let region, kwd_match, c = cover_m (c_token value.kwd_match) (c_nsepseq_last cases.value c_reg) in
        let cases = {cases with value = c} in
        let value = {value with cases; kwd_match} in
        ok @@ ECase {value; region}
    | ECond {value; _} -> 
        (match value.ifnot with 
          | Some (kwd_else, else_body) -> 
            let region, kwd_if, else_body = cover_m (c_token value.kwd_if) (c_expr else_body) in
            let else_ = Some(kwd_else, else_body) in
            let value = {value with kwd_if; ifnot=else_} in 
            ok @@ ECond {value; region}
          | None ->
            let region, kwd_if, ifso = cover_m (c_token value.kwd_if) (c_expr value.ifso) in
            let value = {value with kwd_if; ifso} in
            ok @@ ECond {value; region})
    | EAnnot {value; _} ->
        let region, lpar, rpar = cover_m (c_token value.lpar) (c_token value.rpar) in
        let value = {value with lpar; rpar} in 
        ok @@ EAnnot {value; region}
    | ELogic (BoolExpr (Or {value; _})) ->
        let* v = bin_op value in
        ok @@ ELogic (BoolExpr (Or v))
    | ELogic (BoolExpr (And {value; _})) ->
        let* v = bin_op value in
        ok @@ ELogic (BoolExpr (And v))
    | ELogic (CompExpr (Lt {value; _})) ->
        let* v = bin_op value in
        ok @@ ELogic (CompExpr (Lt v))
    | ELogic (CompExpr (Leq {value; _})) ->
        let* v = bin_op value in
        ok @@ ELogic (CompExpr (Leq v))
    | ELogic (CompExpr (Gt {value; _})) ->
        let* v = bin_op value in
        ok @@ ELogic (CompExpr (Gt v))
    | ELogic (CompExpr (Geq {value; _})) ->
        let* v = bin_op value in
        ok @@ ELogic (CompExpr (Geq v))
    | ELogic (CompExpr (Equal {value; _})) ->
        let* v = bin_op value in
        ok @@ ELogic (CompExpr (Equal v))
    | ELogic (CompExpr (Neq {value; _})) ->
        let* v = bin_op value in
        ok @@ ELogic (CompExpr (Neq v))
    | EArith (Add {value; _}) ->
        let* v = bin_op value in
        ok @@ EArith (Add v)
    | EArith (Sub {value; _}) ->
        let* v = bin_op value in
        ok @@ EArith (Sub v)
    | EArith (Mult {value; _}) ->
        let* v = bin_op value in
        ok @@ EArith (Mult v)
    | EArith (Div {value; _}) ->
        let* v = bin_op value in
        ok @@ EArith (Div v)
    | EArith (Mod {value; _}) ->
        let* v = bin_op value in
        ok @@ EArith (Mod v)
    | EString (Cat {value; _}) ->
        let* v = bin_op value in 
        ok @@ EString (Cat v)
    | EList (ECons {value; _}) ->
        let* v = bin_op value in 
        ok @@ EList (ECons v)
    | EConstr (ESomeApp {value; _}) ->
        let constr, arg = value in
        let region, c_some, e = cover_m (c_token constr) (c_expr arg) in
        let value = c_some, e in
        ok @@ EConstr (ESomeApp {value; region})
    | EConstr (EConstrApp {value; _}) ->
        let constr, arg = value in
        (match arg with 
          Some e -> 
            let region, constr, arg = cover_m (c_reg constr) (c_expr e) in
            ok @@ EConstr (EConstrApp {value = constr, Some arg; region})  
        | None -> 
            let region = constr.region in
            let constr_region = Region.set_markup constr.region [] in
            let constr = {constr with region=constr_region} in
            ok @@ EConstr (EConstrApp {value = constr, arg; region}))
    | EFun {value; _} -> 
        let region, kwd_fun, body = cover_m (c_token value.kwd_fun) (c_expr value.body) in
        let value = {value with kwd_fun; body} in 
        ok @@ EFun {value; region}
    | ERecord {value; region} ->     
        let attributes = value.attributes in
        let compound = value.compound in
        (match attributes, compound with 
          hd :: tl, Some (Braces (lbrace, rbrace)) ->
            let region, hd, rbrace = cover_m (c_reg hd) (c_token rbrace) in
            let value = {value with attributes = hd :: tl; compound = Some (Braces(lbrace, rbrace))} in
            ok @@ ERecord {value; region}
        | [], Some (Braces (lbrace, rbrace)) -> 
          let region, lbrace, rbrace = cover_m (c_token lbrace) (c_token rbrace) in
          let value = {value with compound = Some (Braces(lbrace, rbrace))} in
          ok @@ ERecord {value; region}
        | _, _ -> ok @@ ERecord {value; region}
        );  
    | EProj {value; _} ->
      let region, struct_name, field_path = cover_m (c_reg value.struct_name) (c_nsepseq_last value.field_path c_selection) in 
      let value  = {value with struct_name; field_path}
      in ok @@ EProj {region; value}
    | EModA {value; _} ->
      let region, module_name, field = cover_m (c_reg value.module_name) (c_expr value.field) in
      let value = {value with module_name; field} in
      ok @@ EModA {value; region}
    | EUpdate {value; _} ->
      let region, lbrace, rbrace = cover_tokens value.lbrace value.rbrace in
      let value = {value with lbrace; rbrace} in
      ok @@ EUpdate {value; region}
    | ECall {value; _} ->
      let region, func, arg = cover_m (c_expr (fst value)) (c_nseq_last (snd value) c_expr) in
      let value = func, arg in
      (* print_endline ("ECall: " ^ string_of_int (List.length region#markup)); *)
      ok @@ ECall {value; region}
    | EUnit {value; _} ->
      let region, lpar, rpar = cover_m (c_token (fst value)) (c_token (snd value)) in
      let value = lpar, rpar in
      ok @@ EUnit {value; region}
    | ETuple {value; _} -> 
      let region, value = cover_nsepseq value c_expr in
      ok @@ ETuple {value; region}
    | EPar {value; _} ->
      let region, lpar, rpar = cover_m (c_token value.lpar) (c_token value.rpar) in
      let value = {value with lpar; rpar} in
      ok @@ EPar {value; region}
    | ELetIn {value; _} ->
      (match value.attributes with 
        hd :: tl ->
          let region, fst_attr, body = cover_m (c_reg hd) (c_expr value.body) in
          let value = {value with attributes = fst_attr :: tl; body} in          
          ok @@ ELetIn {value; region}
      | [] -> 
        let region, kwd_let, body = cover_m (c_token value.kwd_let) (c_expr value.body) in
        let value = {value with kwd_let; body} in
        ok @@ ELetIn {value; region}
      )
    | ETypeIn {value; _} ->
      let type_decl = value.type_decl in
      let region, kwd_type, body = cover_m (c_token type_decl.kwd_type) (c_expr value.body) in
      let type_decl = {type_decl with kwd_type} in
      let value = {value with type_decl; body} in
      ok @@ ETypeIn {value; region}
    | EModIn {value; _} ->
      let mod_decl = value.mod_decl in
      let region, kwd_module, body = cover_m (c_token mod_decl.kwd_module) (c_expr value.body) in
      let mod_decl = {mod_decl with kwd_module} in
      let value = {value with mod_decl; body} in 
      ok @@ EModIn {value; region}
    | EModAlias {value; _} ->
      let mod_alias = value.mod_alias in
      let region, kwd_module, body = cover_m (c_token mod_alias.kwd_module) (c_expr value.body) in
      let mod_alias = {mod_alias with kwd_module} in
      let value = {value with mod_alias; body} in
      ok @@ EModAlias {value; region}
    | ESeq {value; region}  -> (
      match value.compound with 
        Some (BeginEnd (kwd_begin, kwd_end)) -> 
          let region, kwd_begin, kwd_end = cover_tokens kwd_begin kwd_end in
          let compound = Some (BeginEnd (kwd_begin, kwd_end)) in
          let value = {value with compound} in
          ok @@ ESeq {value; region}
      | _ -> ok @@ ESeq {value; region}
    )
    | ECodeInj {value; _} ->
      let region, language, rbracket = cover_m (c_reg value.language) (c_token value.rbracket) in
      let value = {value with language; rbracket} in 
      ok @@ ECodeInj {value; region}
    | _ as e -> ok e

and promote_to_declaration: declaration -> (declaration,'err) result = fun d ->
  match d with 
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

let map_comments: ('err) Helpers.mapper = {
    e = promote_to_expression;
    t = promote_to_type;
    d = promote_to_declaration;
}
