[@@@warning "-27"]
[@@@warning "-26"]
[@@@warning "-39"]

module AST = Ast_imperative
module CST = Cst.Jsligo
module Predefined = Predefined.Tree_abstraction.Jsligo

open Trace
open Function

(* Utils *)

let ghost = Region.ghost

let wrap = Region.wrap_ghost

let decompile_attributes = List.map wrap

let list_to_sepseq lst =
  match lst with
    [] -> None
  |  hd :: lst ->
      let aux e = (ghost, e) in
      Some (hd, List.map aux lst)

let list_to_nsepseq lst =
  match list_to_sepseq lst with
    Some s -> ok @@ s
  | None   -> failwith "List is empty"

let nelist_to_npseq (hd, lst) = (hd, List.map (fun e -> (ghost, e)) lst)
                                
let npseq_cons hd lst = hd,(ghost, fst lst)::(snd lst)

let par a = CST.{lpar=ghost;inside=a;rpar=ghost}

let inject compound a = CST.{compound;elements=a;terminator=None}

let ne_inject compound fields ~attr = CST.{
  compound;
  ne_elements=fields;
  terminator=None;
  attributes=attr
  }

let prefix_colon a = (ghost, a)

let braces = Some (CST.Braces (ghost,ghost))

(* let brackets = Some (CST.Brackets (ghost,ghost)) *)

let chevrons x = CST.{lchevron=ghost;inside=x;rchevron=ghost}
let brackets x = CST.{lbracket=ghost;inside=x;rbracket=ghost}
let fun_type_arg x = CST.{ name = wrap "_" ; colon = ghost ; type_expr = x }
let braced d = CST.{lbrace=ghost; rbrace=ghost; inside=d}

(* Decompiler *)

let decompile_variable : type a. a Var.t -> CST.variable = fun var ->
  let var = Format.asprintf "%a" Var.pp var in
  if String.contains var '#' then
    let var = String.split_on_char '#' var in
    wrap @@ "gen__" ^ (String.concat "" var)
  else
    if String.length var > 4 && String.equal "gen__" @@ String.sub var 0 5 then
      wrap @@ "user__" ^ var
    else
      wrap @@ var

let rec decompile_type_expr : AST.type_expression -> _ result = fun te ->
  let return te = ok @@ te in
  match te.type_content with
    T_sum { attributes ; fields } ->
    let lst = AST.LMap.to_kv_list fields in
    let aux (AST.Label c, AST.{associated_type;attributes}) =
      let constr = wrap c in
      let%bind arg = decompile_type_expr associated_type in
      let arg : CST.type_tuple = wrap @@ chevrons @@ nelist_to_npseq (arg , []) in
      let _attributes = decompile_attributes attributes in
      let variant : CST.type_expr = TApp (wrap (
          constr ,
          arg
        ))
      in
      ok variant
    in
    let%bind variants = bind_map_list aux lst in
    let%bind variants = list_to_nsepseq variants in
    let lead_vbar = Some ghost in
    let attributes = decompile_attributes attributes in
    let sum : CST.sum_type = { lead_vbar ; variants ; attributes} in
    return @@ CST.TSum (wrap sum)
  | T_record {fields; attributes} ->
     let record = AST.LMap.to_kv_list fields in
     let aux (AST.Label c, AST.{associated_type; attributes; _}) =
       let field_name = wrap c in
       let colon = ghost in
       let%bind field_type = decompile_type_expr associated_type in
       let attributes = decompile_attributes attributes in
       let field : CST.field_decl =
         {field_name; colon; field_type; attributes} in
       ok @@ wrap field in
     let%bind record = bind_map_list aux record in
     let%bind record = list_to_nsepseq record in
     let attributes = List.map (fun el -> wrap el) attributes in
     return @@ CST.TObject (wrap @@ ne_inject braces record ~attr:attributes)
  | T_tuple tuple ->
    let%bind tuple = bind_map_list decompile_type_expr tuple in
    let%bind tuple = list_to_nsepseq tuple in
    let tuple = brackets tuple in
    return @@ CST.TProd ({inside = wrap tuple; attributes = []})
  | T_arrow {type1;type2} ->
    let%bind type1 = decompile_type_expr type1 in
    let type_arg = fun_type_arg type1 in
    let type_args = par @@ nelist_to_npseq (type_arg , []) in
    let%bind type2 = decompile_type_expr type2 in
    let arrow = (type_args, ghost, type2) in
    return @@ CST.TFun (wrap arrow)
  | T_variable variable ->
    let var = decompile_variable variable in
    return @@ CST.TVar var
  | T_app {type_operator; arguments} ->
    let type_operator = wrap @@ Var.to_name type_operator in
    let%bind lst = bind_map_list decompile_type_expr arguments in
    let%bind lst = list_to_nsepseq lst in
    let lst : CST.type_tuple = wrap @@ chevrons lst in
    return @@ CST.TApp (wrap (type_operator,lst))
  | T_annoted _annot ->
    failwith "let's work on it later"
  | T_module_accessor {module_name;element} ->
    let module_name = wrap module_name in
    let%bind field  = decompile_type_expr element in
    return @@ CST.TModA (wrap CST.{module_name;selector=ghost;field})
  | T_singleton x ->
      match x with
      | Literal_int i ->
        let z : CST.type_expr = CST.TInt { region = Region.ghost ; value = (Z.to_string i, i) } in
        return z
      | _ -> failwith "unsupported singleton"

let get_e_variable : AST.expression -> _ result = fun expr ->
  match expr.expression_content with
    E_variable var -> ok @@ var.wrap_content
  | _ -> failwith @@
    Format.asprintf "%a should be a variable expression"
    AST.PP.expression expr

let get_e_tuple : AST.expression -> _ result = fun expr ->
  match expr.expression_content with
    E_tuple tuple -> ok @@ tuple
  | E_variable _
  | E_literal _
  | E_constant _
  | E_lambda _ -> ok @@ [expr]
  | _ -> failwith @@
    Format.asprintf "%a should be a tuple expression"
    AST.PP.expression expr

type statement_or_expr =
  | Statement of CST.statement
  | Expr of CST.expr

let e_hd = function 
  [Expr hd] -> hd
| _ -> failwith "not supported"

let rec s_hd = function 
  [Statement hd] -> ok @@ hd
| [Expr e] -> ok @@ CST.SExpr e
| lst -> 
  let%bind lst = bind_map_list (fun e -> s_hd [e]) lst in
  let%bind lst = list_to_nsepseq lst in
  ok @@ CST.SBlock (wrap @@ braced @@ lst)

let decompile_expression : AST.expression -> (CST.expr list, _) result = fun expr ->
  failwith "todo: fix me"

let rec decompile_expression_in : AST.expression -> (statement_or_expr list, _) result = fun expr ->
  let return_expr expr = ok @@ expr in
  let return_expr_with_par expr = return_expr @@ [CST.EPar (wrap @@ par @@ expr)] in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name.wrap_content in
    return_expr @@ [Expr (CST.EVar (var))]
  | E_constant {cons_name; arguments} ->
    let expr = CST.EVar (wrap @@ Predefined.constant_to_string cons_name) in
    (match arguments with
      [] -> return_expr @@ [Expr expr]
    | _ ->
      let%bind arguments =
        map (fun xs -> CST.Multiple (wrap (par xs))) @@
        map (fun (hd,tl) -> hd,List.map (fun x -> ghost,x) tl) @@
        map List.Ne.of_list @@
        map (List.map (fun x -> CST.EPar (wrap @@ par @@ x))) @@
        bind_map_list (fun e -> 
          let%bind e = decompile_expression_in e in 
          match e with 
            Expr hd :: [] -> ok @@ hd
          | _ -> failwith "should not happen"
          ) arguments in
      let const = wrap (expr, arguments) in
      return_expr @@ [Expr (CST.ECall const)]
    )
    | E_literal literal ->
      (match literal with
          Literal_unit  ->  return_expr @@ [Expr (CST.EUnit (wrap (ghost,ghost)))]
        | Literal_int i ->  return_expr @@ [Expr (CST.EArith (Int (wrap ("",i))))]
        | Literal_nat n ->  return_expr @@ [Expr (CST.EAnnot {value = CST.EArith (Int (wrap ("",n))), ghost, CST.TVar {value = "nat"; region = ghost}; region = ghost })]
        | Literal_timestamp time ->
          let time = Tezos_utils.Time.Protocol.to_notation @@
            Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time in
            (* TODO combinators for CSTs. *)
          let%bind ty = decompile_type_expr @@ AST.t_timestamp () in
          let time = CST.EString (String (wrap time)) in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (time, ghost, ty)))]
        | Literal_mutez mtez -> return_expr @@ [Expr (CST.EAnnot {value = CST.EArith (Int (wrap ("", mtez))), ghost, CST.TVar {value = "mutez"; region = ghost}; region = ghost })]
        | Literal_string (Standard str) -> return_expr @@ [Expr (CST.EString (String   (wrap str)))]
        | Literal_string (Verbatim ver) -> return_expr @@ [Expr (CST.EString (Verbatim (wrap ver)))]
        | Literal_bytes b ->
          let b = Hex.of_bytes b in
          let s = Hex.to_string b in
          return_expr @@ [Expr (CST.EBytes (wrap (s,b)))]
        | Literal_address addr ->
          let addr = CST.EString (String (wrap addr)) in
          let%bind ty = decompile_type_expr @@ AST.t_address () in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (addr,ghost,ty)))]
        | Literal_signature sign ->
          let sign = CST.EString (String (wrap sign)) in
          let%bind ty = decompile_type_expr @@ AST.t_signature () in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (sign,ghost,ty)))]
        | Literal_key k ->
          let k = CST.EString (String (wrap k)) in
          let%bind ty = decompile_type_expr @@ AST.t_key () in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (k,ghost,ty)))]
        | Literal_key_hash kh ->
          let kh = CST.EString (String (wrap kh)) in
          let%bind ty = decompile_type_expr @@ AST.t_key_hash () in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (kh,ghost,ty)))]
        | Literal_chain_id _
        | Literal_operation _ ->
          failwith "chain_id, operation are not created currently ?"
      )
  | E_application {lamb;args} ->
    let%bind lamb = decompile_expression_in lamb in
    let lamb = match lamb with 
      Expr hd :: [] -> hd
    |  _ -> failwith "should not happen"
    in
    let%bind args =
      map (fun xs -> CST.Multiple (wrap (par xs))) @@
      map (fun (hd,tl) -> hd,List.map (fun x -> ghost,x) tl) @@
      map List.Ne.of_list @@
      bind (bind_map_list (fun e -> 
        let%bind x = decompile_expression_in e in 
        match x with 
          Expr hd :: [] -> ok hd
        | _ -> failwith "should not happen"
        )) @@
      get_e_tuple args
    in
    return_expr @@ [Expr (CST.ECall (wrap (lamb,args)))]
  | E_lambda lambda
  | E_recursive {lambda; _} ->
    let%bind (parameters,lhs_type,body) = decompile_lambda lambda in
    let fun_expr : CST.fun_expr = {parameters;lhs_type;arrow=ghost;body} in
    return_expr @@ [Expr (CST.EFun (wrap @@ fun_expr))]
  | E_let_in {let_binder={var;ascr};rhs;let_result;attributes} ->
    let attributes = decompile_attributes attributes in
    let var = CST.PVar (decompile_variable @@ var.wrap_content) in
    let binders = var in
    let%bind lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) ascr in
    let%bind expr = decompile_expression_in rhs in
    let expr = e_hd expr in
    let let_binding = CST.{
      binders;
      lhs_type;
      eq = ghost;
      expr;
      attributes;
    } in
    let const = CST.SConst (wrap CST.{
      kwd_const = ghost;
      bindings  = (wrap let_binding, []);
    }) in
    let%bind body = decompile_expression_in let_result in
    return_expr @@ [Statement const] @ body
  | E_type_in {type_binder;rhs;let_result} ->
    let name = wrap @@ Var.to_name type_binder in
    let%bind type_expr = decompile_type_expr rhs in
    let type_decl : CST.type_decl = {kwd_type=ghost;name;eq=ghost;type_expr} in
    let%bind body = decompile_expression_in let_result in
    return_expr @@ [Statement (CST.SType (wrap type_decl))] @ body
  | E_mod_in {module_binder;rhs;let_result} ->
    let name = wrap module_binder in
    let%bind module_ = decompile_module rhs in
    let toplevel_to_statement = function
        CST.TopLevel (s, _) -> s
      | _ -> failwith "not implemented"
      in
    let a = (fst module_.statements) in
    let statements: CST.statements = (toplevel_to_statement a, List.map (fun e -> (ghost, toplevel_to_statement e)) (snd module_.statements)) in 
    let statements: CST.statements CST.braced Region.reg = wrap @@ braced statements in
    let%bind body = decompile_expression_in let_result in
    ok @@ [Statement (CST.SNamespace (wrap (ghost, name, statements)))] @ body
  | E_mod_alias {alias; binders; result} ->
    let alias   = wrap alias in
    let binders = nelist_to_npseq @@ List.Ne.map wrap binders in
    let mod_alias : CST.import = {kwd_import=ghost;alias;equal=ghost;module_path=binders} in
    let%bind body = decompile_expression_in result in
    return_expr @@ [Statement (CST.SImport (wrap mod_alias))] @ body
  | E_raw_code {language; code} ->
    let language = wrap language in
    let%bind code = decompile_expression_in code in
    let (code, kwd_as, type_expr) = match code with 
      [Expr (CST.EAnnot {value = hd; _})] -> hd
    | _ -> failwith "not implemented"
    in
    return_expr @@ [Expr (CST.EAnnot {value = CST.ECodeInj (wrap CST.{language; code}), kwd_as,type_expr; region = ghost })]
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = wrap constr in
    let%bind element = decompile_expression_in element in
    let element = e_hd element in
    return_expr @@ [Expr (CST.EConstr (EConstrApp (wrap (constr, Some element))))]
  | E_matching {matchee; cases} ->
    let%bind expr  = decompile_expression_in matchee in
    let expr = e_hd expr in
    let%bind cases = decompile_matching_cases cases in
    return_expr @@ [Expr (CST.ECall (wrap (CST.EVar (wrap "match"), CST.Multiple (wrap CST.{lpar = ghost; rpar = ghost; inside = expr, [(ghost, cases)]}) )))]
  | E_record record  ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label str, expr) =
      let field_name = wrap str in
      let%bind field_expr = decompile_expression_in expr in
      let expr = e_hd field_expr in
      let field : CST.property = CST.Property (wrap CST.{name = EVar (wrap str); colon = ghost; value = expr}) in
      ok @@ field
    in
    let%bind record = bind_map_list aux record in
    let%bind record = list_to_nsepseq record in
    let record = braced record in
    return_expr @@ [Expr (CST.EObject (wrap record))]
  | E_accessor {record; path} ->
    let%bind record = decompile_expression_in record in
    let rec proj expr = function
      AST.Access_map e :: rest ->        
        let%bind e = decompile_expression_in e in
        let e = e_hd e in
        let arg = CST.Multiple (wrap (par (e,[ghost,expr]))) in
        proj (CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))) rest
    | AST.Access_tuple index :: rest -> 
      let i = CST.EArith (Int (wrap ("", index))) in
      let p = CST.{
        expr;
        selection = Component (wrap @@ brackets @@ i)
      } in
      proj (CST.EProj (wrap p)) rest
    | AST.Access_record e :: rest ->
      let p = CST.{
        expr;
        selection = FieldName (wrap {dot = ghost; value = wrap e})
      } in
      proj (CST.EProj (wrap p)) rest
    | [] -> ok expr
    in
    let%bind x = proj (e_hd record) path in
    ok @@ [Expr x]
  | E_ascription {anno_expr;type_annotation} ->
    let%bind expr = decompile_expression_in anno_expr in
    let expr = e_hd expr in
    let%bind ty   = decompile_type_expr type_annotation in
    return_expr @@ [Expr (CST.EAnnot (wrap @@ (expr,ghost,ty)))]
  | E_module_accessor {module_name;element} ->
    let module_name = wrap module_name in
    let%bind field  = decompile_expression_in element in
    let field = e_hd field in
    return_expr @@ [Expr (CST.EModA (wrap CST.{module_name;selector=ghost;field}))]
  | E_sequence {expr1;expr2} ->
    let%bind expr1 = decompile_expression_in expr1 in
    let%bind s1 = s_hd expr1 in
    let%bind expr2 = decompile_expression_in expr2 in
    let%bind s2 = s_hd expr2 in
    let l2: statement_or_expr list = [Statement s1; Statement s2] in 
    let%bind s = statements_to_block l2 in
    return_expr [Statement (CST.SBlock s)]
  | E_cond {condition;then_clause;else_clause} ->
    let%bind test  = decompile_expression_in condition in
    let test = CST.{lpar = ghost; rpar = ghost; inside = e_hd test} in
    let%bind ifso  = decompile_expression_in then_clause in
    let%bind ifso = s_hd ifso in
    let%bind ifnot = decompile_expression_in else_clause in
    let%bind ifnot = s_hd ifnot in
    let ifnot = Some(ghost,ifnot) in
    let cond : CST.cond_statement = {kwd_if=ghost;test;ifso;ifnot} in
    return_expr @@ [Statement (CST.SCond (wrap cond))]
  | E_tuple tuple ->
    let%bind tuple = bind_map_list (fun e ->
      let%bind e = decompile_expression_in e in
      ok @@ (CST.Expr_entry (e_hd e))
    ) tuple in
    let%bind tuple = list_to_nsepseq tuple in
    return_expr @@ [Expr (CST.EArray (wrap @@ brackets @@ tuple))]
  | E_map map ->
    let%bind map = bind_map_list (bind_map_pair (fun e ->
      let%bind e = decompile_expression_in e in
      ok @@ (CST.Expr_entry (e_hd e))
    )) map in
    let%bind tuple = list_to_nsepseq map in
    let aux (k,v) = CST.EArray (wrap @@ brackets (k,[(ghost,v)])) in
    let map = List.map aux map in
    (match map with
      [] -> return_expr @@ [Expr (CST.EVar (wrap "Big_map.empty"))]
    | hd::tl  ->
        let var = CST.EVar (wrap "Map.literal") in
        let args = CST.Multiple (wrap (par (hd,List.map (fun x -> ghost,x) tl))) in
      return_expr @@ [Expr (CST.ECall (wrap @@ (var, args)))]
    )
    | E_big_map big_map ->
      let%bind big_map = bind_map_list (bind_map_pair (fun e ->
        let%bind e = decompile_expression_in e in
        ok @@ (CST.Expr_entry (e_hd e))
      )) big_map in
      let aux (k,v) = CST.EArray (wrap @@ brackets (k,[(ghost,v)])) in
      let big_map = List.map aux big_map in
      (match big_map with
        [] -> return_expr @@ [Expr (CST.EVar (wrap "Big_map.empty"))]
      | hd::tl  ->
        let var = CST.EVar (wrap "Big_map.literal") in
        let args = CST.Multiple (wrap (par (hd,List.map (fun x -> ghost,x) tl))) in
        return_expr @@ [Expr (CST.ECall (wrap @@ (var, args)))]
      )
  | E_list lst ->
    let%bind lst = bind_map_list (fun e ->
      let%bind e = decompile_expression_in e in
      ok @@ (CST.Expr_entry (e_hd e))
    ) lst in
    (match lst with 
      [] ->
      let lst = (CST.Empty_entry ghost, []) in
      return_expr @@ [Expr (ECall (wrap (CST.EVar (wrap "list"), CST.Multiple (wrap @@ par @@ (CST.EArray (wrap @@ brackets lst), [] )))))]
    | _ ->
      let%bind lst = list_to_nsepseq lst in
      return_expr @@ [Expr (ECall (wrap (CST.EVar (wrap "list"), CST.Multiple (wrap @@ par @@ (CST.EArray (wrap @@ brackets lst), [] )))))])
  | E_set set ->
    let%bind set = bind_map_list decompile_expression_in set in
    let set = List.map e_hd set in
    let hd,tl = List.Ne.of_list @@ set in
    let var = CST.EVar (wrap "Set.literal") in
    let args = CST.Multiple (wrap (par (hd,List.map (fun x -> ghost,x) tl))) in
    return_expr @@ [Expr (CST.ECall (wrap @@ (var,args)))]
  (* We should avoid to generate skip instruction*)
  | E_skip -> return_expr @@ [Expr (CST.EUnit (wrap (ghost,ghost)))]
  | E_assign {variable;access_path;expression} when List.length access_path > 0 ->
    failwith "Assignments with access paths are not supported by JsLIGO."
  | E_assign {variable;expression;_} ->
    let name = Var.to_name variable.wrap_content in
    let evar = CST.EVar (wrap name) in
    let%bind rhs = decompile_expression_in expression in
    return_expr @@ [Expr (CST.EAssign (evar, ghost, e_hd rhs))]
  | E_for_each {fe_binder;collection;fe_body; _} ->
    let var = decompile_variable @@ (fst fe_binder).wrap_content in
    let bind_to = Option.map (fun (x:AST.expression_variable) -> (ghost,decompile_variable x.wrap_content)) @@ snd fe_binder in
    let%bind expr = decompile_expression_in collection in
    let expr = e_hd expr in
    let%bind block = decompile_expression_in fe_body in
    let%bind statement = s_hd block in
    let for_of : CST.for_of = {kwd_for=ghost;lpar=ghost;const=true;name=var;kwd_of=ghost;expr;rpar=ghost;statement} in
    return_expr [Statement (CST.SForOf (wrap for_of))]
  | E_while {cond;body} ->
    let%bind cond  = decompile_expression_in cond in
    let expr = e_hd cond in
    let%bind block = decompile_expression_in body in
    let%bind statement = s_hd block in
    let loop : CST.while_ = {kwd_while=ghost;lpar=ghost;expr;rpar=ghost;statement} in
    return_expr @@ [Statement (CST.SWhile (wrap loop))]
  | E_for _ ->
    failwith @@ Format.asprintf "Decompiling a for loop to JsLIGO %a"
    AST.PP.expression expr   
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {record;path;update} when List.length path > 1 ->
    failwith "Nested updates are not supported in JsLIGO."
  | E_update {record; path; update} ->
    let%bind record = decompile_expression_in record in
    let expr = e_hd record in
    let name = match path with 
      [AST.Access_record name] -> CST.EVar (wrap name)
    | _ -> failwith "not supported"
    in
    let%bind update = decompile_expression_in update in 
    let update = e_hd update in
    let p:CST.property = CST.Property (wrap CST.{
      name;
      colon = ghost;
      value = update
    }) in
    return_expr @@ [Expr (CST.EObject (wrap @@ braced (CST.Property_rest (wrap ({expr; ellipsis = ghost}: CST.property_rest)), [(ghost, p)])))]

and statements_to_block (statements: statement_or_expr list) = 
  let statements = List.map (fun f ->
    match f with 
      Statement s -> s
    | Expr e -> SExpr e
  ) statements in
  let%bind s = list_to_nsepseq statements in
  ok @@ wrap @@ braced s

and add_return statements = 
  let statements = List.rev statements in
  let (last, before) = match statements with 
    Statement last :: before -> (last, before)
  | Expr last :: before -> (SExpr last, before)
  | _ -> failwith "not implemented"
  in
  let rec aux l =
    match l with 
      CST.SExpr (EUnit _) -> ok @@ CST.SReturn (wrap CST.{kwd_return = ghost; expr = None})
    | CST.SExpr e -> ok @@ CST.SReturn (wrap CST.{kwd_return = ghost; expr = Some e})
    | CST.SCond {value = {kwd_if; test; ifso; ifnot}; region} -> 
      let%bind ifso = aux ifso in
      let%bind ifnot = match ifnot with 
        Some (e, s) -> 
          let%bind s = aux s in
          ok @@ Some (e, s)
      | None -> ok @@ None
      in
      ok @@ CST.SCond {value = {kwd_if; test; ifso; ifnot}; region }
    | CST.SBlock {value = {lbrace; inside; rbrace}; region} -> 
      let inside = Utils.nsepseq_to_list inside in
      let inside = List.rev inside in 
      let (last, before) = (match inside with 
        last :: before -> (last, before)
      | [] -> failwith "not implemented"
      ) in
      let%bind last = aux last in
      let inside = last :: before in
      let inside = List.rev inside in
      let%bind inside = list_to_nsepseq inside in
      ok @@ CST.SBlock {value = {lbrace; inside; rbrace}; region}      
    | _ -> failwith "not implemented"
  in
  let%bind last = aux last in
  ok @@ List.rev (Statement last :: before)

and function_body body = 
  let%bind body = match body with 
  | [Expr e] -> ok @@ CST.ExpressionBody e
  | (_ :: _) as s -> 
    let%bind s = add_return s in
    let%bind o = statements_to_block s in
    
    ok @@ CST.FunctionBody o
  | _ -> failwith "not supported"
  in
  ok @@ body

and decompile_lambda : (AST.expr, AST.ty_expr) AST.lambda -> _ =
  fun {binder;output_type;result} ->
    let%bind type_expr = bind_map_option decompile_type_expr binder.ascr in
    let type_expr = Option.unopt ~default:(CST.TWild ghost) type_expr in
    let v = decompile_variable binder.var.wrap_content in
    let seq = CST.ESeq (wrap (CST.EAnnot (wrap (CST.EVar v,ghost,type_expr)), [])) in
    let parameters = CST.EPar (wrap @@ par seq ) in
    let%bind lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) output_type in    
    let%bind body = decompile_expression_in result in
    let%bind body = function_body body in
    ok @@ (parameters, lhs_type, body)

and decompile_matching_cases : _ AST.match_case list -> (CST.expr,_) result =
  fun m -> ignore m ; failwith "TODO: decompile matching cases"
    (* old version (before deep pattern matching) :
    let cases = match m with
    | Match_variant lst ->
      let aux ((c,(v:AST.expression_variable)),e) =
        let AST.Label c = c in
        let%bind rhs = decompile_expression_in e in
        let rhs = e_hd rhs in
        ok @@ (CST.Property (wrap ({
          name = CST.EVar (wrap c);
          colon = ghost;
          value = rhs;
        }: CST.property2)))
      in
      let%bind fields = bind_map_list aux lst in 
      let%bind fields = list_to_nsepseq fields in
      ok @@ CST.EObject (wrap @@ braced fields)
    | Match_list {match_nil; match_cons} ->
      let (hd,tl,expr) = match_cons in
      let%bind nil_expr = decompile_expression_in match_nil in
      let%bind body = function_body nil_expr in
      let nil  = CST.EFun (wrap CST.{
        parameters = EAnnot (wrap (EArray (wrap @@ brackets (Empty_entry ghost, [])), ghost, TVar (wrap "any"))); 
        lhs_type = None;
        arrow = ghost; 
        body}) in
      let%bind cons_expr = decompile_expression_in expr in
      let%bind body = function_body cons_expr in
      let hd = Var.to_name hd.wrap_content in
      let tl = ({expr = EVar (wrap (Var.to_name tl.wrap_content)); ellipsis = ghost }: CST.array_item_rest) in
      let cons  = CST.EFun (wrap CST.{
        parameters = EAnnot (wrap (EArray (wrap @@ brackets (Expr_entry (EVar (wrap hd)), [(ghost, (Rest_entry (wrap tl)))])), ghost, TVar (wrap "any"))); 
        lhs_type = None;
        arrow = ghost; 
        body}) in
      let args = (CST.Expr_entry cons, [(ghost, CST.Expr_entry nil)]) in
      ok @@ CST.ECall (wrap ((CST.EVar (wrap "list")), CST.Multiple (wrap @@ par (CST.EArray (wrap @@ brackets args), []))))
    | Match_variable (var, expr) -> failwith "not implemented"
    | Match_record _ -> failwith "match_record not available yet"
    | Match_tuple (lst, expr) ->  failwith "not implemented"
    | Match_option {match_none;match_some} ->
      let%bind a = decompile_expression_in match_none in
      let%bind body = function_body a in
      let name = Var.to_name (fst match_some).wrap_content in
      let%bind body2 = decompile_expression_in (snd match_some) in
      let%bind body2 = function_body body2 in
      let fields = CST.[
        CST.Property (wrap 
          {name = CST.EVar (wrap "Some"); 
          colon = ghost; 
          value = EFun (wrap CST.{parameters = EAnnot (wrap (EVar (wrap name), ghost, TVar (wrap "any"))); lhs_type = None; arrow = ghost; body = body2})
          });
          CST.Property (wrap 
            {name = CST.EVar (wrap "None"); 
            colon = ghost; 
            value = EFun (wrap CST.{parameters = EUnit (wrap (ghost, ghost)); lhs_type = None; arrow = ghost; body})
          })
      ] in
      let%bind fields = list_to_nsepseq fields in
      ok @@ CST.EObject (wrap @@ braced fields)
  in
  cases
  *)

and decompile_declaration : AST.declaration Location.wrap -> (CST.statement, _) result = fun decl ->
  let decl = Location.unwrap decl in
  let wrap value = ({value;region=Region.ghost} : _ Region.reg) in
  match decl with
    Declaration_type {type_binder;type_expr} ->
    let name = decompile_variable type_binder in
    let%bind type_expr = decompile_type_expr type_expr in
    ok @@ CST.SType (wrap (CST.{kwd_type=ghost; name; eq=ghost; type_expr}))
  | Declaration_constant {binder; attr; expr; } ->
    let attributes : CST.attributes = decompile_attributes attr in
    let var = CST.PVar (decompile_variable binder.var.wrap_content) in
    let binders = var in
    let%bind lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) binder.ascr in
    let%bind expr = decompile_expression_in expr in
    let expr = e_hd expr in
    let binding = CST.({
      binders;
      lhs_type;
      eq = Region.ghost;
      expr;
      attributes
    }) in
    ok @@ CST.SConst (wrap (CST.{kwd_const=ghost; bindings = (wrap binding, [])}))
  | Declaration_module {module_binder; module_} ->
    let name = wrap module_binder in
    let%bind module_ = decompile_module module_ in
    let toplevel_to_statement = function
        CST.TopLevel (s, _) -> s
      | _ -> failwith "not implemented"
      in
    let a = (fst module_.statements) in
    let statements: CST.statements = (toplevel_to_statement a, List.map (fun e -> (ghost, toplevel_to_statement e)) (snd module_.statements)) in 
    let statements: CST.statements CST.braced Region.reg = wrap @@ braced statements in
    ok @@ CST.SNamespace (wrap (ghost, name, statements))
  | Module_alias {alias; binders} ->
    let alias = wrap alias in
    let binders = nelist_to_npseq @@ List.Ne.map wrap binders in
    ok @@ CST.SImport (wrap CST.{alias; module_path = binders; kwd_import = ghost; equal = ghost})

and decompile_module : AST.module_ -> (CST.ast, _) result = fun prg ->
  let%bind decl = bind_map_list decompile_declaration prg in
  let statements = List.Ne.of_list decl in
  let statements = Utils.nseq_map (fun s -> CST.TopLevel (s, None)) statements in
  (* let statements = ((fst statements, None), List.map (fun e -> (e, None)) (snd statements)) in *)
  ok @@ ({statements;eof=ghost}: CST.ast)
