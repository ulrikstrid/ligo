module AST = Ast_imperative
module CST = Cst.Reasonligo
module Predefined = Predefined.Tree_abstraction.Reasonligo

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

let brackets = Some (CST.Brackets (ghost,ghost))

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
      let* arg = decompile_type_expr associated_type in
      let arg = Some (ghost, arg) in
      let attributes = decompile_attributes attributes in
      let variant : CST.variant = {constr; arg; attributes} in
      ok @@ wrap variant
    in
    let* variants = bind_map_list aux lst in
    let* variants = list_to_nsepseq variants in
    let lead_vbar = Some ghost in
    let attributes = decompile_attributes attributes in
    let sum : CST.sum_type = { lead_vbar ; variants ; attributes} in
    return @@ CST.TSum (wrap sum)
  | T_record {fields; attributes} ->
     let record = AST.LMap.to_kv_list fields in
     let aux (AST.Label c, AST.{associated_type; attributes; _}) =
       let field_name = wrap c in
       let colon = ghost in
       let* field_type = decompile_type_expr associated_type in
       let attributes = decompile_attributes attributes in
       let field : CST.field_decl =
         {field_name; colon; field_type; attributes} in
       ok @@ wrap field in
     let* record = bind_map_list aux record in
     let* record = list_to_nsepseq record in
    let attributes = List.map (fun el -> wrap el) attributes in
     return @@ CST.TRecord (wrap @@ ne_inject braces record ~attr:attributes)
  | T_tuple tuple ->
    let* tuple = bind_map_list decompile_type_expr tuple in
    let* tuple = list_to_nsepseq tuple in
    let tuple = CST.{lpar=ghost; inside=tuple; rpar=ghost} in
    return @@ CST.TProd (wrap tuple)
  | T_arrow {type1;type2} ->
    let* type1 = decompile_type_expr type1 in
    let* type2 = decompile_type_expr type2 in
    let arrow = (type1, ghost, type2) in
    return @@ CST.TFun (wrap arrow)
  | T_variable variable ->
    let var = decompile_variable variable in
    return @@ CST.TVar var
  | T_app {type_operator; arguments} ->
    let type_operator = wrap @@ Var.to_name type_operator in
    let* lst = bind_map_list decompile_type_expr arguments in
    let* lst = list_to_nsepseq lst in
    let lst : _ CST.par = {lpar=ghost;inside=lst;rpar=ghost} in
    return @@ CST.TApp (wrap (type_operator,wrap lst))
  | T_annoted _annot ->
    failwith "let's work on it later"
  | T_module_accessor {module_name;element} ->
    let module_name = wrap module_name in
    let* field  = decompile_type_expr element in
    return @@ CST.TModA (wrap CST.{module_name;selector=ghost;field})
  | T_singleton x -> (
    match x with
    | Literal_int i ->
      let z : CST.type_expr = CST.TInt { region = Region.ghost ; value = (Z.to_string i, i) } in
      return z
    | _ -> failwith "unsupported singleton"
  )

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

let pattern_type ({var;ascr}: _ AST.binder) =
  let var = CST.PVar (decompile_variable var.wrap_content) in
  let* type_expr = bind_map_option decompile_type_expr ascr in
  let type_expr = Option.unopt ~default:(CST.TWild ghost) type_expr in
  ok @@ CST.PTyped (wrap @@ CST.{pattern=var;colon=ghost;type_expr})

let rec decompile_expression : AST.expression -> _ result = fun expr ->
  let return_expr expr = ok @@ expr in
  let return_expr_with_par expr = return_expr @@ CST.EPar (wrap @@ par @@ expr) in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name.wrap_content in
    return_expr @@ CST.EVar (var)
  | E_constant {cons_name; arguments} ->
    let expr = CST.EVar (wrap @@ Predefined.constant_to_string cons_name) in
    (match arguments with
      [] -> return_expr @@ expr
    | _ ->
       let* arguments =
        map (fun xs -> CST.Multiple (wrap (par xs))) @@
        map (fun (hd,tl) -> hd,List.map (fun x -> ghost,x) tl) @@
        map List.Ne.of_list @@
        map (List.map (fun x -> CST.EPar (wrap @@ par @@ x))) @@
        bind_map_list decompile_expression arguments in
      let const = wrap (expr, arguments) in
      return_expr_with_par @@ CST.ECall const
    )
  | E_literal literal ->
    (match literal with
        Literal_unit  ->  return_expr @@ CST.EUnit (wrap (ghost,ghost))
      | Literal_int i ->  return_expr @@ CST.EArith (Int (wrap ("",i)))
      | Literal_nat n ->  return_expr @@ CST.EArith (Nat (wrap ("",n)))
      | Literal_timestamp time ->
        let time = Tezos_utils.Time.Protocol.to_notation @@
          Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time in
          (* TODO combinators for CSTs. *)
        let* ty = decompile_type_expr @@ AST.t_timestamp () in
        let time = CST.EString (String (wrap time)) in
        return_expr_with_par @@ CST.EAnnot (wrap @@ (time, ghost, ty))
      | Literal_mutez mtez -> return_expr @@ CST.EArith (Mutez (wrap ("",mtez)))
      | Literal_string (Standard str) -> return_expr @@ CST.EString (String   (wrap str))
      | Literal_string (Verbatim ver) -> return_expr @@ CST.EString (Verbatim (wrap ver))
      | Literal_bytes b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        return_expr @@ CST.EBytes (wrap (s,b))
      | Literal_address addr ->
        let addr = CST.EString (String (wrap addr)) in
        let* ty = decompile_type_expr @@ AST.t_address () in
        return_expr_with_par @@ CST.EAnnot (wrap @@ (addr,ghost,ty))
      | Literal_signature sign ->
        let sign = CST.EString (String (wrap sign)) in
        let* ty = decompile_type_expr @@ AST.t_signature () in
        return_expr_with_par @@ CST.EAnnot (wrap @@ (sign,ghost,ty))
      | Literal_key k ->
        let k = CST.EString (String (wrap k)) in
        let* ty = decompile_type_expr @@ AST.t_key () in
        return_expr_with_par @@ CST.EAnnot (wrap @@ (k,ghost,ty))
      | Literal_key_hash kh ->
        let kh = CST.EString (String (wrap kh)) in
        let* ty = decompile_type_expr @@ AST.t_key_hash () in
        return_expr_with_par @@ CST.EAnnot (wrap @@ (kh,ghost,ty))
      | Literal_chain_id _
      | Literal_operation _ ->
        failwith "chain_id, operation are not created currently ?"
    )
  | E_application {lamb;args} ->
    let* lamb = decompile_expression lamb in
    let* args =
      map (fun xs -> CST.Multiple (wrap (par xs))) @@
      map (fun (hd,tl) -> hd,List.map (fun x -> ghost,x) tl) @@
      map List.Ne.of_list @@
      bind (bind_map_list decompile_expression) @@
      get_e_tuple args
    in
    return_expr @@ CST.ECall (wrap (lamb,args))
  | E_lambda lambda ->
    let* (binders,lhs_type,body) = decompile_lambda lambda in
    let fun_expr : CST.fun_expr = {binders;lhs_type;arrow=ghost;body} in
    return_expr_with_par @@ CST.EFun (wrap @@ fun_expr)
  | E_recursive _ ->
    failwith "corner case : annonymous recursive function"
  | E_let_in {let_binder={var;ascr};rhs;let_result;attributes} ->
    let var = CST.PVar (decompile_variable @@ var.wrap_content) in
    let binders = var in
    let* lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) ascr in
    let* let_rhs = decompile_expression rhs in
    let binding : CST.let_binding = {binders;lhs_type;eq=ghost;let_rhs} in
    let* body = decompile_expression let_result in
    let attributes = decompile_attributes attributes in
    let lin : CST.let_in = {kwd_let=ghost;kwd_rec=None;binding;semi=ghost;body;attributes} in
    return_expr @@ CST.ELetIn (wrap lin)
  | E_type_in {type_binder;rhs;let_result} ->
    let name = wrap @@ Var.to_name type_binder in
    let* type_expr = decompile_type_expr rhs in
    let type_decl : CST.type_decl = {kwd_type=ghost;name;eq=ghost;type_expr} in
    let* body = decompile_expression let_result in
    let tin : CST.type_in = {type_decl;semi=ghost;body} in
    return_expr @@ CST.ETypeIn (wrap tin)
  | E_mod_in {module_binder;rhs;let_result} ->
    let name = wrap module_binder in
    let* module_ = decompile_module rhs in
    let mod_decl : CST.module_decl = {kwd_module=ghost;name;eq=ghost;lbrace=ghost;module_;rbrace=ghost} in
    let* body = decompile_expression let_result in
    let tin : CST.mod_in = {mod_decl;semi=ghost;body} in
    return_expr @@ CST.EModIn (wrap tin)
  | E_mod_alias {alias; binders; result} ->
    let alias   = wrap alias in
    let binders = nelist_to_npseq @@ List.Ne.map wrap binders in
    let mod_alias : CST.module_alias = {kwd_module=ghost;alias;eq=ghost;binders} in
    let* body = decompile_expression result in
    let mod_alias : CST.mod_alias = {mod_alias;semi=ghost;body} in
    return_expr @@ CST.EModAlias (wrap mod_alias)
  | E_raw_code {language; code} ->
    let language = wrap @@ wrap @@ language in
    let* code = decompile_expression code in
    let ci : CST.code_inj = {language;code;rbracket=ghost} in
    return_expr @@ CST.ECodeInj (wrap ci)
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = wrap constr in
    let* element = decompile_expression element in
    return_expr_with_par @@ CST.EConstr (EConstrApp (wrap (constr, Some element)))
  | E_matching {matchee; cases} ->
    let* expr  = decompile_expression matchee in
    let aux : _ AST.match_case -> (_ CST.case_clause CST.reg,_) result =
      fun { pattern ; body } ->
        let* rhs = decompile_expression body in
        let* pattern = decompile_pattern pattern in
        ok (wrap ({pattern ; arrow = ghost ; rhs ; terminator = Some ghost}:_ CST.case_clause))
    in
    let* case_clauses = bind_map_list aux cases in 
    let* cases = list_to_nsepseq case_clauses in
    let cases = wrap cases in
    let cases : _ CST.case = {kwd_switch=ghost;lbrace=ghost;rbrace=ghost;expr;cases} in
    return_expr @@ CST.ECase (wrap cases)
  | E_record record  ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label str, expr) =
      let field_name = wrap str in
      let* field_expr = decompile_expression expr in
      let field : CST.field_assign = {field_name;assignment=ghost;field_expr} in
      ok @@ wrap field
    in
    let* record = bind_map_list aux record in
    let* record = list_to_nsepseq record in
    let record = ne_inject braces record ~attr:[] in
    (* why is the record not empty ? *)
    return_expr @@ CST.ERecord (wrap record)
  | E_accessor {record; path} ->
    (match List.rev path with
      Access_map e :: [] ->
      let* map = decompile_expression record in
      let* e = decompile_expression e in
      let arg = CST.Multiple (wrap (par (e,[ghost,map]))) in
      return_expr @@ CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))
    | Access_map e :: lst ->
      let path = List.rev lst in
      let* field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection path in
      let* struct_name = map (decompile_variable) @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=ghost;field_path} in
      let* e = decompile_expression e in
      let arg =  CST.Multiple (wrap (par (e,[ghost, CST.EProj (wrap proj)]))) in
      return_expr @@ CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))
    | _ ->
      let* field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection path in
       let* struct_name = map (decompile_variable) @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=ghost;field_path} in
      return_expr @@ CST.EProj (wrap proj)
    )
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {record={expression_content=E_update _;_} as record;path;update} ->
    let* record = decompile_expression record in
    let* (record,updates) = match record with
      CST.EUpdate {value;_} -> ok @@ (value.record,value.updates)
    | _ -> failwith @@ Format.asprintf "Inpossible case %a" AST.PP.expression expr
    in
    let* var,path = match path with
      Access_record var::path -> ok @@ (var,path)
    | _ -> failwith "Impossible case %a"
    in
    let* field_path = decompile_to_path (Location.wrap @@ Var.of_name var) path in
    let* field_expr = decompile_expression update in
    let field_assign : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
    let updates = updates.value.ne_elements in
    let updates =
      wrap @@ ne_inject ~attr:[] braces @@ npseq_cons (wrap @@ field_assign) updates in
    let update : CST.update = {lbrace=ghost;record;ellipsis=ghost;comma=ghost;updates;rbrace=ghost} in
    return_expr @@ CST.EUpdate (wrap @@ update)
  | E_update {record; path; update} ->
    let* record = map (decompile_variable) @@ get_e_variable record in
    let* field_expr = decompile_expression update in
    let (struct_name,field_path) = List.Ne.of_list path in
    (match field_path with
      [] ->
      (match struct_name with
        Access_record name ->
        let record : CST.path = Name record in
        let field_path = CST.Name (wrap name) in
        let update : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=ghost;record;ellipsis=ghost;comma=ghost;updates;rbrace=ghost} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_tuple i ->
        let record : CST.path = Name record in
        let field_path = CST.Name (wrap @@ Z.to_string i) in
        let update : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=ghost;record;ellipsis=ghost;comma=ghost;updates;rbrace=ghost} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_map e ->
        let* e = decompile_expression e in
        let arg = CST.Multiple (wrap (par (field_expr,[ghost,e; ghost,CST.EVar record]))) in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"), arg))
      )
    | _ ->
      let* struct_name = match struct_name with
          Access_record name -> ok @@ wrap name
        | Access_tuple i -> ok @@ wrap @@ Z.to_string i
        | Access_map _ -> failwith @@ Format.asprintf "invalid map update %a" AST.PP.expression expr
      in
      (match List.rev field_path with
        Access_map e :: lst ->
        let field_path = List.rev lst in
        let* field_path = bind_map_list decompile_to_selection field_path in
        let* field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=ghost;field_path} in
        let field_path = CST.EProj (wrap @@ field_path) in
        let* e = decompile_expression e in
        let arg = CST.Multiple (wrap (par (field_expr, [ghost,e; ghost,field_path]))) in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"),arg))
      | _ ->
        let* field_path = bind_map_list decompile_to_selection field_path in
        let* field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=ghost;field_path} in
        let field_path = CST.Path (wrap @@ field_path) in
        let record : CST.path = Name record in
        let update : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=ghost;record;ellipsis=ghost;comma=ghost;updates;rbrace=ghost} in
        return_expr @@ CST.EUpdate (wrap update)
      )
    )
  | E_ascription {anno_expr;type_annotation} ->
    let* expr = decompile_expression anno_expr in
    let* ty   = decompile_type_expr type_annotation in
    return_expr_with_par @@ CST.EAnnot (wrap @@ (expr,ghost,ty))
  | E_module_accessor {module_name;element} ->
    let module_name = wrap module_name in
    let* field  = decompile_expression element in
    return_expr @@ CST.EModA (wrap CST.{module_name;selector=ghost;field})
  | E_cond {condition;then_clause;else_clause} ->
    let* test  = decompile_expression condition in
    let* ifso  = decompile_expression then_clause in
    let ifso = CST.{lbrace=ghost; inside=(ifso,None); rbrace=ghost} in
    let* ifnot = decompile_expression else_clause in
    let ifnot = CST.{lbrace=ghost; inside=(ifnot,None); rbrace=ghost} in
    let ifnot = Some(ghost,ifnot) in
    let cond : CST.cond_expr = {kwd_if=ghost;test;ifso;ifnot} in
    return_expr @@ CST.ECond (wrap cond)
  | E_sequence {expr1;expr2} ->
    let* expr1 = decompile_expression expr1 in
    let* expr2 = decompile_expression expr2 in
    return_expr @@ CST.ESeq (wrap @@ inject braces @@ list_to_sepseq [expr1; expr2])
  | E_tuple tuple ->
    let* tuple = bind_map_list decompile_expression tuple in
    let* tuple = list_to_nsepseq tuple in
    return_expr @@ CST.ETuple (wrap @@ tuple)
  | E_map map ->
    let* map = bind_map_list (bind_map_pair decompile_expression) map in
    let aux (k,v) = CST.ETuple (wrap (k,[(ghost,v)])) in
    let map = List.map aux map in
    (match map with
      [] -> return_expr @@ CST.EVar (wrap "Big_map.empty")
    | hd::tl  ->
       let var = CST.EVar (wrap "Map.literal") in
       let args = CST.Multiple (wrap (par (hd,List.map (fun x -> ghost,x) tl))) in
      return_expr @@ CST.ECall (wrap @@ (var, args))
    )
  | E_big_map big_map ->
    let* big_map = bind_map_list (bind_map_pair decompile_expression) big_map in
    let aux (k,v) = CST.ETuple (wrap (k,[(ghost,v)])) in
    let big_map = List.map aux big_map in
    (match big_map with
      [] -> return_expr @@ CST.EVar (wrap "Big_map.empty")
    | hd::tl  ->
      let var = CST.EVar (wrap "Big_map.literal") in
      let args = CST.Multiple (wrap (par (hd,List.map (fun x -> ghost,x) tl))) in
      return_expr @@ CST.ECall (wrap @@ (var, args))
    )
  | E_list lst ->
    let* lst = bind_map_list decompile_expression lst in
    let lst = list_to_sepseq lst in
    return_expr @@ CST.EList (EListComp (wrap @@ inject brackets @@ lst))
  | E_set set ->
    let* set = bind_map_list decompile_expression set in
    let hd,tl = List.Ne.of_list @@ set in
    let var = CST.EVar (wrap "Set.literal") in
    let args = CST.Multiple (wrap (par (hd,List.map (fun x -> ghost,x) tl))) in
    return_expr @@ CST.ECall (wrap @@ (var,args))
    (* We should avoid to generate skip instruction*)
  | E_skip -> return_expr @@ CST.EUnit (wrap (ghost,ghost))
  | E_assign _
  | E_for _
  | E_for_each _
  | E_while _ ->
    failwith @@ Format.asprintf "Decompiling a imperative construct to CameLIGO %a"
    AST.PP.expression expr

and decompile_to_path : AST.expression_variable -> _ AST.access list -> (CST.path, _) result = fun var access ->
  let struct_name = decompile_variable var.wrap_content in
  match access with
    [] -> ok @@ CST.Name struct_name
  | lst ->
    let* field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection lst in
    let path : CST.projection = {struct_name;selector=ghost;field_path} in
    ok @@ (CST.Path (wrap @@ path) : CST.path)

and decompile_to_selection : _ AST.access -> (CST.selection, _) result = fun access ->
  match access with
    Access_tuple index -> ok @@ CST.Component (wrap @@ ("",index))
  | Access_record str  -> ok @@ CST.FieldName (wrap str)
  | Access_map _ ->
    failwith @@ Format.asprintf
    "Can't decompile access_map to selection"

and decompile_lambda : (AST.expr, AST.ty_expr) AST.lambda -> _ =
  fun {binder;output_type;result} ->
    let* param_decl = pattern_type binder in
    let param = CST.PPar (wrap @@ par @@ param_decl) in
    let* ret_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) output_type in
    let* return = decompile_expression result in
    ok @@ (param,ret_type,return)

and decompile_declaration : AST.declaration Location.wrap -> (CST.declaration, _) result = fun decl ->
  let decl = Location.unwrap decl in
  let wrap value = ({value;region=Region.ghost} : _ Region.reg) in
  match decl with
    Declaration_type {type_binder;type_expr} ->
    let name = decompile_variable type_binder in
    let* type_expr = decompile_type_expr type_expr in
    ok @@ CST.TypeDecl (wrap (CST.{kwd_type=ghost; name; eq=ghost; type_expr}))
  | Declaration_constant {binder;attr;expr}-> (
    let attributes : CST.attributes = decompile_attributes attr in
    let var = CST.PVar (decompile_variable binder.var.wrap_content) in
    let binders = var in
    let* lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) binder.ascr in
    match expr.expression_content with
      E_lambda lambda ->
      let* let_rhs = decompile_expression @@ AST.make_e @@ AST.E_lambda lambda in
      let let_binding : CST.let_binding = {binders;lhs_type;eq=ghost;let_rhs} in
      let let_decl = wrap (ghost,None,let_binding,attributes) in
      ok @@ CST.ConstDecl let_decl
    | E_recursive {lambda; _} ->
      let* let_rhs = decompile_expression @@ AST.make_e @@ AST.E_lambda lambda in
      let let_binding : CST.let_binding = {binders;lhs_type;eq=ghost;let_rhs} in
      let let_decl = wrap (ghost,Some ghost,let_binding,attributes) in
      ok @@ CST.ConstDecl let_decl
    | _ ->
      let* let_rhs = decompile_expression expr in
      let let_binding : CST.let_binding = {binders;lhs_type;eq=ghost;let_rhs} in
      let let_decl = wrap (ghost,None,let_binding,attributes) in
      ok @@ CST.ConstDecl let_decl
  )
  | Declaration_module {module_binder;module_} ->
    let name = wrap module_binder in
    let* module_ = decompile_module module_ in
    ok @@ CST.ModuleDecl (wrap (CST.{kwd_module=ghost; name; eq=ghost; lbrace=ghost; module_; rbrace=ghost}))
  | Module_alias {alias;binders} ->
    let alias   = wrap alias in
    let binders = nelist_to_npseq @@ List.Ne.map wrap binders in
    ok @@ CST.ModuleAlias (wrap (CST.{kwd_module=ghost; alias; eq=ghost; binders}))

and decompile_pattern : AST.type_expression AST.pattern -> (CST.pattern,_) result =
  fun pattern ->
    match pattern.wrap_content with
    | AST.P_unit -> ok @@ CST.PUnit (wrap (ghost, ghost))
    | AST.P_var v ->
      let name = (decompile_variable v.var.wrap_content).value in
      ok @@ CST.PVar (wrap name)
    | AST.P_list pl -> (
      let ret x = ok (CST.PList x) in
      match pl with
      | AST.Cons (pa,pb) ->
        let* pa = decompile_pattern pa in
        let* pb = decompile_pattern pb in
        let cons :  CST.cons_pattern =
          { lbracket = ghost;
            lpattern = pa;
            comma    = ghost;
            ellipsis = ghost;
            rpattern = pb;
            rbracket = ghost;
          }
        in
        ret (PCons (wrap cons))
      | AST.List [] ->
        let nil = list_to_sepseq [] in
        let injection = wrap @@ inject (brackets) nil in
        ret (PListComp injection)
      | AST.List plst ->
        let* plst = bind_map_list decompile_pattern plst in
        let plst = list_to_sepseq plst in
        let injection = wrap @@ inject (brackets) plst in
        ret (PListComp injection)
    )
    | AST.P_variant (constructor,p) -> (
      match constructor with
      | Label "Some" ->
        let* p = decompile_pattern p in
        let proj = wrap (ghost, p) in
        ok @@ CST.PConstr (PSomeApp proj)
      | Label "None" -> ok @@ CST.PConstr (PNone ghost)
      | Label "true" -> ok @@ CST.PConstr (PTrue ghost)
      | Label "false" -> ok @@ CST.PConstr (PFalse ghost)
      | Label constructor -> (
        let* p = decompile_pattern p in
        let constr = wrap (wrap constructor, Some p) in
        ok @@ CST.PConstr (PConstrApp constr)
      )
    )
    | AST.P_tuple lst ->
      let* pl = bind_map_list decompile_pattern lst in
      let* pl = list_to_nsepseq pl in
      ok @@ CST.PTuple (wrap pl)
    | AST.P_record (llst,lst) ->
      let* pl = bind_map_list decompile_pattern lst in
      let fields_name = List.map (fun (AST.Label x) -> wrap x) llst in
      let field_patterns =
        List.map
          (fun (field_name,pattern) -> wrap ({ field_name ; eq = ghost ; pattern }:CST.field_pattern))
          (List.combine fields_name pl)
      in
      let* field_patterns = list_to_nsepseq field_patterns in
      let inj = ne_inject braces field_patterns ~attr:[] in
      ok @@ CST.PRecord (wrap inj)

and decompile_module : AST.module_ -> (CST.ast, _) result = fun prg ->
  let* decl = bind_map_list decompile_declaration prg in
  let decl = List.Ne.of_list decl in
  ok @@ ({decl;eof=ghost}: CST.ast)
