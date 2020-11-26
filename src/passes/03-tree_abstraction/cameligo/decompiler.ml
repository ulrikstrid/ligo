module AST = Ast_imperative
module CST = Cst.Cameligo
module Predefined = Predefined.Tree_abstraction.Cameligo

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

let braces   = Some (CST.Braces (ghost,ghost))
let brackets = Some (CST.Brackets (ghost,ghost))
let beginEnd = Some (CST.BeginEnd (ghost,ghost))

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
    let attributes = decompile_attributes attributes in
    let lst = AST.LMap.to_kv_list fields in
    let aux (AST.Label c, AST.{associated_type; attributes=row_attr; _}) =
      let constr = wrap c in
      let%bind arg = decompile_type_expr associated_type in
      let arg = Some (ghost, arg) in
      let row_attr = decompile_attributes row_attr in
      let variant : CST.variant = {constr; arg; attributes=row_attr} in
      ok @@ wrap variant in
    let%bind variants = bind_map_list aux lst in
    let%bind variants = list_to_nsepseq variants in
    let lead_vbar = Some ghost in
    let sum : CST.sum_type = {lead_vbar; variants; attributes} in
    return @@ CST.TSum (wrap sum)
  | T_record {fields; attributes} ->
     let record = AST.LMap.to_kv_list fields in
     let aux (AST.Label c, AST.{associated_type; attributes=field_attr; _}) =
      let field_name = wrap c in
      let colon = ghost in
      let%bind field_type = decompile_type_expr associated_type in
      let field_attr = decompile_attributes field_attr in
      let field : CST.field_decl =
        {field_name; colon; field_type; attributes=field_attr} in
      ok @@ wrap field in
    let%bind record = bind_map_list aux record in
    let%bind record = list_to_nsepseq record in
    let attributes = decompile_attributes attributes in
    return @@ CST.TRecord (wrap @@ ne_inject braces record ~attr:attributes)
  | T_tuple tuple ->
    let%bind tuple = bind_map_list decompile_type_expr tuple in
    let%bind tuple = list_to_nsepseq @@ tuple in
    return @@ CST.TProd (wrap tuple)
  | T_arrow {type1;type2} ->
    let%bind type1 = decompile_type_expr type1 in
    let%bind type2 = decompile_type_expr type2 in
    let arrow = (type1, ghost, type2) in
    return @@ CST.TFun (wrap arrow)
  | T_variable variable ->
    let var = decompile_variable variable in
    return @@ CST.TVar var
  | T_app {type_operator; arguments} ->
    let type_constant = wrap @@ Var.to_name type_operator in
    let%bind arguments = bind_map_list decompile_type_expr arguments in
    let%bind arguments = list_to_nsepseq arguments in
    let lst : _ CST.par = {lpar=ghost;inside=arguments;rpar=ghost} in
    return @@ CST.TApp (wrap (type_constant,wrap lst))
  | T_annoted _annot ->
    failwith "let's work on it later"

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
  | E_lambda _ -> ok @@ [expr]
  | _ -> failwith @@
    Format.asprintf "%a should be a tuple expression"
    AST.PP.expression expr

let pattern_type ({var;ascr}: _ AST.binder) =
  let var = CST.PVar (decompile_variable var.wrap_content) in
  match ascr with
    Some s ->
      let%bind type_expr = decompile_type_expr s in
      ok @@ CST.PTyped (wrap @@ CST.{pattern=var;colon=ghost;type_expr})
  | None -> ok @@ var

let rec decompile_expression : AST.expression -> _ result = fun expr ->
  let return_expr expr = ok @@ expr in
  let return_expr_with_par expr = return_expr @@ CST.EPar (wrap @@ par @@ expr) in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name.wrap_content in
    return_expr @@ CST.EVar (var)
  | E_literal literal ->
    (match literal with
        Literal_unit  ->  return_expr @@ CST.EUnit (wrap (ghost,ghost))
      | Literal_int i ->  return_expr @@ CST.EArith (Int (wrap ("",i)))
      | Literal_nat n ->  return_expr @@ CST.EArith (Nat (wrap ("",n)))
      | Literal_timestamp time ->
        let time = Tezos_utils.Time.Protocol.to_notation @@
          Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time in
          (* TODO combinators for CSTs. *)
        let%bind ty = decompile_type_expr @@ AST.t_timestamp () in
        let time = CST.EString (String (wrap time)) in
        return_expr @@ CST.EAnnot (wrap @@ par (time, ghost, ty))
      | Literal_mutez mtez -> return_expr @@ CST.EArith (Mutez (wrap ("",mtez)))
      | Literal_string (Standard str) -> return_expr @@ CST.EString (String   (wrap str))
      | Literal_string (Verbatim ver) -> return_expr @@ CST.EString (Verbatim (wrap ver))
      | Literal_bytes b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        return_expr @@ CST.EBytes (wrap (s,b))
      | Literal_address addr ->
        let addr = CST.EString (String (wrap addr)) in
        let%bind ty = decompile_type_expr @@ AST.t_address () in
        return_expr @@ CST.EAnnot (wrap @@ par (addr,ghost,ty))
      | Literal_signature sign ->
        let sign = CST.EString (String (wrap sign)) in
        let%bind ty = decompile_type_expr @@ AST.t_signature () in
        return_expr @@ CST.EAnnot (wrap @@ par (sign,ghost,ty))
      | Literal_key k ->
        let k = CST.EString (String (wrap k)) in
        let%bind ty = decompile_type_expr @@ AST.t_key () in
        return_expr @@ CST.EAnnot (wrap @@ par (k,ghost,ty))
      | Literal_key_hash kh ->
        let kh = CST.EString (String (wrap kh)) in
        let%bind ty = decompile_type_expr @@ AST.t_key_hash () in
        return_expr @@ CST.EAnnot (wrap @@ par (kh,ghost,ty))
      | Literal_chain_id _
      | Literal_operation _ ->
        failwith "chain_id, operation are not created currently ?"
    )
  | E_application {lamb;args} ->
    let%bind lamb = decompile_expression lamb in
    let%bind args = map List.Ne.of_list @@
      bind (bind_map_list decompile_expression) @@
      get_e_tuple args
    in
    return_expr @@ CST.ECall (wrap (lamb,args))
  | E_lambda lambda ->
    let%bind (binders,_lhs_type,_block_with,body) = decompile_lambda lambda in
    let fun_expr : CST.fun_expr = {kwd_fun=ghost;binders;lhs_type=None;arrow=ghost;body} in
    return_expr_with_par @@ CST.EFun (wrap @@ fun_expr)
  | E_recursive _ ->
    failwith "corner case : annonymous recursive function"
  | E_let_in {let_binder={var;ascr};rhs;let_result;attributes} ->
    let var = CST.PVar (decompile_variable @@ var.wrap_content) in
    let binders = (var,[]) in
    let%bind lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) @@ ascr in
    let%bind let_rhs = decompile_expression rhs in
    let binding : CST.let_binding = {binders;lhs_type;eq=ghost;let_rhs} in
    let%bind body = decompile_expression let_result in
    let attributes = decompile_attributes attributes in
    let lin : CST.let_in = {kwd_let=ghost;kwd_rec=None;binding;kwd_in=ghost;body;attributes} in
    return_expr @@ CST.ELetIn (wrap lin)
  | E_raw_code {language; code} ->
    let language = wrap @@ wrap @@ language in
    let%bind code = decompile_expression code in
    let ci : CST.code_inj = {language;code;rbracket=ghost} in
    return_expr @@ CST.ECodeInj (wrap ci)
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = wrap constr in
    let%bind element = decompile_expression element in
    return_expr_with_par @@ CST.EConstr (EConstrApp (wrap (constr, Some element)))
  | E_matching {matchee; cases} ->
    let%bind expr  = decompile_expression matchee in
    let%bind cases = decompile_matching_cases cases in
    let cases : _ CST.case = {kwd_match=ghost;expr;kwd_with=ghost;lead_vbar=None;cases} in
    return_expr @@ CST.ECase (wrap cases)
  | E_record record  ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label str, expr) =
      let field_name = wrap str in
      let%bind field_expr = decompile_expression expr in
      let field : CST.field_assign = {field_name;assignment=ghost;field_expr} in
      ok @@ wrap field
    in
    let%bind record = bind_map_list aux record in
    let%bind record = list_to_nsepseq record in
    let record = ne_inject braces record ~attr:[] in
    (* why is the record not empty ? *)
    return_expr @@ CST.ERecord (wrap record)
  | E_accessor {record; path} ->
    (match List.rev path with
      Access_map e :: [] ->
      let%bind map = decompile_expression record in
      let%bind e = decompile_expression e in
      let arg = e,[map] in
      return_expr @@ CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))
    | Access_map e :: lst ->
      let path = List.rev lst in
      let%bind field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection path in
      let%bind struct_name = map (decompile_variable) @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=ghost;field_path} in
      let%bind e = decompile_expression e in
      let arg = e,[CST.EProj (wrap proj)] in
      return_expr @@ CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))
    | _ ->
      let%bind field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection path in
       let%bind struct_name = map (decompile_variable) @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=ghost;field_path} in
      return_expr @@ CST.EProj (wrap proj)
    )
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {record={expression_content=E_update _;_} as record;path;update} ->
    let%bind record = decompile_expression record in
    let%bind (record,updates) = match record with
      CST.EUpdate {value;_} -> ok @@ (value.record,value.updates)
    | _ -> failwith @@ Format.asprintf "Inpossible case %a" AST.PP.expression expr
    in
    let%bind var,path = match path with
      Access_record var::path -> ok @@ (var,path)
    | _ -> failwith "Impossible case %a"
    in
    let%bind field_path = decompile_to_path (Location.wrap @@ Var.of_name var) path in
    let%bind field_expr = decompile_expression update in
    let field_assign : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
    let updates = updates.value.ne_elements in
    let updates = wrap @@ ne_inject ~attr:[] braces @@ npseq_cons (wrap @@ field_assign) updates in
    let update : CST.update = {lbrace=ghost;record;kwd_with=ghost;updates;rbrace=ghost} in
    return_expr @@ CST.EUpdate (wrap @@ update)
  | E_update {record; path; update} ->
    let%bind record = map (decompile_variable) @@ get_e_variable record in
    let%bind field_expr = decompile_expression update in
    let (struct_name,field_path) = List.Ne.of_list path in
    (match field_path with
      [] ->
      (match struct_name with
        Access_record name ->
        let record : CST.path = Name record in
        let field_path = CST.Name (wrap name) in
        let update : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=ghost;record;kwd_with=ghost;updates;rbrace=ghost} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_tuple i ->
        let record : CST.path = Name record in
        let field_path = CST.Name (wrap @@ Z.to_string i) in
        let update : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=ghost;record;kwd_with=ghost;updates;rbrace=ghost} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_map e ->
        let%bind e = decompile_expression e in
        let arg = field_expr,[e; CST.EVar record] in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"), arg))
      )
    | _ ->
      let%bind struct_name = match struct_name with
          Access_record name -> ok @@ wrap name
        | Access_tuple i -> ok @@ wrap @@ Z.to_string i
        | Access_map _ -> failwith @@ Format.asprintf "invalid map update %a" AST.PP.expression expr
      in
      (match List.rev field_path with
        Access_map e :: lst ->
        let field_path = List.rev lst in
        let%bind field_path = bind_map_list decompile_to_selection field_path in
        let%bind field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=ghost;field_path} in
        let field_path = CST.EProj (wrap @@ field_path) in
        let%bind e = decompile_expression e in
        let arg = field_expr, [e; field_path] in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"),arg))
      | _ ->
        let%bind field_path = bind_map_list decompile_to_selection field_path in
        let%bind field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=ghost;field_path} in
        let field_path = CST.Path (wrap @@ field_path) in
        let record : CST.path = Name record in
        let update : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] braces @@ (wrap update,[]) in
        let update : CST.update = {lbrace=ghost;record;kwd_with=ghost;updates;rbrace=ghost} in
        return_expr @@ CST.EUpdate (wrap update)
      )
    )
  | E_ascription {anno_expr;type_annotation} ->
    let%bind expr = decompile_expression anno_expr in
    let%bind ty   = decompile_type_expr type_annotation in
    return_expr @@ CST.EAnnot (wrap @@ par (expr,ghost,ty))
  | E_cond {condition;then_clause;else_clause} ->
    let%bind test  = decompile_expression condition in
    let%bind ifso  = decompile_expression then_clause in
    let%bind ifnot = decompile_expression else_clause in
    let ifnot = Some(ghost,ifnot) in
    let cond : CST.cond_expr = {kwd_if=ghost;test;kwd_then=ghost;ifso;ifnot} in
    return_expr @@ CST.ECond (wrap cond)
  | E_sequence {expr1;expr2} ->
    let%bind expr1 = decompile_expression expr1 in
    let%bind expr2 = decompile_expression expr2 in
    return_expr @@ CST.ESeq (wrap @@ inject beginEnd @@ list_to_sepseq [expr1; expr2])
  | E_tuple tuple ->
    let%bind tuple = bind_map_list decompile_expression tuple in
    let%bind tuple = list_to_nsepseq tuple in
    return_expr @@ CST.ETuple (wrap @@ tuple)
  | E_map map ->
    let%bind map = bind_map_list (bind_map_pair decompile_expression) map in
    let aux (k,v) = CST.ETuple (wrap (k,[(ghost,v)])) in
    let map = List.map aux map in
    (match map with
      [] -> return_expr @@ CST.EVar (wrap "Big_map.empty")
    | _  ->
      let var = CST.EVar (wrap "Map.literal") in
      return_expr @@ CST.ECall (wrap @@ (var, List.Ne.of_list @@ map))
    )
  | E_big_map big_map ->
    let%bind big_map = bind_map_list (bind_map_pair decompile_expression) big_map in
    let aux (k,v) = CST.ETuple (wrap (k,[(ghost,v)])) in
    let big_map = List.map aux big_map in
    (match big_map with
      [] -> return_expr @@ CST.EVar (wrap "Big_map.empty")
    | _  ->
      let var = CST.EVar (wrap "Big_map.literal") in
      return_expr @@ CST.ECall (wrap @@ (var, List.Ne.of_list @@ big_map))
    )
  | E_list lst ->
    let%bind lst = bind_map_list decompile_expression lst in
    let lst = list_to_sepseq lst in
    return_expr @@ CST.EList (EListComp (wrap @@ inject brackets @@ lst))
  | E_set set ->
    let%bind set = bind_map_list decompile_expression set in
    let set = List.Ne.of_list @@ set in
    let var = CST.EVar (wrap "Set.literal") in
    return_expr @@ CST.ECall (wrap @@ (var,set))
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
    let%bind field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection lst in
    let path : CST.projection = {struct_name;selector=ghost;field_path} in
    ok @@ (CST.Path (wrap @@ path) : CST.path)

and decompile_to_selection : _ AST.access -> (CST.selection, _) result = fun access ->
  match access with
    Access_tuple index -> ok @@ CST.Component (wrap @@ ("",index))
  | Access_record str  -> ok @@ CST.FieldName (wrap str)
  | Access_map _ ->
    failwith @@ Format.asprintf
    "Can't decompile access_map to selection"

and decompile_lambda : (AST.expr,AST.ty_expr) AST.lambda -> _ = fun {binder;output_type;result} ->
    let%bind param_decl = pattern_type binder in
    let param = (param_decl, []) in
    let%bind ret_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) output_type in
    let%bind result = decompile_expression result in
    ok @@ (param,ret_type,None,result)

and decompile_matching_cases : AST.matching_expr -> ((CST.expr CST.case_clause Region.reg, Region.t) Simple_utils.Utils.nsepseq Region.reg,_) result =
fun m ->
  let%bind cases = match m with
    Match_variable (binder, expr) ->
    let%bind pattern = pattern_type binder in
    let%bind rhs = decompile_expression expr in
    let case : _ CST.case_clause = {pattern; arrow=ghost; rhs}in
    ok @@ [wrap case]
  | Match_tuple (lst, expr) ->
    let%bind tuple = bind list_to_nsepseq @@ bind_map_list pattern_type lst in
    let pattern : CST.pattern = PTuple (wrap @@ tuple) in
    let%bind rhs = decompile_expression expr in
    let case : _ CST.case_clause = {pattern; arrow=ghost; rhs}in
    ok @@ [wrap case]
  | Match_record _ -> failwith "match_record not availiable yet"
  | Match_option {match_none;match_some}->
    let%bind rhs = decompile_expression match_none in
    let none_case : _ CST.case_clause = {pattern=PConstr (PNone ghost);arrow=ghost; rhs} in
    let%bind rhs = decompile_expression @@ snd match_some in
    let var = CST.PVar (decompile_variable @@ (fst match_some).wrap_content)in
    let some_case : _ CST.case_clause = {pattern=PConstr (PSomeApp (wrap (ghost,var)));arrow=ghost; rhs} in
    ok @@ [wrap some_case;wrap none_case]
  | Match_list {match_nil; match_cons} ->
    let (hd,tl,expr) = match_cons in
    let hd = CST.PVar (decompile_variable hd.wrap_content) in
    let tl = CST.PVar (decompile_variable tl.wrap_content) in
    let cons = (hd,ghost,tl) in
    let%bind rhs = decompile_expression @@ expr in
    let cons_case : _ CST.case_clause = {pattern=PList (PCons (wrap cons));arrow=ghost; rhs} in
    let%bind rhs = decompile_expression @@ match_nil in
    let nil_case : _ CST.case_clause = {pattern=PList (PListComp (wrap @@ inject brackets None));arrow=ghost; rhs} in
    ok @@ [wrap cons_case; wrap nil_case]
  | Match_variant lst ->
    let aux ((c,(v:AST.expression_variable)),e) =
      let AST.Label c = c in
      let constr = wrap @@ c in
      let var : CST.pattern = PVar (decompile_variable v.wrap_content) in
      let tuple = var in
      let pattern : CST.pattern = PConstr (PConstrApp (wrap (constr, Some tuple))) in
      let%bind rhs = decompile_expression e in
      let case : _ CST.case_clause = {pattern;arrow=ghost;rhs} in
      ok @@ wrap case
    in
    bind_map_list aux lst
  in
  map wrap @@ list_to_nsepseq cases
let decompile_declaration : AST.declaration Location.wrap -> (CST.declaration, _) result = fun decl ->
  let decl = Location.unwrap decl in
  let wrap value = ({value;region=Region.ghost} : _ Region.reg) in
  match decl with
    Declaration_type {type_binder;type_expr} ->
    let name = decompile_variable type_binder in
    let%bind type_expr = decompile_type_expr type_expr in
    ok @@ CST.TypeDecl (wrap (CST.{kwd_type=ghost; name; eq=ghost; type_expr}))
  | Declaration_constant {binder;attr;expr}->
    let attributes : CST.attributes = decompile_attributes attr in
    let var = CST.PVar (decompile_variable binder.var.wrap_content) in
    let binders = (var,[]) in
    let%bind lhs_type = bind_map_option (bind_compose (ok <@ prefix_colon) decompile_type_expr) binder.ascr in
    match expr.expression_content with
      E_lambda lambda ->
      let%bind let_rhs = decompile_expression @@ AST.make_e @@ AST.E_lambda lambda in
      let let_binding : CST.let_binding = {binders;lhs_type;eq=ghost;let_rhs} in
      let let_decl : CST.let_decl = wrap (ghost,None,let_binding,attributes) in
      ok @@ CST.Let let_decl
    | E_recursive {lambda; _} ->
      let%bind let_rhs = decompile_expression @@ AST.make_e @@ AST.E_lambda lambda in
      let let_binding : CST.let_binding = {binders;lhs_type;eq=ghost;let_rhs} in
      let let_decl : CST.let_decl = wrap (ghost,Some ghost,let_binding,attributes) in
      ok @@ CST.Let (let_decl)
    | _ ->
      let%bind let_rhs = decompile_expression expr in
      let let_binding : CST.let_binding = {binders;lhs_type;eq=ghost;let_rhs} in
      let let_decl : CST.let_decl = wrap (ghost,None,let_binding,attributes) in
      ok @@ CST.Let let_decl

let decompile_program : AST.program -> (CST.ast, _) result = fun prg ->
  let%bind decl = bind_map_list decompile_declaration prg in
  let decl = List.Ne.of_list decl in
  ok @@ ({decl;eof=ghost}: CST.ast)
