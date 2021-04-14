module AST = Ast_imperative
module CST = Cst.Pascaligo
module Predefined = Predefined.Tree_abstraction.Pascaligo

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
  | None   -> failwith "List is not a non_empty list"
let nelist_to_npseq (hd, lst) = (hd, List.map (fun e -> (ghost, e)) lst)
let npseq_cons hd lst = hd,(ghost, fst lst)::(snd lst)

let par a = CST.{lpar=ghost;inside=a;rpar=ghost}
let braces a = CST.{lbrace=ghost;inside=a;rbrace=ghost}
let brackets a = CST.{lbracket=ghost;inside=a;rbracket=ghost}
let prefix_colon a = (ghost, a)
let suffix_with a = (a, ghost)

(* Dialect-relevant functions *)
type dialect = Terse | Verbose

let terminator = function
  | Terse -> Some ghost
  | Verbose -> None

let lead_vbar = terminator

let enclosing = function
  | Terse -> CST.Brackets (ghost,ghost)
  | Verbose -> CST.End ghost

let block_enclosing = function
  | Terse -> CST.Block (ghost,ghost,ghost)
  | Verbose -> CST.BeginEnd (ghost,ghost)

let module_enclosing = function
  | Terse -> CST.Brace (ghost,ghost)
  | Verbose -> CST.BeginEnd (ghost,ghost)

let inject dialect kind a =
  CST.{kind;enclosing=enclosing dialect;elements=a;terminator=terminator dialect}

let ne_inject dialect kind a ~attr = CST.{
  kind;
  enclosing=enclosing dialect;
  ne_elements=a;
  terminator=terminator dialect;
  attributes=attr
  }

let to_block dialect a =
  CST.{enclosing=block_enclosing dialect;statements=a;terminator=terminator dialect}

let empty_block dialect =
  to_block dialect (CST.Instr (CST.Skip ghost),[])

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
let rec decompile_type_expr : dialect -> AST.type_expression -> _ result = fun dialect te ->
  let return te = ok @@ te in
  match te.type_content with
    T_sum {attributes ; fields } ->
    let attributes = decompile_attributes attributes in
    let lst = AST.LMap.to_kv_list fields in
    let aux (AST.Label c, AST.{associated_type; attributes=row_attr; _}) =
      let constr = wrap c in
      let%bind arg = decompile_type_expr dialect associated_type in
      let arg = Some (ghost, arg) in
      let row_attr = decompile_attributes row_attr in
      let variant : CST.variant = {constr; arg; attributes=row_attr} in
      ok @@ wrap variant in
    let%bind variants = bind_map_list aux lst in
    let%bind variants = list_to_nsepseq variants in
    let lead_vbar = Some ghost in
    let sum : CST.sum_type = { lead_vbar ; variants ; attributes}in
    return @@ CST.TSum (wrap sum)
  | T_record {fields; attributes} ->
     let record = AST.LMap.to_kv_list fields in
     let aux (AST.Label c, AST.{associated_type; attributes=field_attr; _}) =
       let field_name = wrap c in
       let colon = ghost in
       let%bind field_type = decompile_type_expr dialect associated_type in
      let field_attr = decompile_attributes field_attr in
       let field : CST.field_decl =
         {field_name; colon; field_type; attributes=field_attr} in
       ok @@ wrap field in
    let%bind record = bind_map_list aux record in
    let%bind record = list_to_nsepseq record in
    let attributes = decompile_attributes attributes in
    return @@ CST.TRecord (wrap @@ ne_inject ~attr:attributes dialect (NEInjRecord ghost) record)
  | T_tuple tuple ->
    let%bind tuple = bind_map_list (decompile_type_expr dialect) tuple in
    let%bind tuple = list_to_nsepseq @@ tuple in
    return @@ CST.TProd (wrap tuple)
  | T_arrow {type1;type2} ->
    let%bind type1 = decompile_type_expr dialect type1 in
    let%bind type2 = decompile_type_expr dialect type2 in
    let arrow = (type1, ghost, type2) in
    return @@ CST.TFun (wrap arrow)
  | T_variable variable ->
    let v = decompile_variable variable in
    return @@ CST.TVar v
  | T_app {type_operator; arguments} ->
    let v = decompile_variable type_operator in
    let%bind lst = bind_map_list (decompile_type_expr dialect) arguments in
    let%bind lst = list_to_nsepseq lst in
    let lst : _ CST.par = {lpar=ghost;inside=lst;rpar=ghost} in
    return @@ CST.TApp (wrap (v,wrap lst))
  | T_annoted _annot ->
    failwith "TODO: decompile T_annoted"
  | T_module_accessor {module_name;element} ->
    let module_name = wrap module_name in
    let%bind field  = decompile_type_expr dialect element in
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

let rec get_e_accessor : AST.expression -> _ result = fun expr ->
  match expr.expression_content with
    E_variable var -> ok @@ (var, [])
  | E_accessor {record;path} ->
    let%bind (var, lst) = get_e_accessor record in
    ok @@ (var, lst @ path)
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
type eos =
| Expression
| Statements

type state = Cst_pascaligo.Printer.state

let statements_of_expression : CST.expr -> CST.statement List.Ne.t option = fun stat ->
  match stat with
  | CST.ECall call -> Some (CST.Instr (CST.ProcCall call), [])
  | _ -> None

let rec decompile_expression ?(dialect=Verbose) : AST.expression -> _ result = fun e ->
  let%bind (block,expr) = decompile_to_block dialect e in
  match expr with
    Some expr ->
    ( match block with
      Some block ->
        let block = wrap @@ block in
        ok @@ CST.EBlock (wrap @@ CST.{block;kwd_with=ghost;expr})
    | None -> ok @@ expr
    )
  | None ->
    failwith @@ Format.asprintf
      "An expression was expected, but this was decompile to statements. \n
      Expr : %a
      Loc : %a"
      AST.PP.expression e
      Location.pp e.location

and decompile_statements : dialect -> AST.expression -> _ result = fun dialect expr ->
  let%bind (stat,_) = decompile_eos dialect Statements expr in
  match stat with
    Some stat -> ok @@ stat
  | None ->
      failwith @@ Format.asprintf
        "Statements was expected, but this was decompile to expression. \n
        Expr : %a
        Loc : %a"
        AST.PP.expression expr
        Location.pp expr.location

and decompile_pattern : dialect -> AST.type_expression AST.pattern -> (CST.pattern,_) result =
  fun dialect pattern ->
    match pattern.wrap_content with
    | AST.P_unit -> ok @@ CST.PConstr (PUnit (ghost))
    | AST.P_var v ->
      let name = (decompile_variable v.var.wrap_content).value in
      ok @@ CST.PVar (wrap name)
    | AST.P_list pl -> (
      let ret x = ok (CST.PList x) in
      match pl with
      | AST.Cons (pa,pb) ->
        let%bind plst = bind_map_list (decompile_pattern dialect) [pa;pb] in
        let%bind plst' = list_to_nsepseq plst in
        let cons = wrap plst' in
        ret (PCons cons)
      | AST.List [] ->
        ret (PNil ghost)
      | AST.List plst ->
        let%bind plst = bind_map_list (decompile_pattern dialect) plst in
        let elements = list_to_sepseq plst in
        let inj = inject dialect (CST.InjList ghost) elements in
        ret (CST.PListComp (wrap inj))
    )
    | AST.P_variant (constructor,popt) -> (
      match constructor with
      | Label "Some" ->
        let p = Option.unopt_exn popt in
        let%bind p = decompile_pattern dialect p in
        ok @@ CST.PConstr (PSomeApp (wrap (ghost, p)))
      | Label "None" -> ok @@ CST.PConstr (PNone ghost)
      | Label "true" -> ok @@ CST.PConstr (PTrue ghost)
      | Label "false" -> ok @@ CST.PConstr (PFalse ghost)
      | Label constructor -> (
        match popt with
        | Some p ->
          let%bind p = decompile_pattern dialect p in
          let%bind p = list_to_nsepseq [p] in
          let p = wrap (par p) in
          let constr = wrap (wrap constructor, Some p) in
          ok @@ CST.PConstr (PConstrApp constr)
        | None ->
          let constr = wrap (wrap constructor, None) in
          ok @@ CST.PConstr (PConstrApp constr)
      )
    )
    | AST.P_tuple lst ->
      let%bind pl = bind_map_list (decompile_pattern dialect) lst in
      let%bind pl = list_to_nsepseq pl in
      ok @@ CST.PTuple (wrap (par pl))
    | AST.P_record (labels, patterns) ->
      let aux : AST.label * AST.type_expression AST.pattern -> (CST.field_pattern CST.reg, _) result =
        fun (Label label, pattern) ->
          let%bind pattern = decompile_pattern dialect pattern in
          let field_name = wrap label in
          ok @@ wrap ({ field_name ; eq = ghost ; pattern } : CST.field_pattern)
      in
      let%bind field_patterns = bind_map_list aux (List.combine labels patterns) in
      let inj = inject dialect (InjRecord ghost) (list_to_sepseq field_patterns) in
      ok @@ CST.PRecord (wrap inj)

and decompile_to_block : dialect -> AST.expression -> _ result = fun dialect expr ->
  let%bind (stats,next) = decompile_eos dialect Expression expr in
  let block = Option.map (to_block dialect <@ nelist_to_npseq) stats in
  ok @@ (block, next)

and decompile_to_tuple_expr : dialect -> AST.expression list -> (CST.tuple_expr,_) result = fun dialect expr ->
  let%bind tuple_expr = bind_map_list (decompile_expression ~dialect) expr in
  let%bind tuple_expr = list_to_nsepseq tuple_expr in
  let tuple_expr : CST.tuple_expr = wrap @@ par @@  tuple_expr in
  ok @@ tuple_expr

and decompile_eos : dialect -> eos -> AST.expression -> ((CST.statement List.Ne.t option)* (CST.expr option), _) result = fun dialect output expr ->
  let return (a,b) = ok @@ (a,b) in
  let return_expr expr = return @@ (None, Some expr) in
  let return_expr_with_par expr = return_expr @@ CST.EPar (wrap @@ par @@ expr) in
  let return_stat stat = return @@ (Some stat, None) in
  let return_stat_ez stat = return_stat @@ (stat, []) in
  let return_inst inst = return_stat_ez @@ CST.Instr inst in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name.wrap_content in
    return_expr @@ CST.EVar (var)
  | E_constant {cons_name; arguments} ->
    let expr = CST.EVar (wrap @@ Predefined.constant_to_string cons_name) in
    (match arguments with
      [] -> return_expr @@ expr
    | _ ->
      let%bind arguments = decompile_to_tuple_expr dialect arguments in
      let const : CST.fun_call = wrap (expr, arguments) in
      (match output with
        Expression -> return_expr (CST.ECall const)
      | Statements -> return_inst (CST.ProcCall const)
      )
    )
  | E_literal literal ->
    (match literal with
        Literal_unit  ->  return_expr @@ CST.EUnit ghost
      | Literal_int i ->  return_expr @@ CST.EArith (Int (wrap ("",i)))
      | Literal_nat n ->  return_expr @@ CST.EArith (Nat (wrap ("",n)))
      | Literal_timestamp time ->
        let time = Tezos_utils.Time.Protocol.to_notation @@
          Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time in
          (* TODO combinators for CSTs. *)
        let%bind ty = decompile_type_expr dialect @@ AST.t_timestamp () in
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
        let%bind ty = decompile_type_expr dialect @@ AST.t_address () in
        return_expr @@ CST.EAnnot (wrap @@ par (addr,ghost,ty))
      | Literal_signature sign ->
        let sign = CST.EString (String (wrap sign)) in
        let%bind ty = decompile_type_expr dialect @@ AST.t_signature () in
        return_expr @@ CST.EAnnot (wrap @@ par (sign,ghost,ty))
      | Literal_key k ->
        let k = CST.EString (String (wrap k)) in
        let%bind ty = decompile_type_expr dialect @@ AST.t_key () in
        return_expr @@ CST.EAnnot (wrap @@ par (k,ghost,ty))
      | Literal_key_hash kh ->
        let kh = CST.EString (String (wrap kh)) in
        let%bind ty = decompile_type_expr dialect @@ AST.t_key_hash () in
        return_expr @@ CST.EAnnot (wrap @@ par (kh,ghost,ty))
      | Literal_chain_id _
      | Literal_operation _ ->
        failwith "chain_id, operation are not created currently ?"
    )
  | E_application {lamb;args} ->
    let%bind lamb = decompile_expression ~dialect lamb in
    let%bind args = bind (decompile_to_tuple_expr dialect) @@ get_e_tuple args in
    (match output with
      Expression ->
      return_expr @@ CST.ECall (wrap (lamb,args))
    | Statements ->
      return_inst @@ CST.ProcCall (wrap (lamb,args))
    )
  | E_lambda lambda ->
    let%bind (param,ret_type,return) = decompile_lambda dialect lambda in
    let fun_expr : CST.fun_expr = {kwd_function=ghost;param;ret_type;kwd_is=ghost;return} in
    return_expr_with_par @@ CST.EFun (wrap @@ fun_expr)
  | E_recursive _ ->
    failwith "corner case : annonymous recursive function"
  | E_let_in {let_binder={var;_};rhs={expression_content=E_update {record={expression_content=E_variable v;_};path;update};_};let_result;attributes=_}
      when Var.equal var.wrap_content v.wrap_content ->
    let%bind lhs = (match List.rev path with
      Access_map e :: path ->
      let%bind path  = decompile_to_path var @@ List.rev path in
      let%bind index = map (wrap <@ brackets) @@ decompile_expression ~dialect e in
      let mlu : CST.map_lookup = {path; index} in
      ok @@ CST.MapPath (wrap @@ mlu)
    | _ ->
      let%bind path  = decompile_to_path var @@ path in
      ok @@ (CST.Path (path) : CST.lhs)
    )
    in
    let%bind rhs = decompile_expression ~dialect update in
    let assign : CST.assignment = {lhs;assign=ghost;rhs} in
    let assign = CST.Instr (CST.Assign (wrap @@ assign)) in
    let%bind (stat,expr) = decompile_eos dialect output let_result in
    let stat = (match stat with
      Some (stat) -> Some (List.Ne.cons assign stat)
    | None -> Some (assign,[])
    )
    in
    return @@ (stat,expr)
  | E_let_in {let_binder;rhs;let_result;attributes} ->
    let%bind lin = decompile_to_data_decl dialect let_binder rhs attributes in
    let%bind (lst, expr) = decompile_eos dialect Expression let_result in
    let lst = match lst with
      Some lst -> List.Ne.cons (CST.Data lin) lst
    | None -> (CST.Data lin, [])
    in
    return @@ (Some lst, expr)
  | E_type_in {type_binder;rhs;let_result} ->
    let kwd_type = Region.ghost
    and name = decompile_variable type_binder
    and kwd_is = Region.ghost in
    let%bind type_expr = decompile_type_expr dialect rhs in
    let terminator = terminator dialect in
    let tin = wrap @@ (CST.{kwd_type; name; kwd_is; type_expr; terminator}) in
    let%bind (lst, expr) = decompile_eos dialect Expression let_result in
    let lst = match lst with
      Some lst -> List.Ne.cons (CST.Data (CST.LocalType tin)) lst
    | None -> (CST.Data (CST.LocalType tin), [])
    in
    return @@ (Some lst, expr)
  | E_mod_in {module_binder;rhs;let_result} ->
    let kwd_module = Region.ghost
    and name = wrap module_binder
    and kwd_is = Region.ghost in
    let%bind module_ = decompile_module ~dialect rhs in
    let terminator = terminator dialect in
    let enclosing  = module_enclosing dialect in
    let min = wrap @@ (CST.{kwd_module; name; kwd_is;enclosing; module_; terminator}) in
    let%bind (lst, expr) = decompile_eos dialect Expression let_result in
    let lst = match lst with
      Some lst -> List.Ne.cons (CST.Data (CST.LocalModule min)) lst
    | None -> (CST.Data (CST.LocalModule min), [])
    in
    return @@ (Some lst, expr)
  | E_mod_alias {alias; binders; result} ->
    let alias   = wrap alias in
    let binders = nelist_to_npseq @@ List.Ne.map wrap binders in
    let terminator = terminator dialect in
    let ma = wrap @@ CST.{kwd_module=ghost;alias;kwd_is=ghost;binders;terminator} in
    let%bind (lst, expr) = decompile_eos dialect Expression result in
    let lst = match lst with
      Some lst -> List.Ne.cons (CST.Data (CST.LocalModuleAlias ma)) lst
    | None -> (CST.Data (CST.LocalModuleAlias ma), [])
    in
    return @@ (Some lst, expr)
  | E_raw_code {language; code} ->
    let language = wrap @@ wrap @@ language in
    let%bind code = decompile_expression ~dialect code in
    let ci : CST.code_inj = {language;code;rbracket=ghost} in
    return_expr @@ CST.ECodeInj (wrap ci)
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = wrap constr in
    let%bind element = bind (decompile_to_tuple_expr dialect) @@ get_e_tuple element in
    return_expr_with_par @@ CST.EConstr (ConstrApp (wrap (constr, Some element)))
  | E_matching {matchee; cases} -> (
    let%bind expr  = decompile_expression ~dialect matchee in
    let enclosing = enclosing dialect in
    let lead_vbar = lead_vbar dialect in
    let aux decompile_f =
      fun ({ pattern ; body }:(AST.expression, AST.type_expression) AST.match_case) ->
        let%bind pattern = decompile_pattern dialect pattern in
        let%bind rhs = decompile_f body in
        let clause : (_ CST.case_clause)= { pattern ; arrow = ghost ; rhs } in
        ok (wrap clause)
    in
    match output with
    | Expression ->
      let%bind cases = bind_map_list (aux (decompile_expression ~dialect)) cases in
      let%bind cases = list_to_nsepseq cases in 
      let cases : _ CST.case = {kwd_case=ghost;expr;kwd_of=ghost;enclosing;lead_vbar;cases = wrap cases} in
      return_expr @@ CST.ECase (wrap cases)
    | Statements ->
      let%bind cases = bind_map_list (aux (decompile_if_clause dialect)) cases in
      let%bind cases = list_to_nsepseq cases in 
      let cases : _ CST.case = {kwd_case=ghost;expr;kwd_of=ghost;enclosing;lead_vbar;cases = wrap cases} in
      return_inst @@ CST.CaseInstr (wrap cases)
  )
  | E_record record  ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label str, expr) =
      let field_name = wrap str in
      let%bind field_expr = decompile_expression ~dialect expr in
      let field : CST.field_assignment = {field_name;assignment=ghost;field_expr} in
      ok @@ wrap field
    in
    let%bind record = bind_map_list aux record in
    let%bind record = list_to_nsepseq record in
    let record = ne_inject ~attr:[] dialect (NEInjRecord ghost) record in
    (* why is the record not empty ? *)
    return_expr @@ CST.ERecord (wrap record)
  | E_accessor {record; path} ->
    (match List.rev path with
      Access_map e :: [] ->
      let%bind (var,lst) = get_e_accessor @@ record in
      let%bind path = decompile_to_path var lst in
      let%bind e = decompile_expression ~dialect e in
      let index = wrap @@ brackets @@ e in
      let mlu : CST.map_lookup = {path;index} in
      return_expr @@ CST.EMap(MapLookUp (wrap @@ mlu))
    | Access_map e :: lst ->
      let path = List.rev lst in
      let%bind field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection path in
      let%bind struct_name = map (decompile_variable) @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=ghost;field_path} in
      let path : CST.path = CST.Path (wrap proj) in
      let%bind e = decompile_expression ~dialect e in
      let index = wrap @@ brackets @@ e in
      let mlu : CST.map_lookup = {path;index} in
      return_expr @@ CST.EMap(MapLookUp (wrap @@ mlu))
    | _ ->
      let%bind field_path = bind list_to_nsepseq @@ bind_map_list decompile_to_selection path in
       let%bind struct_name = map (decompile_variable) @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=ghost;field_path} in
      return_expr @@ CST.EProj (wrap proj)
    )
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {record={expression_content=E_update _;_} as record;path;update} ->
    let%bind record = decompile_expression ~dialect record in
    let%bind (record,updates) = match record with
      CST.EUpdate {value;_} -> ok @@ (value.record,value.updates)
    | _ -> failwith @@ Format.asprintf "Inpossible case %a" AST.PP.expression expr
    in
    let%bind var,path = match path with
      Access_record var::path -> ok @@ (var,path)
    | _ -> failwith "Impossible case %a"
    in
    let%bind field_path = decompile_to_path (Location.wrap @@ Var.of_name var) path in
    let%bind field_expr = decompile_expression ~dialect update in
    let field_assign : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
    let updates = updates.value.ne_elements in
    let updates = wrap @@ ne_inject ~attr:[] dialect (NEInjRecord ghost)
                  @@ npseq_cons (wrap @@ field_assign) updates in
    let update : CST.update = {record;kwd_with=ghost;updates} in
    return_expr @@ CST.EUpdate (wrap @@ update)
  | E_update {record; path; update} ->
    let%bind record = map (decompile_variable) @@ get_e_variable record in
    let%bind field_expr = decompile_expression ~dialect update in
    let (struct_name,field_path) = List.Ne.of_list path in
    (match field_path with
      [] ->
      (match struct_name with
        Access_record name ->
        let record : CST.path = Name record in
        let field_path = CST.Name (wrap name) in
        let update : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] dialect (NEInjRecord ghost) @@ (wrap update,[]) in
        let update : CST.update = {record;kwd_with=ghost;updates;} in
        return_expr @@ CST.EUpdate (wrap update)
      | Access_tuple _ -> failwith @@ Format.asprintf "invalid tuple update %a" AST.PP.expression expr
      | Access_map e ->
        let%bind e = decompile_expression ~dialect e in
        let arg : CST.tuple_expr = wrap @@ par @@ nelist_to_npseq (field_expr,[e; CST.EVar record]) in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"), arg))
      )
    | _ ->
      let%bind struct_name = match struct_name with
          Access_record name -> ok @@ wrap name
        | Access_tuple _ -> failwith @@ Format.asprintf "invalid tuple update %a" AST.PP.expression expr
        | Access_map _ -> failwith @@ Format.asprintf "invalid map update %a" AST.PP.expression expr
      in
      (match List.rev field_path with
        Access_map e :: lst ->
        let field_path = List.rev lst in
        let%bind field_path = bind_map_list decompile_to_selection field_path in
        let%bind field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=ghost;field_path} in
        let field_path = CST.EProj (wrap @@ field_path) in
        let%bind e = decompile_expression ~dialect e in
        let arg = wrap @@ par @@ nelist_to_npseq (field_expr, [e; field_path]) in
        return_expr @@ CST.ECall (wrap (CST.EVar (wrap "Map.add"),arg))
      | _ ->
        let%bind field_path = bind_map_list decompile_to_selection field_path in
        let%bind field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=ghost;field_path} in
        let field_path : CST.path = CST.Path (wrap @@ field_path) in
        let record : CST.path = Name record in
        let update : CST.field_path_assignment = {field_path;assignment=ghost;field_expr} in
        let updates = wrap @@ ne_inject ~attr:[] dialect (NEInjRecord ghost) @@ (wrap update,[]) in
        let update : CST.update = {record;kwd_with=ghost;updates;} in
        return_expr @@ CST.EUpdate (wrap update)
      )
    )
  | E_ascription {anno_expr;type_annotation} ->
    let%bind expr = decompile_expression ~dialect anno_expr in
    let%bind ty   = decompile_type_expr dialect type_annotation in
    return_expr @@ CST.EAnnot (wrap @@ par (expr,ghost,ty))
  | E_module_accessor {module_name;element} ->
    let module_name = wrap module_name in
    let%bind field  = decompile_expression element in
    return_expr @@ CST.EModA (wrap CST.{module_name;selector=ghost;field})
  | E_cond {condition;then_clause;else_clause} ->
     let%bind test  = decompile_expression ~dialect condition in
     let terminator = terminator dialect in
    (match output with
      Expression ->
      let%bind ifso = decompile_expression ~dialect then_clause in
      let%bind ifnot = decompile_expression ~dialect else_clause in
      let cond : CST.cond_expr = {kwd_if=ghost;test;kwd_then=ghost;ifso;terminator;kwd_else=ghost;ifnot} in
      return_expr @@ CST.ECond (wrap cond)
    | Statements ->
      let%bind ifso  = decompile_if_clause dialect then_clause in
      let%bind ifnot = decompile_if_clause dialect else_clause in
      let cond : CST.conditional = {kwd_if=ghost;test;kwd_then=ghost;ifso;terminator; kwd_else=ghost;ifnot} in
      return_inst @@ CST.Cond (wrap cond)
    )
  | E_sequence {expr1;expr2} ->
    let%bind expr1 = decompile_statements dialect expr1 in
    let%bind (expr2,next) = decompile_eos dialect Statements expr2 in
    let expr1 = Option.unopt ~default:expr1 @@ Option.map (List.Ne.append expr1) expr2 in
    return @@ (Some expr1, next)
  | E_skip -> return_inst @@ CST.Skip ghost
  | E_tuple tuple ->
    let%bind tuple = bind_map_list (decompile_expression ~dialect) tuple in
    let%bind tuple = list_to_nsepseq tuple in
    return_expr @@ CST.ETuple (wrap @@ par tuple)
  | E_map map ->
    let%bind map = bind_map_list (bind_map_pair (decompile_expression ~dialect)) map in
    let aux (k,v) =
      let binding : CST.binding = {source=k;arrow=ghost;image=v} in
      wrap @@ binding
    in
    let map = list_to_sepseq @@ List.map aux map in
    return_expr @@ CST.EMap (MapInj (wrap @@ inject dialect (InjMap ghost) @@ map))
  | E_big_map big_map ->
    let%bind big_map = bind_map_list (bind_map_pair (decompile_expression ~dialect)) big_map in
    let aux (k,v) =
      let binding : CST.binding = {source=k;arrow=ghost;image=v} in
      wrap @@ binding
    in
    let big_map = list_to_sepseq @@ List.map aux big_map in
    return_expr @@ CST.EMap (BigMapInj (wrap @@ inject dialect (InjBigMap ghost) @@ big_map))
  | E_list lst ->
    let%bind lst = bind_map_list (decompile_expression ~dialect) lst in
    let lst = list_to_sepseq lst in
    return_expr @@ CST.EList (EListComp (wrap @@ inject dialect (InjList ghost) @@ lst))
  | E_set set ->
    let%bind set = bind_map_list (decompile_expression ~dialect) set in
    let set = list_to_sepseq set in
    return_expr @@ CST.ESet (SetInj (wrap @@ inject dialect (InjSet ghost) @@ set))
  | E_assign {variable;access_path;expression} ->
    let%bind lhs = decompile_to_lhs dialect variable access_path in
    let%bind rhs = decompile_expression ~dialect expression in
    let assign : CST.assignment = {lhs;assign=ghost;rhs} in
    return_inst @@ Assign (wrap assign)
  | E_for {binder;start;final;incr;f_body} ->
    let binder     = decompile_variable binder.wrap_content in
    let%bind init  = decompile_expression ~dialect start in
    let%bind bound = decompile_expression ~dialect final in
    let%bind step  = decompile_expression ~dialect incr  in
    let step       = Some (ghost, step) in
    let%bind (block,_next) = decompile_to_block dialect f_body in
    let block = wrap @@ Option.unopt ~default:(empty_block dialect) block in
    let fl : CST.for_int = {kwd_for=ghost;binder;assign=ghost;init;kwd_to=ghost;bound;step;block} in
    return_inst @@ CST.Loop (For (ForInt (wrap fl)))
  | E_for_each {fe_binder;collection;collection_type;fe_body} ->
    let var = decompile_variable @@ (fst fe_binder).wrap_content in
    let bind_to = Option.map (fun (x:AST.expression_variable) -> (ghost,decompile_variable x.wrap_content)) @@ snd fe_binder in
    let%bind expr = decompile_expression ~dialect collection in
    let collection = match collection_type with
      Map -> CST.Map ghost | Set -> Set ghost | List -> List ghost | Any -> failwith "TODO : have the type of the collection propagated from AST_typed" in
    let%bind (block,_next) = decompile_to_block dialect fe_body in
    let block = wrap @@ Option.unopt ~default:(empty_block dialect) block in
    let fc : CST.for_collect = {kwd_for=ghost;var;bind_to;kwd_in=ghost;collection;expr;block} in
    return_inst @@ CST.Loop (For (ForCollect (wrap fc)))
  | E_while {cond;body} ->
    let%bind cond  = decompile_expression ~dialect cond in
    let%bind (block,_next) = decompile_to_block dialect body in
    let block = wrap @@ Option.unopt ~default:(empty_block dialect) block in
    let loop : CST.while_loop = {kwd_while=ghost;cond;block} in
    return_inst @@ CST.Loop (While (wrap loop))

and decompile_if_clause : dialect -> AST.expression -> (CST.if_clause, _) result = fun dialect e ->
  let%bind clause = decompile_statements dialect e in
  match clause with
    CST.Instr instr,[] ->
    ok @@ CST.ClauseInstr instr
  | _ ->
    let clause = nelist_to_npseq clause, Some ghost in
    ok @@ CST.ClauseBlock (ShortBlock (wrap @@ braces @@ clause))

and decompile_to_data_decl : dialect -> _ AST.binder -> AST.expression -> AST.attributes -> (CST.data_decl, _) result =
    fun dialect binder expr attributes ->
  let name = decompile_variable binder.var.wrap_content in
  let%bind const_type =
    Trace.bind_map_option
      (bind_compose (ok <@ prefix_colon)
                    (decompile_type_expr dialect))
      binder.ascr in
  let attributes = decompile_attributes attributes in
  let fun_name = name in
  let terminator = terminator dialect in
  match expr.expression_content with
    E_lambda lambda ->
    let%bind (param,ret_type,return) = decompile_lambda dialect lambda in
    let fun_decl : CST.fun_decl = {kwd_recursive=None;kwd_function=ghost;fun_name;param;ret_type;kwd_is=ghost;return;terminator;attributes} in
    ok @@ CST.LocalFun (wrap fun_decl)
  | E_recursive {lambda; _} ->
    let%bind (param,ret_type,return) = decompile_lambda dialect lambda in
    let fun_decl : CST.fun_decl = {kwd_recursive=Some ghost;kwd_function=ghost;fun_name;param;ret_type;kwd_is=ghost;return;terminator;attributes} in
    ok @@ CST.LocalFun (wrap fun_decl)
  | _ ->
    let%bind init = decompile_expression ~dialect expr in
    let const_decl : CST.const_decl = {kwd_const=ghost;pattern = PVar name;const_type;equal=ghost;init;terminator; attributes} in
    let data_decl  : CST.data_decl  =  LocalConst (wrap const_decl) in
    ok @@ data_decl

and decompile_to_lhs : dialect -> AST.expression_variable -> _ AST.access list -> (CST.lhs, _) result = fun dialect var access ->
  match List.rev access with
    [] -> ok @@ (CST.Path (Name (decompile_variable var.wrap_content)) : CST.lhs)
  | hd :: tl ->
    match hd with
    | AST.Access_map e ->
      let%bind path = decompile_to_path var @@ List.rev tl in
      let%bind index = map (wrap <@ brackets) @@ decompile_expression ~dialect e in
      let mlu: CST.map_lookup = {path;index} in
      ok @@ CST.MapPath (wrap @@ mlu)
    | _ ->
      let%bind path = decompile_to_path var @@ access in
      ok @@ (CST.Path (path) : CST.lhs)

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

and decompile_lambda : dialect -> (AST.expr, AST.ty_expr) AST.lambda -> _ = fun dialect {binder;result} ->
    let var = decompile_variable @@ binder.var.wrap_content in
    let%bind param_type = bind_map_option (bind_compose (ok <@ prefix_colon) (decompile_type_expr dialect)) binder.ascr in
    let param_const : CST.param_const = {kwd_const=ghost;var;param_type} in
    let param_decl : CST.param_decl = ParamConst (wrap param_const) in
    let param = nelist_to_npseq (param_decl, []) in
    let param : CST.parameters = wrap @@ par param in
    let%bind result,ret_type = match result.expression_content with
      AST.E_ascription {anno_expr; type_annotation} ->
      let%bind ret_type = bind_compose (ok <@ prefix_colon) (decompile_type_expr dialect) type_annotation in
      ok @@ (anno_expr, Some ret_type)
    | _ -> ok (result,None) in
    let%bind return = decompile_expression ~dialect result in
    ok @@ (param,ret_type,return)

and decompile_declaration ~dialect : AST.declaration Location.wrap -> (CST.declaration, _) result = fun decl ->
  let decl = Location.unwrap decl in
  let wrap value = ({value;region=Region.ghost} : _ Region.reg) in
  match decl with
    Declaration_type {type_binder;type_expr} ->
    let kwd_type = Region.ghost
    and name = decompile_variable type_binder
    and kwd_is = Region.ghost in
    let%bind type_expr = decompile_type_expr dialect type_expr in
    let terminator = terminator dialect in
    ok @@ CST.TypeDecl (wrap (CST.{kwd_type; name; kwd_is; type_expr; terminator}))
  | Declaration_constant {binder; attr; expr} -> (
    let attributes = decompile_attributes attr in
    let name = decompile_variable binder.var.wrap_content in
    let fun_name = name in
    let terminator = terminator dialect in
    match expr.expression_content with
      E_lambda lambda ->
      let%bind (param,ret_type,return) = decompile_lambda dialect lambda in
      let fun_decl : CST.fun_decl = {kwd_recursive=None;kwd_function=ghost;fun_name;param;ret_type;kwd_is=ghost;return;terminator;attributes} in
      ok @@ CST.FunDecl (wrap fun_decl)
    | E_recursive {lambda; _} ->
      let%bind (param,ret_type,return) = decompile_lambda dialect lambda in
      let fun_decl : CST.fun_decl = {kwd_recursive=Some ghost;kwd_function=ghost;fun_name;param;ret_type;kwd_is=ghost;return;terminator;attributes} in
      ok @@ CST.FunDecl (wrap fun_decl)
    | _ ->
      let%bind const_type = bind_map_option (bind_compose (ok <@ prefix_colon) (decompile_type_expr dialect)) binder.ascr in
      let%bind init = decompile_expression ~dialect expr in
      let const_decl : CST.const_decl = {kwd_const=ghost;pattern = PVar name;const_type=const_type;equal=ghost;init;terminator; attributes} in
      ok @@ CST.ConstDecl (wrap const_decl)
  )
  | Declaration_module {module_binder;module_} ->
    let kwd_module = Region.ghost
    and name = wrap module_binder
    and kwd_is = Region.ghost in
    let%bind module_ = decompile_module ~dialect module_ in
    let terminator = terminator dialect in
    let enclosing = module_enclosing dialect in
    ok @@ CST.ModuleDecl (wrap (CST.{kwd_module; name; kwd_is; enclosing; module_; terminator}))
  | Module_alias {alias;binders} ->
    let kwd_module = Region.ghost
    and alias   = wrap alias
    and binders = nelist_to_npseq @@ List.Ne.map wrap binders
    and kwd_is = Region.ghost in
    let terminator = terminator dialect in
    ok @@ CST.ModuleAlias (wrap (CST.{kwd_module; alias; kwd_is; binders; terminator}))
  

and decompile_module ?(dialect=Verbose): AST.module_ -> (CST.ast, _) result = fun prg ->
  let%bind decl = bind_map_list (decompile_declaration ~dialect) prg in
  let decl = List.Ne.of_list decl in
  ok @@ ({decl;eof=ghost}: CST.ast)
