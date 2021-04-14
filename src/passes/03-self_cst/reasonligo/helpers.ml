open Cst.Reasonligo
open Trace

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map snd tl)
let bind_map_npseq f (hd,tl) =
  let%bind hd = f hd in
  let%bind tl = bind_map_list (fun (a,b) -> let%bind b = f b in ok @@ (a,b)) tl in
  ok @@ (hd,tl)
let bind_fold_npseq f init (hd,tl) =
  let%bind res = f init hd in
  let%bind res = bind_fold_list (fun init (_,b) -> f init b) res tl in
  ok @@ res


let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let bind_map_pseq f = bind_map_option @@ bind_map_npseq f
let bind_fold_pseq f init seq =
  let%bind res = bind_map_option (bind_fold_npseq f init) seq in
  ok @@ Option.unopt ~default:(init) res

type ('a, 'err) folder = {
  e : 'a -> expr -> ('a, 'err) result;
  t : 'a -> type_expr -> ('a, 'err) result ;
  d : 'a -> declaration -> ('a, 'err) result;
}

let rec fold_type_expression : ('a, 'err) folder -> 'a -> type_expr -> ('a, 'err) result = fun f init t ->
  let self = fold_type_expression f in
  let%bind init = f.t init t in
  match t with
    TProd   {value;region=_} ->
    bind_fold_ne_list self init @@ npseq_to_ne_list value.inside
  | TSum    {value;region=_} ->
    let {lead_vbar=_;variants;attributes=_} = value in
    let aux init ({value;region=_} : _ reg) =
      let {constr=_;arg;attributes=_} = value in
      match arg with
        Some (_,t) -> self init t
      | None -> ok @@ init
    in
    bind_fold_ne_list aux init @@ npseq_to_ne_list variants
  | TRecord {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_name=_;colon=_;field_type;attributes=_} = value in
      self init field_type
    in
    bind_fold_ne_list aux init @@ npseq_to_ne_list value.ne_elements
  | TApp    {value;region=_} ->
    let (_, tuple) = value in
    bind_fold_ne_list self init @@ npseq_to_ne_list tuple.value.inside
  | TFun    {value;region=_} ->
    let (ty1, _, ty2) = value in
    let%bind res = self init ty1 in
    let%bind res = self res  ty2 in
    ok @@ res
  | TPar    {value;region=_} ->
    self init value.inside
  | TModA {value;region=_} ->
    self init value.field
  | TVar    _
  | TWild   _
  | TInt    _
  | TString _ -> ok @@ init

let rec fold_expression : ('a, 'err) folder -> 'a -> expr -> ('a, 'err) result = fun f init e  ->
  let self = fold_expression f in
  let self_type = fold_type_expression f in
  let self_module = fold_module f in
  let%bind init = f.e init e in
  let bin_op value =
    let {op=_;arg1;arg2} = value in
    let%bind res = fold_expression f init arg1 in
    let%bind res = fold_expression f res  arg2 in
    ok @@ res
  in
  match e with
    ECase    {value;region=_} ->
    let {kwd_switch=_;expr;lbrace=_;cases;rbrace=_} = value in
    let%bind res = self init expr in
    let%bind res = matching_cases self res cases in
    ok @@ res
  | ECond    {value;region=_} ->
    let {kwd_if=_;test;ifso;ifnot} = value in
    let%bind res = self init test in
    let%bind res = self res @@ fst ifso.inside in
    (match ifnot with
    | None -> ok @@ res
    | Some (_,e) -> self res @@ fst e.inside
    )
  | EAnnot   {value;region=_} ->
    let (expr, _, type_expr) = value in
    let%bind res = self init expr in
    let%bind res = self_type res type_expr in
    ok res
  | ELogic BoolExpr Or  {value;region=_} -> bin_op value
  | ELogic BoolExpr And {value;region=_} -> bin_op value
  | ELogic BoolExpr Not {value;region=_} ->
    let {op=_;arg} = value in
    let%bind res = fold_expression f init arg in
    ok @@ res
  | ELogic BoolExpr True _ -> ok @@ init
  | ELogic BoolExpr False _ -> ok @@ init
  | ELogic CompExpr Lt    {value;region=_}
  | ELogic CompExpr Leq   {value;region=_}
  | ELogic CompExpr Gt    {value;region=_}
  | ELogic CompExpr Geq   {value;region=_}
  | ELogic CompExpr Equal {value;region=_}
  | ELogic CompExpr Neq   {value;region=_} ->
    bin_op value
  | EArith Add   {value;region=_}
  | EArith Sub   {value;region=_}
  | EArith Mult  {value;region=_}
  | EArith Div   {value;region=_}
  | EArith Mod   {value;region=_} ->
    bin_op value
  | EArith Neg   {value;region=_} ->
    let {op=_;arg} = value in
    let%bind res = fold_expression f init arg in
    ok @@ res
  | EArith Int   _
  | EArith Nat   _
  | EArith Mutez _ -> ok @@ init
  | EString Cat {value;region=_} -> bin_op value
  | EString String   _
  | EString Verbatim _ -> ok init
  | EList ECons {value;region=_} ->
    let {lbracket=_;lexpr;comma=_;ellipsis=_;rexpr;rbracket=_} = value in
    let%bind res = self init lexpr in
    let%bind res = self res rexpr in
    ok @@ res
  | EList EListComp {value;region=_} ->
    bind_fold_list self init @@ pseq_to_list value.elements
  | EConstr ENone _ -> ok @@ init
  | EConstr ESomeApp {value;region=_} ->
    let _, expr = value in
    self init expr
  | EConstr EConstrApp {value;region=_} ->
    let _, expr = value in
    (match expr with
      None -> ok @@ init
    | Some e -> self init e
    )
  | ERecord  {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_name=_;assignment=_;field_expr} = value in
      let%bind res = self init field_expr in
      ok res
    in
    bind_fold_ne_list aux init @@ npseq_to_ne_list value.ne_elements
  | EProj    _ -> ok @@ init
  | EUpdate  {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_path=_;assignment=_;field_expr} = value in
      let%bind res = self init field_expr in
      ok res
    in
    bind_fold_ne_list aux init @@ npseq_to_ne_list value.updates.value.ne_elements
  | EModA    {value;region=_} -> self init value.field
  | EVar     _ -> ok init
  | ECall    {value;region=_} ->
    let (lam, args) = value in
    let%bind res = self init lam in
    (match args with
    | Unit _ -> ok @@ res
    | Multiple {value;region=_} ->
      bind_fold_ne_list self res @@ npseq_to_ne_list value.inside
    )
  | EBytes   _ -> ok @@ init
  | EUnit    _ -> ok @@ init
  | ETuple   {value;region=_} ->
    bind_fold_ne_list self init @@ npseq_to_ne_list value
  | EPar     {value;region=_} ->
    self init value.inside
  | ELetIn   {value;region=_} ->
    let {kwd_let=_;kwd_rec=_;binding;semi=_;body;attributes=_} = value in
    let {binders=_;lhs_type;eq=_;let_rhs} = binding in
    let%bind res = self init let_rhs in
    let%bind res = self res body in
    (match lhs_type with
      Some (_, ty) -> self_type res ty
    | None ->    ok @@ res
    )
  | ETypeIn  {value;region=_} ->
    let {type_decl;semi=_;body} = value in
    let {kwd_type=_;name=_;eq=_;type_expr} = type_decl in
    let%bind res = self_type init type_expr in
    let%bind res = self res body in
    ok @@ res
  | EModIn  {value;region=_} ->
    let {mod_decl;semi=_;body} = value in
    let {kwd_module=_;name=_;eq=_;lbrace=_;module_;rbrace=_} = mod_decl in
    let%bind res = self_module init module_ in
    let%bind res = self res body in
    ok @@ res
  | EModAlias  {value;region=_} ->
    let {mod_alias;semi=_;body} = value in
    let {kwd_module=_;alias=_;eq=_;binders=_} = mod_alias in
    let%bind res = self init body in
    ok @@ res
  | EFun     {value;region=_} ->
    let {binders=_; lhs_type; arrow=_; body} = value in
    let%bind res = self init body in
    (match lhs_type with
      Some (_, ty) -> self_type res ty
    | None ->    ok res
    )
  | ESeq     {value;region=_} ->
    bind_fold_list self init @@ pseq_to_list value.elements
  | ECodeInj {value;region=_} ->
    let {language=_;code;rbracket=_} = value in
    self init code

and matching_cases self init ({value;region=_}: _ reg) =
  bind_fold_ne_list (case_clause self) init @@ npseq_to_ne_list value

and case_clause self init ({value;region=_}: _ case_clause reg) =
  let {pattern=_;arrow=_;rhs} = value in
  self init rhs

and fold_declaration : ('a, 'err) folder -> 'a -> declaration -> ('a, 'err) result =
  fun f init d ->
  let self_expr = fold_expression f in
  let self_type = fold_type_expression f in
  let self_module = fold_module f in
  let%bind init = f.d init d in
  match d with
    ConstDecl {value;region=_} ->
    let (_,_,let_binding,_) = value in
    let {binders=_;lhs_type;eq=_;let_rhs} = let_binding in
    let%bind res = self_expr init let_rhs in
    (match lhs_type with
      Some (_, ty) -> self_type res ty
    | None -> ok res
    )
  | TypeDecl {value;region=_} ->
    let {kwd_type=_;name=_;eq=_;type_expr} = value in
    let%bind res = self_type init type_expr in
    ok res

  | ModuleDecl {value;region=_} ->
    let {kwd_module=_;name=_;eq=_;lbrace=_;module_;rbrace=_} = value in
    let%bind res = self_module init module_ in
    ok res
  | ModuleAlias {value;region=_} ->
    let {kwd_module=_;alias=_;eq=_;binders=_} = value in
    ok init
  | Directive _ -> ok init

and fold_module : ('a, 'err) folder -> 'a -> t -> ('a, 'err) result =
  fun f init {decl;eof=_} ->
  let self = fold_declaration f in
  bind_fold_ne_list self init @@ decl

type ('err) mapper = {
  e : expr -> (expr, 'err) result;
  t : type_expr -> (type_expr, 'err) result ;
  d : declaration -> (declaration, 'err) result ;
}

let rec map_type_expression : ('err) mapper -> type_expr -> ('b, 'err) result = fun f t ->
  let self = map_type_expression f in
  let%bind t = f.t t in
  let return = ok in
  match t with
    TProd   {value;region} ->
    let%bind inside = bind_map_npseq self value.inside in
    let value = {value with inside} in
    return @@ TProd {value;region}
  | TSum    {value;region} ->
    let aux (e : variant reg) =
      let%bind arg = bind_map_option (fun (a,b) -> let%bind b = self b in ok (a,b)) e.value.arg in
      let value = {e.value with arg} in
      ok @@ {e with value}
    in
    let%bind variants = bind_map_npseq aux value.variants in
    let value = {value with variants} in
    return @@ TSum {value;region}
  | TRecord {value;region} ->
    let aux (element : _ reg ) =
      let%bind field_type = self element.value.field_type in
      let value = {element.value with field_type} in
      ok @@ {element with value }
    in
    let%bind ne_elements = bind_map_npseq aux value.ne_elements in
    let value = {value with ne_elements} in
    return @@ TRecord {value;region}
  | TApp    {value;region} ->
    let (const, tuple) = value in
    let%bind inside = bind_map_npseq self tuple.value.inside in
    let tuple = {tuple with value = {tuple.value with inside }} in
    let value = (const, tuple) in
    return @@ TApp {value;region}
  | TFun    {value;region} ->
    let (ty1, wild, ty2) = value in
    let%bind ty1 = self ty1 in
    let%bind ty2 = self ty2 in
    let value = (ty1, wild, ty2) in
    return @@ TFun {value;region}
  | TPar    {value;region} ->
    let%bind inside = self value.inside in
    let value = {value with inside} in
    return @@ TPar {value;region}
  | TModA {value;region} ->
    let%bind field = self value.field in
    let value = {value with field} in
    return @@ TModA {value;region}
  | (TVar   _
  | TWild   _
  | TInt    _
  | TString _ as e )-> ok @@ e

let rec map_expression : ('err) mapper -> expr -> (expr, 'err) result = fun f e  ->
  let self = map_expression f in
  let self_type = map_type_expression f in
  let self_module = map_module f in
  let return = ok in
  let%bind e = f.e e in
  let bin_op value =
    let {op;arg1;arg2} = value in
    let%bind arg1 = self arg1 in
    let%bind arg2 = self arg2 in
    ok @@ {op;arg1;arg2}
  in
  match e with
    ECase    {value;region} ->
    let {kwd_switch=_;expr;lbrace=_;cases;rbrace=_} = value in
    let%bind expr = self expr in
    let%bind cases = matching_cases self cases in
    let value = {value with expr;cases} in
    return @@ ECase {value;region}
  | ECond    {value;region} ->
    let {kwd_if;test;ifso;ifnot} = value in
    let%bind test = self test in
    let aux (x: _ braced) =
      let%bind inside = (fun (a,b)
        -> let%bind a = self a in ok @@ (a,b)) x.inside in
      ok @@ {x with inside}
    in
    let%bind ifso = aux ifso in
    let%bind ifnot = bind_map_option (fun (a,b) ->
      let%bind b = aux b in ok @@ (a,b)) ifnot in
    let value = {kwd_if;test;ifso;ifnot} in
    return @@ ECond {value;region}
  | EAnnot   {value;region} ->
    let expr, comma, type_expr = value in
    let%bind expr = self expr in
    let%bind type_expr = self_type type_expr in
    let value = expr, comma, type_expr in
    return @@ EAnnot {value;region}
  | ELogic BoolExpr Or  {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (BoolExpr (Or {value;region}))
  | ELogic BoolExpr And {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (BoolExpr (And {value;region}))
  | ELogic BoolExpr Not {value;region} ->
    let%bind arg = self value.arg in
    let value = {value with arg} in
    return @@ ELogic (BoolExpr (Not {value;region}))
  | ELogic BoolExpr True _
  | ELogic BoolExpr False _ as e -> return @@ e
  | ELogic CompExpr Lt    {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Lt {value;region}))
  | ELogic CompExpr Leq   {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Leq {value;region}))
  | ELogic CompExpr Gt    {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Gt {value;region}))
  | ELogic CompExpr Geq   {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Geq {value;region}))
  | ELogic CompExpr Equal {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Equal {value;region}))
  | ELogic CompExpr Neq   {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Neq {value;region}))
  | EArith Add   {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Add {value;region})
  | EArith Sub   {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Sub {value;region})
  | EArith Mult  {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Mult {value;region})
  | EArith Div   {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Div {value;region})
  | EArith Mod   {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Mod {value;region})
  | EArith Neg   {value;region} ->
    let%bind arg = self value.arg in
    let value = {value with arg} in
    return @@ EArith (Neg {value;region})
  | EArith Int   _
  | EArith Nat   _
  | EArith Mutez _ as e -> return @@ e
  | EString Cat {value;region} ->
    let%bind value = bin_op value in
    return @@ EString (Cat {value;region})
  | EString String   _
  | EString Verbatim _ as e -> return @@ e
  | EList ECons {value;region} ->
    let {lbracket=_;lexpr;comma=_;ellipsis=_;rexpr;rbracket=_} = value in
    let%bind lexpr = self lexpr in
    let%bind rexpr = self rexpr in
    let value = {value with lexpr;rexpr} in
    return @@ EList (ECons {value;region})
  | EList EListComp {value;region} ->
    let%bind elements = bind_map_pseq self value.elements in
    let value = {value with elements} in
    return @@ EList (EListComp {value;region})
  | EConstr ENone _ as e -> return @@ e
  | EConstr ESomeApp {value;region} ->
    let some_, expr = value in
    let%bind expr = self expr in
    let value = some_,expr in
    return @@ EConstr (ESomeApp {value;region})
  | EConstr EConstrApp {value;region} ->
    let const, expr = value in
    let%bind expr = bind_map_option self expr in
    let value = const,expr in
    return @@ EConstr (EConstrApp {value;region})
  | ERecord  {value;region} ->
    let aux (e : field_assign reg) =
      let%bind field_expr = self e.value.field_expr in
      ok @@ {e with value = {e.value with field_expr}}
    in
    let%bind ne_elements = bind_map_npseq aux value.ne_elements in
    let value = {value with ne_elements} in
    return @@ ERecord {value;region}
  | EProj    _  as e -> return @@ e
  | EUpdate  {value;region} ->
    let aux (e : field_path_assignment reg) =
      let%bind field_expr = self e.value.field_expr in
      ok @@ {e with value = {e.value with field_expr}}
    in
    let%bind ne_elements = bind_map_npseq aux value.updates.value.ne_elements in
    let updates = {value.updates with value = {value.updates.value with ne_elements}} in
    let value = {value with updates} in
    return @@ EUpdate {value;region}
  | EModA {value;region} ->
    let%bind field = self value.field in
    let value = {value with field} in
    return @@ EModA {value;region}
  | EVar     _ as e -> return e
  | ECall    {value;region} ->
    let (lam, args) = value in
    let%bind lam = self lam in
    let%bind args = (match args with
    | Unit _ as u -> ok @@ u
    | Multiple {value;region} ->
      let%bind inside = bind_map_npseq self value.inside in
      let value = {value with inside} in
      ok @@ Multiple {value;region}
    ) in
    let value = (lam,args) in
    return @@ ECall {value;region}
  | EBytes   _ as e -> return @@ e
  | EUnit    _ as e -> return @@ e
  | ETuple   {value;region} ->
    let%bind value = bind_map_npseq self value in
    return @@ ETuple {value;region}
  | EPar     {value;region} ->
    let%bind inside = self value.inside in
    let value = {value with inside} in
    return @@ EPar {value;region}
  | ELetIn   {value;region} ->
    let {kwd_let=_;kwd_rec=_;binding;semi=_;body;attributes=_} = value in
    let {binders;lhs_type;eq;let_rhs} = binding in
    let%bind let_rhs = self let_rhs in
    let%bind lhs_type = bind_map_option (fun (a,b) ->
      let%bind b = self_type b in ok (a,b)) lhs_type in
    let binding = {binders;lhs_type;eq;let_rhs} in
    let%bind body = self body in
    let value = {value with binding;body} in
    return @@ ELetIn {value;region}
  | ETypeIn  {value;region} ->
    let {type_decl;semi;body} = value in
    let {kwd_type=_;name=_;eq=_;type_expr} = type_decl in
    let%bind type_expr = self_type type_expr in
    let%bind body = self body in
    let type_decl = {type_decl with type_expr} in
    let value = {type_decl;semi;body} in
    return @@ ETypeIn {value;region}
  | EModIn  {value;region} ->
    let {mod_decl;semi;body} = value in
    let {kwd_module=_;name=_;eq=_;lbrace=_;module_;rbrace=_} = mod_decl in
    let%bind module_ = self_module module_ in
    let%bind body = self body in
    let mod_decl = {mod_decl with module_} in
    let value = {mod_decl;semi;body} in
    return @@ EModIn {value;region}
  | EModAlias  {value;region} ->
    let {mod_alias;semi;body} = value in
    let {kwd_module=_;alias=_;eq=_;binders=_} = mod_alias in
    let%bind body = self body in
    let value = {mod_alias;semi;body} in
    return @@ EModAlias {value;region}
  | EFun     {value;region} ->
    let {binders=_; lhs_type; arrow=_; body} = value in
    let%bind body = self body in
    let%bind lhs_type = bind_map_option (fun (a,b) ->
      let%bind b = self_type b in ok (a,b)) lhs_type in
    let value = {value with body;lhs_type} in
    return @@ EFun {value;region}
  | ESeq     {value;region} ->
    let%bind elements = bind_map_pseq self value.elements in
    let value = {value with elements} in
    return @@ ESeq {value;region}
  | ECodeInj {value;region} ->
    let%bind code = self value.code in
    let value = {value with code} in
    return @@ ECodeInj {value;region}

and matching_cases self (cases: _ Utils.nsepseq reg) =
  let%bind value = bind_map_npseq (case_clause self) @@ cases.value in
  ok @@ {cases with value}

and case_clause self (case_clause: _ case_clause reg) =
  let {pattern=_;arrow=_;rhs} = case_clause.value in
  let%bind rhs = self rhs in
  let value = {case_clause.value with rhs} in
  ok @@ {case_clause with value}

and map_declaration : ('err) mapper -> declaration -> (declaration, 'err) result =
  fun f d ->
  let self_expr = map_expression f in
  let self_type = map_type_expression f in
  let self_module = map_module f in
  let return = ok in
  match d with
    ConstDecl {value;region} ->
    let (kwd_let,kwd_rec,let_binding,attr) = value in
    let {binders;lhs_type;eq;let_rhs} = let_binding in
    let%bind let_rhs = self_expr let_rhs in
    let%bind lhs_type = bind_map_option (fun (a,b) ->
      let%bind b = self_type b in ok (a,b)) lhs_type in
    let let_binding = {binders;lhs_type;eq;let_rhs} in
    let value = (kwd_let,kwd_rec,let_binding,attr) in
    return @@ ConstDecl {value;region}
  | TypeDecl {value;region} ->
    let {kwd_type=_;name=_;eq=_;type_expr} = value in
    let%bind type_expr = self_type type_expr in
    let value = {value with type_expr} in
    return @@ TypeDecl {value;region}
  | ModuleDecl {value;region} ->
    let {kwd_module=_;name=_;eq=_;lbrace=_;module_;rbrace=_} = value in
    let%bind module_ = self_module module_ in
    let value = {value with module_} in
    return @@ ModuleDecl {value;region}
  | ModuleAlias {value;region} ->
    let {kwd_module=_;alias=_;eq=_;binders=_} = value in
    return @@ ModuleAlias {value;region}
  | Directive _ as d -> return d

and map_module : ('err) mapper -> t -> (t, 'err) result =
  fun f {decl;eof} ->
  let self = map_declaration f in
  map (fun decl -> {decl;eof}) @@
  bind_map_ne_list self @@ decl

(* TODO this is stupid *)
let fold_to_map : unit -> (unit, 'err) folder -> ('err) mapper =
  fun init {e;t;d} ->
  let e expr =
    let%bind () = e init expr in ok @@ expr
  in
  let t ty =
    let%bind () = t init ty in ok @@ ty
  in
  let d decl =
    let%bind () = d init decl in ok @@ decl
  in
  {e;t;d}
