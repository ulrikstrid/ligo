(* Concrete Syntax Tree (CST) for CameLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30-40-42"]

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module ExtRegion = Stage_common.Ext_region

open Utils
type 'a reg = 'a ExtRegion.reg

(* Lexemes *)

type lexeme = string

(* Keywords of OCaml *)

type keyword       = ExtRegion.t
type kwd_and       = ExtRegion.t
type kwd_begin     = ExtRegion.t
type kwd_else      = ExtRegion.t
type kwd_end       = ExtRegion.t
type kwd_false     = ExtRegion.t
type kwd_fun       = ExtRegion.t
type kwd_rec       = ExtRegion.t
type kwd_if        = ExtRegion.t
type kwd_in        = ExtRegion.t
type kwd_let       = ExtRegion.t
type kwd_match     = ExtRegion.t
type kwd_mod       = ExtRegion.t
type kwd_not       = ExtRegion.t
type kwd_of        = ExtRegion.t
type kwd_or        = ExtRegion.t
type kwd_then      = ExtRegion.t
type kwd_true      = ExtRegion.t
type kwd_type      = ExtRegion.t
type kwd_with      = ExtRegion.t
type kwd_let_entry = ExtRegion.t
type kwd_module    = ExtRegion.t
type kwd_struct    = ExtRegion.t

(* Data constructors *)

type c_None  = ExtRegion.t
type c_Some  = ExtRegion.t

(* Symbols *)

type arrow    = ExtRegion.t  (* "->" *)
type cons     = ExtRegion.t  (* "::" *)
type cat      = ExtRegion.t  (* "^"  *)
type append   = ExtRegion.t  (* "@"  *)
type dot      = ExtRegion.t  (* "."  *)

(* Arithmetic operators *)

type minus    = ExtRegion.t  (* "-" *)
type plus     = ExtRegion.t  (* "+" *)
type slash    = ExtRegion.t  (* "/" *)
type times    = ExtRegion.t  (* "*" *)

(* Boolean operators *)

type bool_or  = ExtRegion.t  (* "||" *)
type bool_and = ExtRegion.t  (* "&&" *)

(* Comparisons *)

type equal = ExtRegion.t  (* "="  *)
type neq   = ExtRegion.t  (* "<>" *)
type lt    = ExtRegion.t  (* "<"  *)
type gt    = ExtRegion.t  (* ">"  *)
type leq   = ExtRegion.t  (* "=<" *)
type geq   = ExtRegion.t  (* ">=" *)

(* Compounds *)

type lpar     = ExtRegion.t  (* "(" *)
type rpar     = ExtRegion.t  (* ")" *)
type lbracket = ExtRegion.t  (* "[" *)
type rbracket = ExtRegion.t  (* "]" *)
type lbrace   = ExtRegion.t  (* "{" *)
type rbrace   = ExtRegion.t  (* "}" *)

(* Separators *)

type comma = ExtRegion.t  (* "," *)
type semi  = ExtRegion.t  (* ";" *)
type vbar  = ExtRegion.t  (* "|" *)
type colon = ExtRegion.t  (* ":" *)

(* Wildcard *)

type wild = ExtRegion.t  (* "_" *)

(* Virtual tokens *)

type eof = ExtRegion.t

(* Literals *)

type variable    = string reg
type module_name = string reg
type fun_name    = string reg
type type_name   = string reg
type field_name  = string reg
type type_constr = string reg
type constr      = string reg
type attribute   = string reg

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

type the_unit = lpar * rpar

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and ast = t

and attributes = attribute list

and declaration =
  Let         of let_decl     reg
| TypeDecl    of type_decl    reg
| ModuleDecl  of module_decl  reg
| ModuleAlias of module_alias reg
| Directive   of Directive.t

(* Non-recursive values *)

and let_decl =
  (kwd_let * kwd_rec option * let_binding * attributes)

and let_binding = {
  binders  : pattern nseq;
  lhs_type : (colon * type_expr) option;
  eq       : equal;
  let_rhs  : expr
}

(* Type declarations *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  eq         : equal;
  type_expr  : type_expr
}

and module_decl = {
  kwd_module : kwd_module;
  name       : module_name;
  eq         : equal;
  kwd_struct : kwd_struct;
  module_    : t;
  kwd_end    : kwd_end;
}

and module_alias = {
  kwd_module : kwd_module;
  alias      : module_name;
  eq         : equal;
  binders    : (module_name, dot) nsepseq;
}

and type_expr =
  TProd   of cartesian
| TSum    of sum_type reg
| TRecord of field_decl reg ne_injection reg
| TApp    of (type_constr * type_tuple) reg
| TFun    of (type_expr * arrow * type_expr) reg
| TPar    of type_expr par reg
| TVar    of variable
| TWild   of wild
| TString of lexeme reg
| TInt    of (lexeme * Z.t) reg
| TModA   of type_expr module_access reg

and cartesian = (type_expr, times) nsepseq reg

and sum_type = {
  lead_vbar  : vbar option;
  variants   : (variant reg, vbar) nsepseq;
  attributes : attributes
}

and variant = {
  constr     : constr;
  arg        : (kwd_of * type_expr) option;
  attributes : attributes
}

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr;
  attributes : attributes
}

and type_tuple = (type_expr, comma) nsepseq par reg

and pattern =
  PConstr   of constr_pattern
| PUnit     of the_unit reg
| PVar      of variable
| PInt      of (lexeme * Z.t) reg
| PNat      of (lexeme * Z.t) reg
| PBytes    of (lexeme * Hex.t) reg
| PString   of string reg
| PVerbatim of string reg
| PList     of list_pattern
| PTuple    of (pattern, comma) nsepseq reg
| PPar      of pattern par reg
| PRecord   of field_pattern reg ne_injection reg
| PTyped    of typed_pattern reg

and constr_pattern =
  PNone      of c_None
| PSomeApp   of (c_Some * pattern) reg
| PFalse    of kwd_false
| PTrue     of kwd_true
| PConstrApp of (constr * pattern option) reg

and list_pattern =
  PListComp of pattern injection reg
| PCons     of (pattern * cons * pattern) reg

and typed_pattern = {
  pattern   : pattern;
  colon     : colon;
  type_expr : type_expr
}

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

and expr =
  ECase     of expr case reg
| ECond     of cond_expr reg
| EAnnot    of annot_expr par reg
| ELogic    of logic_expr
| EArith    of arith_expr
| EString   of string_expr
| EList     of list_expr
| EConstr   of constr_expr
| ERecord   of record reg
| EProj     of projection reg
| EModA     of expr module_access reg
| EUpdate   of update reg
| EVar      of variable
| ECall     of (expr * expr nseq) reg
| EBytes    of (string * Hex.t) reg
| EUnit     of the_unit reg
| ETuple    of (expr, comma) nsepseq reg
| EPar      of expr par reg
| ELetIn    of let_in reg
| ETypeIn   of type_in reg
| EModIn    of mod_in reg
| EModAlias of mod_alias reg
| EFun      of fun_expr reg
| ESeq      of expr injection reg
| ECodeInj  of code_inj reg

and annot_expr = expr * colon * type_expr

and 'a injection = {
  compound   : compound option;
  elements   : ('a, semi) sepseq;
  terminator : semi option
}

and 'a ne_injection = {
  compound    : compound option;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option;
  attributes  : attributes
}

and compound =
  BeginEnd of kwd_begin * kwd_end
| Braces   of lbrace * rbrace
| Brackets of lbracket * rbracket

and list_expr =
  ECons     of cons bin_op reg
| EListComp of expr injection reg
  (*| Append of (expr * append * expr) reg*)

and string_expr =
  Cat      of cat bin_op reg
| String   of string reg
| Verbatim of string reg

and constr_expr =
  ENone      of c_None
| ESomeApp   of (c_Some * expr) reg
| EConstrApp of (constr * expr option) reg

and arith_expr =
  Add   of plus bin_op reg
| Sub   of minus bin_op reg
| Mult  of times bin_op reg
| Div   of slash bin_op reg
| Mod   of kwd_mod bin_op reg
| Neg   of minus un_op reg
| Int   of (string * Z.t) reg
| Nat   of (string * Z.t) reg
| Mutez of (string * Z.t) reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or    of kwd_or bin_op reg
| And   of kwd_and bin_op reg
| Not   of kwd_not un_op reg
| True  of kwd_true
| False of kwd_false

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and comp_expr =
  Lt    of lt    bin_op reg
| Leq   of leq   bin_op reg
| Gt    of gt    bin_op reg
| Geq   of geq   bin_op reg
| Equal of equal bin_op reg
| Neq   of neq   bin_op reg

and record = field_assign reg ne_injection

and 'a module_access = {
  module_name : module_name;
  selector    : dot;
  field       : 'a;
}

and projection = {
  struct_name : variable;
  selector    : dot;
  field_path  : (selection, dot) nsepseq
}

and selection =
  FieldName of variable
| Component of (string * Z.t) reg

and field_assign = {
  field_name : field_name;
  assignment : equal;
  field_expr : expr
}

and update = {
  lbrace   : lbrace;
  record   : path;
  kwd_with : kwd_with;
  updates  : field_path_assignment reg ne_injection reg;
  rbrace   : rbrace
}

and field_path_assignment = {
  field_path : path;
  assignment : equal;
  field_expr : expr
}

and path =
  Name of variable
| Path of projection reg

and 'a case = {
  kwd_match : kwd_match;
  expr      : expr;
  kwd_with  : kwd_with;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) nsepseq reg
}

and 'a case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and let_in = {
  kwd_let    : kwd_let;
  kwd_rec    : kwd_rec option;
  binding    : let_binding;
  kwd_in     : kwd_in;
  body       : expr;
  attributes : attributes
}

and type_in = {
  type_decl  : type_decl;
  kwd_in     : kwd_in;
  body       : expr;
}

and mod_in = {
  mod_decl : module_decl;
  kwd_in   : kwd_in;
  body     : expr;
}

and mod_alias = {
  mod_alias : module_alias;
  kwd_in    : kwd_in;
  body      : expr;
}

and fun_expr = {
  kwd_fun    : kwd_fun;
  binders    : pattern nseq;
  lhs_type   : (colon * type_expr) option;
  arrow      : arrow;
  body       : expr;
}

and cond_expr = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : expr;
  ifnot    : (kwd_else * expr) option;
}

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : string reg reg;
  code     : expr;
  rbracket : rbracket;
}

(* Projecting regions from some nodes of the AST *)

let rec last to_region = function
    [] -> ExtRegion.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  ExtRegion.cover (to_region hd) (last reg tl)

let type_expr_to_region = function
  TProd   {region; _}
| TSum    {region; _}
| TRecord {region; _}
| TApp    {region; _}
| TFun    {region; _}
| TPar    {region; _}
| TString {region; _}
| TInt    {region; _}
| TVar    {region; _}
| TWild    region
| TModA   {region; _}
 -> region

let list_pattern_to_region = function
  PListComp {region; _} | PCons {region; _} -> region

let constr_pattern_to_region = function
  PNone region | PSomeApp {region;_}
| PTrue region | PFalse region
| PConstrApp {region;_} -> region

let pattern_to_region = function
| PList p -> list_pattern_to_region p
| PConstr c -> constr_pattern_to_region c
| PUnit {region;_}
| PTuple {region;_} | PVar {region;_}
| PInt {region;_}
| PString {region;_} | PVerbatim {region;_}
| PPar {region;_}
| PRecord {region; _} | PTyped {region; _}
| PNat {region; _} | PBytes {region; _}
  -> region

let bool_expr_to_region = function
  Or {region;_} | And {region;_}
| True region | False region
| Not {region;_} -> region

let comp_expr_to_region = function
  Lt {region;_} | Leq {region;_}
| Gt {region;_} | Geq {region;_}
| Neq {region;_} | Equal {region;_} -> region

let logic_expr_to_region = function
  BoolExpr e -> bool_expr_to_region e
| CompExpr e -> comp_expr_to_region e

let arith_expr_to_region = function
  Add {region;_} | Sub {region;_} | Mult {region;_}
| Div {region;_} | Mod {region;_} | Neg {region;_}
| Int {region;_} | Mutez {region; _}
| Nat {region; _} -> region

let string_expr_to_region = function
  Verbatim {region;_} | String {region;_} | Cat {region;_} -> region

let list_expr_to_region = function
  ECons {region; _} | EListComp {region; _}
(* | Append {region; _}*) -> region

and constr_expr_to_region = function
  ENone region
| EConstrApp {region; _}
| ESomeApp   {region; _} -> region

let expr_to_region = function
  ELogic e -> logic_expr_to_region e
| EArith e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EList e -> list_expr_to_region e
| EConstr e -> constr_expr_to_region e
| EAnnot {region;_ } | ELetIn {region;_}   | EFun {region;_}
| ETypeIn {region;_ }| EModIn {region;_}   | EModAlias {region;_}
| ECond {region;_}   | ETuple {region;_}   | ECase {region;_}
| ECall {region;_}   | EVar {region; _}    | EProj {region; _}
| EUnit {region;_}   | EPar {region;_}     | EBytes {region; _}
| ESeq {region; _}   | ERecord {region; _} | EUpdate {region; _}
| EModA {region; _}
| ECodeInj {region; _} -> region

let declaration_to_region = function
  Let         {region;_}
| TypeDecl    {region;_}
| ModuleDecl  {region;_}
| ModuleAlias {region;_} -> region
| Directive d -> ExtRegion.make (Directive.to_region d) []

let selection_to_region = function
  FieldName f -> f.region
| Component c -> c.region

let path_to_region = function
  Name var -> var.region
| Path {region; _} -> region


(* Helper functions to move regions and markup a level up. *)

let update_expr_region: expr -> ExtRegion.t -> expr = 
  fun e region ->
    match e with 
      ECase     {value; _} ->                         ECase {region; value}
    | ECond     {value; _} ->                         ECond {region; value}
    | EAnnot    {value; _} ->                         EAnnot {region; value}
    | ERecord   {value; _} ->                         ERecord {region; value}
    | EProj     {value; _} ->                         EProj {region; value}
    | EModA     {value; _} ->                         EModA {region; value}
    | EUpdate   {value; _} ->                         EUpdate {region; value}
    | EVar      {value; _} ->                         EVar {region; value}
    | ECall     {value; _} ->                         ECall {region; value}
    | EBytes    {value; _} ->                         EBytes {region; value}
    | EUnit     {value; _} ->                         EUnit {region; value}
    | ETuple    {value; _} ->                         ETuple {region; value}
    | EPar      {value; _} ->                         EPar {region; value}
    | ELetIn    {value; _} ->                         ELetIn {region; value}
    | ETypeIn   {value; _} ->                         ETypeIn {region; value}
    | EModIn    {value; _} ->                         EModIn {region; value}
    | EModAlias {value; _} ->                         EModAlias {region; value}
    | EFun      {value; _} ->                         EFun {region; value}
    | ESeq      {value; _} ->                         ESeq {region; value}
    | ECodeInj  {value; _} ->                         ECodeInj {region; value}
    | ELogic    (BoolExpr (Or {value; _})) ->         ELogic (BoolExpr (Or {region; value}))
    | ELogic    (BoolExpr (And {value; _})) ->        ELogic (BoolExpr (And {region; value}))
    | ELogic    (BoolExpr (Not {value; _})) ->        ELogic (BoolExpr (Not {region; value}))
    | ELogic    (BoolExpr (True _)) ->                ELogic (BoolExpr (True region))
    | ELogic    (BoolExpr (False _)) ->               ELogic (BoolExpr (False region))
    | ELogic    (CompExpr (Lt {value; _})) ->         ELogic (CompExpr (Lt {value; region}))
    | ELogic    (CompExpr (Leq {value; _})) ->        ELogic (CompExpr (Leq {value; region}))
    | ELogic    (CompExpr (Gt {value; _})) ->         ELogic (CompExpr (Gt {value; region}))
    | ELogic    (CompExpr (Geq {value; _})) ->        ELogic (CompExpr (Geq {value; region}))
    | ELogic    (CompExpr (Equal {value; _})) ->      ELogic (CompExpr (Equal {value; region}))
    | ELogic    (CompExpr (Neq {value; _})) ->        ELogic (CompExpr (Neq {value; region}))
    | EArith    (Add {value; _}) ->                   EArith (Add {value; region})
    | EArith    (Sub {value; _}) ->                   EArith (Sub {value; region})
    | EArith    (Mult {value; _}) ->                  EArith (Mult {value; region})
    | EArith    (Div {value; _}) ->                   EArith (Div {value; region})
    | EArith    (Mod {value; _}) ->                   EArith (Mod {value; region})
    | EArith    (Neg {value; _}) ->                   EArith (Neg {value; region})
    | EArith    (Int {value; _}) ->                   EArith (Int {value; region})
    | EArith    (Nat {value; _}) ->                   EArith (Nat {value; region})
    | EArith    (Mutez {value; _}) ->                 EArith (Mutez {value; region})
    | EString   (Cat {value; _}) ->                   EString (Cat {value; region})
    | EString   (String {value; _}) ->                EString (String {value; region})
    | EString   (Verbatim {value; _}) ->              EString (Verbatim {value; region})
    | EList     (ECons {value; _}) ->                 EList (ECons {value; region})
    | EList     (EListComp {value; _}) ->             EList (EListComp {value; region})
    | EConstr   (ENone _) ->                          EConstr (ENone region)
    | EConstr   (ESomeApp {value; _}) ->              EConstr (ESomeApp {value; region})
    | EConstr   (EConstrApp {value; _}) ->            EConstr (EConstrApp {value; region})

let update_type_expr_region: type_expr -> ExtRegion.t -> type_expr = 
  fun t region ->
    match t with 
      TProd   {value; _} -> TProd {value; region}
    | TSum    {value; _} -> TSum {value; region}
    | TRecord {value; _} -> TRecord {value; region}
    | TApp    {value; _} -> TApp {value; region}
    | TFun    {value; _} -> TFun {value; region}
    | TPar    {value; _} -> TPar {value; region}
    | TVar    {value; _} -> TVar {value; region}
    | TWild   _ -> TWild region
    | TString {value; _} -> TString {value; region}
    | TInt    {value; _} -> TInt {value; region}
    | TModA   {value; _} -> TModA {value; region}
    
let update_pattern_region: pattern -> ExtRegion.t -> pattern = 
  fun p region ->
    match p with 
      PConstr   (PNone _) -> PConstr (PNone region)
    | PConstr   (PSomeApp {value; _} ) -> PConstr (PSomeApp {value; region})
    | PConstr   (PFalse _) -> PConstr (PFalse region)
    | PConstr   (PTrue _) -> PConstr (PTrue region)
    | PConstr   (PConstrApp {value; _}) -> PConstr (PConstrApp {value; region})
    | PUnit     {value; _} -> PUnit {value; region}
    | PVar      {value; _} -> PVar {value; region}
    | PInt      {value; _} -> PInt {value; region}
    | PNat      {value; _} -> PNat {value; region}
    | PBytes    {value; _} -> PBytes {value; region}
    | PString   {value; _} -> PString {value; region}
    | PVerbatim {value; _} -> PVerbatim {value; region}
    | PList     (PListComp {value; _}) -> PList (PListComp {value; region})
    | PList     (PCons {value; _}) -> PList (PCons {value; region})
    | PTuple    {value; _} -> PTuple {value; region}
    | PPar      {value; _} -> PPar {value; region}
    | PRecord   {value; _} -> PRecord {value; region}
    | PTyped    {value; _} -> PTyped {value; region}
(*
  Creates a new region that covers r1 and r2 with markup before r1 and after r2.
  Returns the new region and the modified r1 and r2.
*)
let cover_tokens r1 r2 = 
  let new_region = ExtRegion.cover r1 r2 in
  let rec extract_before (other_comments, comments) = function
    (ExtRegion.BlockCom (_, Before) as b) :: rest
  | (LineCom (_, Before) as b) :: rest ->
    extract_before (other_comments, b :: comments) rest
  | other :: rest ->
    extract_before (other :: other_comments, comments) rest
  | [] -> (other_comments, comments) 
  in
  let rec extract_after (other_comments, comments) = function
    (ExtRegion.BlockCom (_, After) as a) :: rest
  | (LineCom (_, After) as a) :: rest ->
    extract_after (other_comments, a :: comments) rest
  | other :: rest ->
    extract_after (other :: other_comments, comments) rest
  | [] -> (other_comments, comments) 
  in
  let r1_markup, before = extract_before ([], []) r1.markup in 
  let r2_markup, before_after = extract_after ([], before) r2.markup in
  let r1 = ExtRegion.make r1.t_region r1_markup in
  let r2 = ExtRegion.make r2.t_region r2_markup in
  let new_region = {new_region with markup=before_after} in
  new_region, r1, r2

type 'a update_region = {
  kregion: ExtRegion.t;
  kupdate: ExtRegion.t -> 'a
}

let c_token (t: ExtRegion.t) = {
  kregion = t;
  kupdate = fun a -> a;
}

let c_reg (r1: _ ExtRegion.reg) = {
  kregion = r1.region;
  kupdate = fun r -> {r1 with region = r}
}

let c_type_expr (t: type_expr) = {
  kregion = type_expr_to_region t;
  kupdate = update_type_expr_region t
}

let c_nseq_fst n = 
  let fst_n: _ ExtRegion.reg = fst n in
  {
    kregion = fst_n.region;
    kupdate = fun r -> (
      let fst_n = {fst_n with region = r} in
      (fst_n, snd n)
    )
  }

let c_nseq_last n (func: 'a -> 'a update_region) = 
  let rev = Utils.nseq_rev n in  
  let last_n = fst rev in
  let klast = func last_n in
  {
    kregion = klast.kregion;
    kupdate = fun r -> (
      let last_n = klast.kupdate r in
      Utils.nseq_rev (last_n, snd rev)
    )
  }

let c_nsepseq_last (n: _ Utils.nsepseq) (func: 'a -> 'a update_region) = 
  let n_rev = Utils.nsepseq_rev n in
  let last = fst n_rev in
  let klast = func last in
  {
    kregion = klast.kregion;
    kupdate = fun r -> (
      let last = klast.kupdate r in 
      Utils.nsepseq_rev (last, snd n_rev);
  )
}

let c_seq_fst s = 
  match s with 
    hd :: tl ->
      {
        kregion = hd.ExtRegion.region;
        kupdate = fun r -> {hd with region = r} :: tl
      }
  | [] -> 
      {
        kregion = ExtRegion.ghost;
        kupdate = fun _ -> []
      }

let c_expr e = {
  kregion = expr_to_region e;
  kupdate = fun r -> update_expr_region e r
}

let c_selection = function
  FieldName {value; region} ->
  {
    kregion = region;
    kupdate = fun r -> FieldName {value; region = r}
  }
| Component {value; region} ->
  {
    kregion = region;
    kupdate = fun r -> Component {value; region = r}
  }

let c_path = function 
  Name {value; region} ->
  {
    kregion = region;
    kupdate = fun r -> Name {value; region = r}
  }
| Path {value; region} ->
  {
    kregion = region;
    kupdate = fun r -> Path {value; region = r}
  }

let c_pattern p = {
  kregion = pattern_to_region p;
  kupdate = fun r -> update_pattern_region p r
}

let cover_m (a: 'a update_region) (b: 'b update_region): ExtRegion.t * 'a * 'b = 
  let region, a_region, b_region = cover_tokens a.kregion b.kregion in
  let a = a.kupdate a_region in
  let b = b.kupdate b_region in
  region, a, b

let cover_nsepseq (n: ('a, _) Utils.nsepseq) (func: 'a -> 'a update_region) : ExtRegion.t * ('a, _) Utils.nsepseq =
  let (before, _) = n in
  let (last, rest2) = Utils.nsepseq_rev n in
  let kbefore = func before in
  let klast = func last in
  let region, before_region, last_region = cover_tokens kbefore.kregion klast.kregion in
  let before = kbefore.kupdate before_region in
  let last = klast.kupdate last_region in
  let (_, rest2) = Utils.nsepseq_rev (last, rest2) in
  region, (before, rest2)

    