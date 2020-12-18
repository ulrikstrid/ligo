(* Concrete Syntax Tree (CST) for JsLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30-40-42"]

(* Utilities *)

module Utils = Simple_utils.Utils
open Utils

(* Regions

   The AST carries all the regions where tokens have been found by the
   lexer, plus additional regions corresponding to whole subtrees
   (like entire expressions, patterns etc.). These regions are needed
   for error reporting and source-to-source transformations. To make
   these pervasive regions more legible, we define singleton types for
   the symbols, keywords etc. with suggestive names like "kwd_and"
   denoting the _region_ of the occurrence of the keyword "and".
*)

module Region = Simple_utils.Region

type 'a reg = 'a Region.reg

(* Lexemes *)

type lexeme = string
type field_name = string reg

(* Keywords of Reason *)

type kwd_else    = Region.t
type kwd_false   = Region.t
type kwd_if      = Region.t
type kwd_let     = Region.t
type kwd_const   = Region.t
type kwd_or      = Region.t
type kwd_then    = Region.t
type kwd_true    = Region.t
type kwd_type    = Region.t
type kwd_return  = Region.t
type kwd_switch  = Region.t
type kwd_case    = Region.t
type kwd_default = Region.t
type kwd_unit    = Region.t
type kwd_new     = Region.t
type kwd_as      = Region.t

(* Data constructors *)

type c_None  = Region.t
type c_Some  = Region.t

(* Symbols *)

type arrow    = Region.t  (* "=>"  *)
type dot      = Region.t  (* "."   *)
type ellipsis = Region.t  (* "..." *)
type equal    = Region.t  (* "="   *)

(* Arithmetic operators *)

type minus      = Region.t  (* "-" *)
type plus       = Region.t  (* "+" *)
type slash      = Region.t  (* "/" *)
type modulo     = Region.t  (* "%" *)
type times      = Region.t  (* "*" *)

(* Boolean operators *)

type bool_or  = Region.t  (* "||" *)
type bool_and = Region.t  (* "&&" *)

(* Comparisons *)

type negate    = Region.t  (* "!"  *)
type equal_cmp = Region.t  (* "=="  *)
type neq       = Region.t  (* "!=" *)
type lt        = Region.t  (* "<"  *)
type gt        = Region.t  (* ">"  *)
type leq       = Region.t  (* "<=" *)
type geq       = Region.t  (* ">=" *)

(* Compounds *)

type lpar     = Region.t  (* "(" *)
type rpar     = Region.t  (* ")" *)
type lbracket = Region.t  (* "[" *)
type rbracket = Region.t  (* "]" *)
type lbrace   = Region.t  (* "{" *)
type rbrace   = Region.t  (* "}" *)

(* Separators *)

type comma = Region.t  (* "," *)
type semi  = Region.t  (* ";" *)
type vbar  = Region.t  (* "|" *)
type colon = Region.t  (* ":" *)

(* Wildcard *)

type wild = Region.t  (* "_" *)

(* Virtual tokens *)

type eof = Region.t

(* Literals *)

type variable    = string reg
type fun_name    = string reg
type type_name   = string reg
type type_constr = string reg
type constr      = string reg
type attribute   = string reg

(* Parentheses *)

type 'a braced = {
  lbrace : lbrace;
  inside : 'a;
  rbrace : rbrace
}

type 'a brackets = {
  lbracket : lbracket;
  inside   : 'a;
  rbracket : rbracket
}

type 'a chevrons = {
  lchevron   : lt;
  inside     : 'a;
  rchevron   : gt
}

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

type the_unit = lpar * rpar

(* The Abstract Syntax Tree *)

type t = {
  statements : statements;
  eof  : eof
}

and statements = (statement, semi) nsepseq

and ast = t

and attributes = attribute list

(* Non-recursive values *)

and let_rhs = {
  eq       : equal;
  expr     : expr
}

and let_binding = {
  binders  : binding_pattern;
  lhs_type : (colon * type_expr) option;
  let_rhs  : let_rhs option;
}

(* Type declarations *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  eq         : equal;
  type_expr  : type_expr
}

and fun_type_arg = {
  name      : variable;
  colon     : colon;
  type_expr : type_expr
}

and fun_type_args = (fun_type_arg, comma) nsepseq par

and type_expr =
  TProd   of cartesian
| TSum    of sum_type reg
| TObject of field_decl reg ne_injection reg
| TApp    of (type_constr * type_tuple) reg
| TFun    of (fun_type_args * arrow * type_expr) reg
| TPar    of type_expr par reg
| TVar    of variable
| TWild   of wild
| TString of lexeme reg

and cartesian = (type_expr, comma) nsepseq brackets reg

and sum_type = {
  lead_vbar  : vbar option;
  variants   : (variant reg, vbar) nsepseq;
  attributes : attributes
}

and pattern 

and variant =
| VString of string reg
| VVar    of variable

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr;
  attributes : attributes
}

and type_tuple = (type_expr, comma) nsepseq chevrons reg

and rest_pattern = {
  ellipsis  : ellipsis;
  rest      : variable
}

and assign_pattern = {
  property  : variable;
  eq        : equal;
  value     : expr
}

and destruct = {
  property  : variable;
  colon     : colon;
  target    : let_binding reg;
}

and binding_pattern =
  PRest     of rest_pattern reg
| PAssign   of assign_pattern reg
| PVar      of variable
| PConstr   of variable
| PDestruct of destruct reg
| PObject   of (binding_pattern, comma) nsepseq braced reg
| PWild
| PArray    of (binding_pattern, comma) nsepseq brackets reg

and string_expr =
  String   of string reg
| Verbatim of string reg

and return = {
  kwd_return: kwd_return;
  expr: expr option;
}

and switch = {
  kwd_switch  : kwd_switch;
  lpar        : lpar;
  expr        : expr;
  rpar        : rpar;
  lbrace      : lbrace;
  cases       : switch_case nseq;
  rbrace      : rbrace;
}

and switch_case =
  Switch_case of {
    kwd_case    : kwd_case;
    expr        : expr;
    colon       : colon;
    statements  : statements option;
  }
| Switch_default_case of {
    kwd_default : kwd_default;
    colon       : colon;
    statements  : statements option;
  }

and array_item_rest = {
  ellipsis : ellipsis;
  expr     : expr
}

and array_item =
  | Empty_entry
  | Expr_entry of expr
  | Rest_entry of array_item_rest reg

and property2 = {
  name  : expr;
  colon : colon;
  value : expr
}

and property_rest = {
  ellipsis : ellipsis;
  expr     : expr
}

and property =
  Punned_property of expr reg
| Property of property2 reg
| Property_rest of property_rest reg

and expr =
  EFun     of fun_expr reg
| EPar     of expr par reg
| ESeq     of (expr, comma) nsepseq reg
| EVar     of variable
| EConstr  of variable
| ELogic   of logic_expr
| EArith   of arith_expr
| ECall    of (expr * arguments) reg
| ENew     of (kwd_new * expr) reg
| EBytes   of (string * Hex.t) reg
| EArray   of (array_item, comma) nsepseq brackets reg
| EObject  of (property, comma) nsepseq braced reg
| EString  of string_expr
| EProj    of projection reg
| EAssign  of expr * equal * expr

| EAnnot   of annot_expr reg
| EUnit    of the_unit reg
| ECodeInj of code_inj reg

and statement =
| SBlock      of (statement, semi) nsepseq braced reg
| SVar        of variable
| SExpr       of expr
| SCond       of cond_statement reg
| SReturn     of return reg
| SLet        of let_ reg
| SConst      of const_ reg
| SType       of type_decl reg
| SSwitch     of switch reg

and arguments =
  Multiple of (expr,comma) nsepseq par reg
| Unit     of the_unit reg

and annot_expr = expr * kwd_as * type_expr

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
| Braces   of lbrace * rbrace
| Brackets of lbracket * rbracket

and arith_expr =
  Add   of plus bin_op reg
| Sub   of minus bin_op reg
| Mult  of times bin_op reg
| Div   of slash bin_op reg
| Mod   of modulo bin_op reg
| Neg   of minus un_op reg
| Int   of (string * Z.t) reg
| Nat   of (string * Z.t) reg
| Mutez of (string * Z.t) reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or    of bool_or bin_op reg
| And   of bool_and bin_op reg
| Not   of negate un_op reg
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
  Lt    of lt        bin_op reg
| Leq   of leq       bin_op reg
| Gt    of gt        bin_op reg
| Geq   of geq       bin_op reg
| Equal of equal_cmp bin_op reg
| Neq   of neq       bin_op reg

and projection = {
  expr      : expr;
  selection : selection;
}

and selection_field_name =
  { dot: dot; value : variable }

and selection =
  FieldName of selection_field_name reg
| Component of expr brackets reg

and let_ = {
  kwd_let    : kwd_let;
  bindings   : (let_binding reg, comma) nsepseq;
  attributes : attributes
}

and const_ = {
  kwd_const  : kwd_const;
  bindings   : (let_binding reg, comma) nsepseq;
  attributes : attributes
}

and fun_expr_body =
  FunctionBody of statements braced reg
| ExpressionBody of expr

and fun_expr = {
  parameters : expr;
  lhs_type   : (colon * type_expr) option;
  arrow      : arrow;
  body       : fun_expr_body;
}

and cond_statement = {
  kwd_if   : kwd_if;
  test     : expr par;
  ifso     : statement;
  ifnot    : (kwd_else * statement) option;
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
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

let type_expr_to_region = function
  TProd   {region; _}
| TSum    {region; _}
| TObject {region; _}
| TApp    {region; _}
| TFun    {region; _}
| TPar    {region; _}
| TString {region; _}
| TVar    {region; _}
| TWild    region
 -> region

let binding_pattern_to_region = function
  PRest {region;_ }   | PAssign {region ;_ }
| PVar {region ;_ }    | PConstr {region; _ } | PDestruct {region ;_ }
| PObject {region ;_ } | PArray {region; _} -> region
| PWild -> Region.ghost

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
  Verbatim {region;_} | String {region;_} -> region

let rec expr_to_region = function
  ELogic e -> logic_expr_to_region e
| EArith e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EAssign (f, _, e) -> Region.cover (expr_to_region f) (expr_to_region e)
| EAnnot {region;_ } | EFun {region;_}
| ECall {region;_}   | EVar {region; _}    | EProj {region; _}
| EUnit {region;_}   | EPar {region;_}     | EBytes {region; _}
| ESeq {region; _}   | EObject {region; _} | EArray { region; _}
| ENew {region; _}   | EConstr {region; _}
| ECodeInj {region; _} -> region

let statement_to_region = function
  SExpr e -> expr_to_region e
| SBlock {region; _ }
| SVar {region; _}
| SCond {region; _}
| SReturn {region; _}
| SLet  {region; _}
| SConst {region; _}
| SSwitch {region; _}
| SType {region; _} -> region

let selection_to_region = function
  FieldName f -> f.region
| Component c -> c.region

let arrow_function_body_to_region = function
  FunctionBody {region; _} -> region
| ExpressionBody s -> expr_to_region s
