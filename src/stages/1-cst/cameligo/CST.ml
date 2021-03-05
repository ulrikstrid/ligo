(* Concrete Syntax Tree (CST) for CameLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30"]

(* Utilities *)

module Utils = Simple_utils.Utils
open Utils

(* Regions

   The CST carries all the regions where tokens have been found by the
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

(* Keywords of CameLIGO *)

(* IMPORTANT: The types are sorted alphabetically, except the generic
   [keyword]. If you add or modify some, please make sure they remain
   in order. *)

type keyword    = Region.t

type kwd_and    = Region.t
type kwd_begin  = Region.t
type kwd_else   = Region.t
type kwd_end    = Region.t
type kwd_false  = Region.t
type kwd_fun    = Region.t
type kwd_if     = Region.t
type kwd_in     = Region.t
type kwd_let    = Region.t
type kwd_match  = Region.t
type kwd_mod    = Region.t
type kwd_module = Region.t
type kwd_None   = Region.t
type kwd_not    = Region.t
type kwd_of     = Region.t
type kwd_or     = Region.t
type kwd_rec    = Region.t
type kwd_Some   = Region.t
type kwd_struct = Region.t
type kwd_then   = Region.t
type kwd_true   = Region.t
type kwd_type   = Region.t
type kwd_with   = Region.t

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow    = Region.t  (* "->" *)
type bool_and = Region.t  (* "&&" *)
type bool_or  = Region.t  (* "||" *)
type caret    = Region.t  (* "^"  *)
type colon    = Region.t  (* ":"  *)
type comma    = Region.t  (* ","  *)
type cons     = Region.t  (* "::" *)
type dot      = Region.t  (* "."  *)
type equal    = Region.t  (* "="  *)
type geq      = Region.t  (* ">=" *)
type gt       = Region.t  (* ">"  *)
type lbrace   = Region.t  (* "{"  *)
type lbracket = Region.t  (* "["  *)
type leq      = Region.t  (* "=<" *)
type lpar     = Region.t  (* "("  *)
type lt       = Region.t  (* "<"  *)
type minus    = Region.t  (* "-"  *)
type neq      = Region.t  (* "<>" *)
type plus     = Region.t  (* "+"  *)
type rbrace   = Region.t  (* "}"  *)
type rbracket = Region.t  (* "]"  *)
type rpar     = Region.t  (* ")"  *)
type semi     = Region.t  (* ";"  *)
type slash    = Region.t  (* "/"  *)
type times    = Region.t  (* "*"  *)
type vbar     = Region.t  (* "|"  *)
type wild     = Region.t  (* "_"  *)

(* Virtual tokens *)

type eof = Region.t

(* Literals *)

type variable    = string reg
type module_name = string reg
type fun_name    = string reg
type type_name   = string reg
type type_ctor   = string reg
type field_name  = string reg
type ctor        = string reg
type attribute   = string reg
type language    = string reg

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

(* Unit type and value *)

type the_unit = lpar * rpar

(* The Concrete Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and cst = t

(* DECLARATIONS (top-level) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Let      of let_decl     reg
| D_Module   of module_decl  reg
| D_ModAlias of module_alias reg
| D_Type     of type_decl    reg

(* Value declarations (a.k.a. let/let-rec declarations) *)

and let_decl =
  kwd_let * kwd_rec option * let_binding * attribute list

and let_binding = {
  binders  : pattern nseq;
  lhs_type : type_annot option;
  eq       : equal;
  let_rhs  : expr
}

and type_annot = colon * type_expr

(* Type declarations *)

and type_decl = {
  kwd_type  : kwd_type;
  name      : type_name;
  eq        : equal;
  type_expr : type_expr
}

(* Module declarations (structures) *)

and module_decl = {
  kwd_module   : kwd_module;
  name         : module_name;
  eq           : equal;
  kwd_struct   : kwd_struct;
  declarations : declaration nseq;
  kwd_end      : kwd_end
}

(* Declaration of module aliases *)

and module_alias = {
  kwd_module : kwd_module;
  alias      : module_name;
  eq         : equal;
  mod_path   : (module_name, dot) nsepseq;
}

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_App     of (type_ctor * type_tuple) reg
| T_Fun     of (type_expr * arrow * type_expr) reg
| T_Int     of (lexeme * Z.t) reg
| T_ModPath of type_expr module_path reg
| T_Par     of type_expr par reg
| T_Prod    of cartesian
| T_Record  of field_decl reg ne_injection reg
| T_String  of lexeme reg
| T_Sum     of sum_type reg
| T_Var     of variable
| T_Wild    of wild

(* Constructors *)

and type_tuple = (type_expr, comma) nsepseq par reg

(* Record types *)

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr;
  attributes : attribute list
}

and 'a ne_injection = {
  compound    : compound option;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option;
  attributes  : attribute list
}

and compound =
  BeginEnd of kwd_begin * kwd_end
| Braces   of lbrace * rbrace
| Brackets of lbracket * rbracket

(* Sum types *)

and sum_type = {
  lead_vbar  : vbar option;
  variants   : (variant reg, vbar) nsepseq;
  attributes : attribute list
}

and cartesian = (type_expr, times) nsepseq reg

and variant = {
  ctor       : ctor;
  arg        : (kwd_of * type_expr) option;
  attributes : attribute list
}

(* Module path as a type expression *)

and 'a module_path = {
  module_path : (module_name, dot) nsepseq;
  selector    : dot;
  field       : 'a
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_Bytes    of (lexeme * Hex.t) reg
| P_Cons     of (pattern, cons) nsepseq reg
| P_Ctor     of (ctor * pattern option) reg
| P_False    of kwd_false
| P_Int      of (lexeme * Z.t) reg
| P_List     of pattern injection reg
| P_Nat      of (lexeme * Z.t) reg
| P_None     of kwd_None
| P_Par      of pattern par reg
| P_Record   of field_pattern reg ne_injection reg
| P_Some     of (kwd_Some * pattern) reg
| P_String   of string reg
| P_True     of kwd_true
| P_Tuple    of (pattern, comma) nsepseq reg
| P_Typed    of typed_pattern reg
| P_Unit     of the_unit reg
| P_Var      of variable
| P_Wild     of wild

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

(* EXPRESSIONS *)

and expr =
  ECase     of expr case reg
| ECond     of cond_expr reg
| EAnnot    of annot_expr par reg
| ELogic    of logic_expr
| EArith    of arith_expr
| EString   of string_expr
| EList     of list_expr
| ECtor     of ctor_expr
| ERecord   of record reg
| EProj     of projection reg
| EModPath   of expr module_path reg
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

and annot_expr = expr * type_annot

and 'a injection = {
  compound   : compound option;
  elements   : ('a, semi) sepseq;
  terminator : semi option
}

and list_expr =
  ECons     of cons bin_op reg
| EListComp of expr injection reg

and string_expr =
  Cat      of caret bin_op reg
| String   of string reg
| Verbatim of string reg

and ctor_expr =
  ENone    of kwd_None
| ESomeApp of (kwd_Some * expr) reg
| ECtorApp of (ctor * expr option) reg

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

and record = field_assignment reg ne_injection

and projection = {
  struct_name : variable;
  selector    : dot;
  field_path  : (selection, dot) nsepseq
}

and selection =
  FieldName of variable
| Component of (string * Z.t) reg

and field_assignment = {
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
  attributes : attribute list
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
  lhs_type   : type_annot option;
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
  language : language reg;
  code     : expr;
  rbracket : rbracket;
}

(* Projecting regions from some nodes of the CST *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

let type_expr_to_region = function
  T_Prod   {region; _}
| T_Sum    {region; _}
| T_Record {region; _}
| T_App    {region; _}
| T_Fun    {region; _}
| T_Par    {region; _}
| T_String {region; _}
| T_Int    {region; _}
| T_Var    {region; _}
| T_Wild    region
| T_ModA   {region; _}
   -> region

let list_pattern_to_region = function
  PListComp {region; _} | PCons {region; _} -> region

let ctor_pattern_to_region = function
  P_None region | P_SomeApp {region;_}
| P_True region | P_False region
| P_CtorApp {region;_} -> region

let pattern_to_region = function
| P_List p -> list_pattern_to_region p
| P_Ctor c -> ctor_pattern_to_region c
| P_Unit {region;_}
| P_Tuple {region;_} | P_Var {region;_}
| P_Int {region;_}
| P_String {region;_} | P_Verbatim {region;_}
| P_Wild region | P_P_ar {region;_}
| P_Record {region; _} | P_Typed {region; _}
| P_Nat {region; _} | P_Bytes {region; _}
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

and ctor_expr_to_region = function
  ENone region
| ECtorApp {region; _}
| ESomeApp {region; _} -> region

let expr_to_region = function
  E_Logic e  -> logic_expr_to_region e
| E_Arith e  -> arith_expr_to_region e
| E_String e -> string_expr_to_region e
| E_List e   -> list_expr_to_region e
| E_Ctor e   -> ctor_expr_to_region e
| E_Annot {region;_}  | E_LetIn {region;_}  | E_Fun {region;_}
| E_TypeIn {region;_} | E_ModIn {region;_}  | E_ModAlias {region;_}
| E_Cond {region;_}   | E_Tuple {region;_}  | E_Case {region;_}
| E_Call {region;_}   | E_Var {region;_}    | E_Proj {region;_}
| E_Unit {region;_}   | E_Par {region;_}    | E_Bytes {region;_}
| E_Seq {region;_}    | E_Record {region;_} | E_Update {region;_}
| E_ModA {region;_}   | E_CodeInj {region;_}
  -> region

let declaration_to_region = function
  Let         {region;_}
| TypeDecl    {region;_}
| ModuleDecl  {region;_}
| ModuleAlias {region;_} -> region

let selection_to_region = function
  FieldName f -> f.region
| Component c -> c.region

let path_to_region = function
  Name var  -> var.region
| Path path -> path.region
