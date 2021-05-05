(* Concrete Syntax Tree (CST) for PascaLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-42-30"]

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

open Region
open Utils

(* Lexemes *)

type lexeme = string

(* Keywords of PascaLIGO *)

(* IMPORTANT: The types are sorted alphabetically, except the generic
   [keyword]. If you add or modify some, please make sure they remain
   in order. *)

type keyword       = Region.t

type kwd_and       = Region.t
type kwd_begin     = Region.t
type kwd_block     = Region.t
type kwd_case      = Region.t
type kwd_const     = Region.t
type kwd_contains  = Region.t
type kwd_down      = Region.t
type kwd_else      = Region.t
type kwd_end       = Region.t
type kwd_False     = Region.t
type kwd_for       = Region.t
type kwd_from      = Region.t
type kwd_function  = Region.t
type kwd_if        = Region.t
type kwd_in        = Region.t
type kwd_is        = Region.t
type kwd_list      = Region.t
type kwd_map       = Region.t
type kwd_mod       = Region.t
type kwd_module    = Region.t
type kwd_nil       = Region.t
type kwd_None      = Region.t
type kwd_not       = Region.t
type kwd_of        = Region.t
type kwd_or        = Region.t
type kwd_patch     = Region.t
type kwd_record    = Region.t
type kwd_recursive = Region.t
type kwd_remove    = Region.t
type kwd_set       = Region.t
type kwd_Some      = Region.t
type kwd_skip      = Region.t
type kwd_step      = Region.t
type kwd_then      = Region.t
type kwd_to        = Region.t
type kwd_True      = Region.t
type kwd_type      = Region.t
type kwd_Unit      = Region.t
type kwd_var       = Region.t
type kwd_while     = Region.t
type kwd_with      = Region.t

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow    = Region.t  (* "->"  *)
type assign   = Region.t  (* ":="  *)
type caret    = Region.t  (* "^"   *)
type colon    = Region.t  (* ":"   *)
type comma    = Region.t  (* ","   *)
type cons     = Region.t  (* "#"   *)
type dot      = Region.t  (* "."   *)
type equal    = Region.t  (* "="   *)
type geq      = Region.t  (* ">="  *)
type gt       = Region.t  (* ">"   *)
type lbrace   = Region.t  (* "{"   *)
type lbracket = Region.t  (* "["   *)
type leq      = Region.t  (* "<="  *)
type lpar     = Region.t  (* "("   *)
type lt       = Region.t  (* "<"   *)
type minus    = Region.t  (* "-"   *)
type neq      = Region.t  (* "=/=" *)
type plus     = Region.t  (* "+"   *)
type rbrace   = Region.t  (* "}"   *)
type rbracket = Region.t  (* "]"   *)
type rpar     = Region.t  (* ")"   *)
type semi     = Region.t  (* ";"   *)
type slash    = Region.t  (* "/"   *)
type times    = Region.t  (* "*"   *)
type vbar     = Region.t  (* "|"   *)
type wild     = Region.t  (* "_"   *)

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

(* Brackets compounds *)

type 'a brackets = {
  lbracket : lbracket;
  inside   : 'a;
  rbracket : rbracket
}

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
  D_Const     of const_decl   reg
| D_Directive of Directive.t
| D_Fun       of fun_decl     reg
| D_Module    of module_decl  reg
| D_ModAlias  of module_alias reg
| D_Type      of type_decl    reg

(* Declarations of constants *)

and const_decl = {
  kwd_const  : kwd_const;
  pattern    : pattern;
  equal      : equal;
  init       : expr;
  terminator : semi option;
  attributes : attribute list
}

(* Type declarations *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  kwd_is     : kwd_is;
  type_expr  : type_expr;
  terminator : semi option
}

(* Module declarations (structures) *)

and module_decl = {
  kwd_module   : kwd_module;
  name         : module_name;
  kwd_is       : kwd_is;
  enclosing    : block_enclosing;
  declarations : declaration nseq;
  terminator   : semi option;
}

(* Declaration of module aliases *)

and module_alias = {
  kwd_module : kwd_module;
  alias      : module_name;
  kwd_is     : kwd_is;
  mod_path   : (module_name, dot) nsepseq;
  terminator : semi option;
}

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_Ctor    of (type_ctor * type_tuple) reg
| T_Fun     of (type_expr * arrow * type_expr) reg
| T_Int     of (lexeme * Z.t) reg
| T_ModPath of type_name module_path reg
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
  field_type : type_annot option;
  attributes : attribute list
}

and 'a ne_injection = {
  kind        : ne_injection_kwd;
  enclosing   : enclosing;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option;
  attributes  : attribute list
}

and ne_injection_kwd = [
  `Set    of keyword
| `Map    of keyword
| `Record of keyword
]

and enclosing =
  Brackets of lbracket * rbracket
| End      of kwd_end

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

(* Function and procedure declarations *)

and fun_decl = {
  kwd_recursive : kwd_recursive option;
  kwd_function  : kwd_function;
  fun_name      : variable;
  param         : parameters;
  ret_type      : type_annot option;
  kwd_is        : kwd_is;
  return        : expr;
  terminator    : semi option;
  attributes    : attribute list
}

and parameters = (param_decl, semi) nsepseq par reg

and param_decl =
  ParamConst of param_const reg
| ParamVar   of param_var reg

and param_const = {
  kwd_const  : kwd_const;
  var        : variable;
  param_type : type_annot option
}

and param_var = {
  kwd_var    : kwd_var;
  var        : variable;
  param_type : type_annot option
}

and type_annot = colon * type_expr

(* Module path as a type expression *)

and 'a module_path = {
  module_path : (module_name, dot) nsepseq;
  selector    : dot;
  field       : 'a
}

(* STATEMENTS *)

and statement =
  S_Instr   of instruction
| S_Decl    of declaration
| S_VarDecl of var_decl reg

and statements = (statement, semi) nsepseq

and var_decl = {
  kwd_var    : kwd_var;
  pattern    : pattern;
  assign     : assign;
  init       : expr;
  terminator : semi option;
}

(* Instructions *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and instruction =
  I_Assign      of assignment reg
| I_Call        of call
| I_Case        of test_clause case reg
| I_Cond        of (test_clause, test_clause) conditional reg
| I_For         of for_int reg
| I_ForIn       of for_in reg
| I_MapPatch    of map_patch reg
| I_MapRemove   of map_remove reg
| I_RecordPatch of record_patch reg
| I_Skip        of kwd_skip
| I_SetPatch    of set_patch reg
| I_SetRemove   of set_remove reg
| I_While       of while_loop reg

and set_remove = {
  kwd_remove : kwd_remove;
  element    : expr;
  kwd_from   : kwd_from;
  kwd_set    : kwd_set;
  set        : path
}

and map_remove = {
  kwd_remove : kwd_remove;
  key        : expr;
  kwd_from   : kwd_from;
  kwd_map    : kwd_map;
  map        : path
}

and set_patch  = {
  kwd_patch : kwd_patch;
  path      : path;
  kwd_with  : kwd_with;
  set_inj   : expr ne_injection reg
}

and map_patch  = {
  kwd_patch : kwd_patch;
  path      : path;
  kwd_with  : kwd_with;
  map_inj   : binding reg ne_injection reg
}

and binding = {
  source : expr;
  arrow  : arrow;
  image  : expr
}

and record_patch = {
  kwd_patch  : kwd_patch;
  path       : path;
  kwd_with   : kwd_with;
  record_inj : record_expr reg
}

and ('ifso, 'ifnot) conditional = {
  kwd_if     : kwd_if;
  test       : expr;
  kwd_then   : kwd_then;
  ifso       : 'ifso;
  ifnot      : (kwd_else * 'ifnot) option
}

and test_clause =
  ClauseInstr of instruction
| ClauseBlock of block reg

and block = {
  enclosing  : block_enclosing;
  statements : statements;
  terminator : semi option
}

and block_enclosing =
  Braces   of kwd_block option * lbrace * rbrace
| BeginEnd of kwd_begin * kwd_end

and set_mem = {
  set          : expr;
  kwd_contains : kwd_contains;
  element      : expr
}

and 'a case = {
  kwd_case  : kwd_case;
  expr      : expr;
  kwd_of    : kwd_of;
  enclosing : enclosing;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) nsepseq reg
}

and 'a case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and assignment = {
  lhs    : lhs;
  assign : assign;
  rhs    : expr
}

and lhs =
  Path    of path
| MapPath of map_lookup reg

and while_loop = {
  kwd_while : kwd_while;
  cond      : expr;
  block     : block reg
}

and for_int = {
  kwd_for : kwd_for;
  binder  : variable;
  assign  : assign;
  init    : expr;
  kwd_to  : kwd_to;
  bound   : expr;
  step    : (kwd_step * expr) option;
  block   : block reg
}

and for_in = {
  kwd_for    : kwd_for;
  var        : variable;
  bind_to    : (arrow * variable) option;
  kwd_in     : kwd_in;
  collection : collection;
  expr       : expr;
  block      : block reg
}

and collection = [
  `List of kwd_list
| `Map  of kwd_map
| `Set  of kwd_set
]

(* Code injection. Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>",
   whereas the innermost covers the <language> part. *)

and code_inj = {
  language : language reg;
  code     : expr;
  rbracket : rbracket;
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_Bytes  of (lexeme * Hex.t) reg
| P_Cons   of (pattern, cons) nsepseq reg
| P_Ctor   of (ctor * tuple_pattern option) reg
| P_False  of kwd_False
| P_Int    of (lexeme * Z.t) reg
| P_List   of pattern injection reg
| P_Nat    of (lexeme * Z.t) reg
| P_Nil    of kwd_nil
| P_None   of kwd_None
| P_Par    of pattern par reg
| P_Record of pattern field reg ne_injection reg
| P_Some   of (kwd_Some * pattern par reg) reg
| P_String of lexeme reg
| P_True   of kwd_True
| P_Tuple  of tuple_pattern
| P_Typed  of typed_pattern reg
| P_Unit   of kwd_Unit
| P_Var    of lexeme reg

and tuple_pattern = (pattern, comma) nsepseq par reg

and typed_pattern = {
  pattern    : pattern;
  type_annot : type_annot
}

and 'rhs field =
  Punned   of field_name
| Complete of 'rhs full_field

and 'rhs full_field = {
  field_name : field_name;
  assign     : equal;
  field_rhs  : 'rhs;
  attributes : attribute list
}

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add       of plus bin_op reg               (* "+"   *)
| E_And       of kwd_and bin_op reg            (* "and" *)
| E_BigMap    of binding reg injection reg
| E_Block     of block_with reg
| E_Bytes     of (lexeme * Hex.t) reg
| E_Call      of call
| E_Case      of expr case reg
| E_Cat       of caret bin_op reg              (* "^"   *)
| E_CodeInj   of code_inj reg
| E_Equal     of equal bin_op reg              (* "="   *)
| E_Cond      of (expr, expr) conditional reg
| E_Cons      of cons bin_op reg
| E_Ctor      of (ctor * arguments option) reg
| E_Div       of slash bin_op reg              (* "/"   *)
| E_False     of kwd_False
| E_Fun       of fun_expr reg
| E_Geq       of geq bin_op reg                (* ">="  *)
| E_Gt        of gt bin_op reg                 (* ">"   *)
| E_Int       of (lexeme * Z.t) reg
| E_Leq       of leq bin_op reg                (* "<="  *)
| E_List      of expr injection reg
| E_Lt        of lt bin_op reg                 (* "<"   *)
| E_Map       of binding reg injection reg
| E_MapLookup of map_lookup reg
| E_Mod       of kwd_mod bin_op reg            (* "mod" *)
| E_ModPath   of expr module_path reg
| E_Mult      of times bin_op reg              (* "*"   *)
| E_Mutez     of (lexeme * Z.t) reg
| E_Nat       of (lexeme * Z.t) reg
| E_Neg       of minus un_op reg               (* "-a"  *)
| E_Nil       of kwd_nil
| E_Neq       of neq bin_op reg                (* "=/=" *)
| E_None      of kwd_None
| E_Not       of kwd_not un_op reg             (* "not" *)
| E_Or        of kwd_or bin_op reg             (* "or"  *)
| E_Par       of expr par reg
| E_Proj      of projection reg
| E_Record    of record_expr reg
| E_Set       of expr injection reg
| E_SetMem    of set_mem reg
| E_Some      of (kwd_Some * arguments) reg
| E_String    of lexeme reg
| E_Sub       of minus bin_op reg              (* "a-b" *)
| E_True      of kwd_True
| E_Tuple     of tuple_expr
| E_Typed     of typed_expr par reg
| E_Unit      of kwd_Unit
| E_Update    of update reg
| E_Var       of lexeme reg
| E_Verbatim  of lexeme reg

and block_with = {
  block    : block reg;
  kwd_with : kwd_with;
  expr     : expr
}

and fun_expr = {
  kwd_function : kwd_function;
  param        : parameters;
  ret_type     : type_annot option;
  kwd_is       : kwd_is;
  return       : expr
}

and typed_expr = expr * type_annot

and map_lookup = {
  path  : path;
  index : expr brackets reg
}

and path =
  Name of variable
| Path of projection reg

and projection = {
  record_name : variable;
  selector    : dot;
  field_path  : (selection, dot) nsepseq
}

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and record_expr = expr field reg ne_injection

and update = {
  record   : path;
  kwd_with : kwd_with;
  updates  : field_path_assignment reg ne_injection reg
}

and field_path_assignment = {
  field_path : path;
  assign     : equal;
  field_expr : expr
}

and selection =
  FieldName of field_name
| Component of (lexeme * Z.t) reg

and tuple_expr = (expr, comma) nsepseq par reg

and call = (expr * arguments) reg

and arguments = tuple_expr

(* Injections *)

and 'a injection = {
  kind       : injection_kwd;
  enclosing  : enclosing;
  elements   : ('a, semi) sepseq;
  terminator : semi option
}

and injection_kwd = [
  `BigMap of keyword
| `List   of keyword
| `Map    of keyword
| `Set    of keyword
]

(* PROJECTING REGIONS *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd, tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd, tl) =
  Region.cover (to_region hd) (last (to_region <@ snd) tl)

let sepseq_to_region to_region = function
      None -> Region.ghost
| Some seq -> nsepseq_to_region to_region seq

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let type_expr_to_region = function
  T_Ctor    {region; _}
| T_Fun     {region; _}
| T_Int     {region; _}
| T_ModPath {region; _}
| T_Par     {region; _}
| T_Prod    {region; _}
| T_Record  {region; _}
| T_String  {region; _}
| T_Sum     {region; _}
| T_Var     {region; _}
| T_Wild     region
  -> region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let expr_to_region = function
  E_Add       {region; _}
| E_And       {region; _}
| E_Typed     {region; _}
| E_BigMap    {region; _}
| E_Block     {region; _}
| E_Bytes     {region; _}
| E_Call      {region; _}
| E_Case      {region;_}
| E_Cat       {region; _}
| E_CodeInj   {region; _}
| E_Equal     {region; _}
| E_Cond      {region; _}
| E_Cons      {region; _}
| E_Ctor      {region; _}
| E_Div       {region; _}
| E_False      region
| E_Fun       {region; _}
| E_Geq       {region; _}
| E_Gt        {region; _}
| E_Int       {region; _}
| E_Leq       {region; _}
| E_List      {region; _}
| E_Lt        {region; _}
| E_Map       {region; _}
| E_MapLookup {region; _}
| E_Mod       {region; _}
| E_ModPath   {region; _}
| E_Mult      {region; _}
| E_Mutez     {region; _}
| E_Nat       {region; _}
| E_Neg       {region; _}
| E_Nil        region
| E_Neq       {region; _}
| E_None       region
| E_Not       {region; _}
| E_Or        {region; _}
| E_Par       {region; _}
| E_Proj      {region; _}
| E_Record    {region; _}
| E_Set       {region; _}
| E_SetMem    {region; _}
| E_Some      {region; _}
| E_String    {region; _}
| E_Sub       {region; _}
| E_True       region
| E_Tuple     {region; _}
| E_Unit       region
| E_Update    {region; _}
| E_Var       {region; _}
| E_Verbatim  {region; _}
  -> region

and tuple_expr_to_region x = x.Region.region

and typed_expr_to_region x = x.Region.region

and record_expr_to_region x = x.Region.region

let path_to_region = function
  Name var  -> var.region
| Path path -> path.region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let instr_to_region = function
  I_Assign      {region; _}
| I_Call        {region; _}
| I_Case        {region; _}
| I_Cond        {region; _}
| I_For         {region; _}
| I_ForIn       {region; _}
| I_MapPatch    {region; _}
| I_MapRemove   {region; _}
| I_RecordPatch {region; _}
| I_Skip         region
| I_SetPatch    {region; _}
| I_SetRemove   {region; _}
| I_While       {region; _}
  -> region

let test_clause_to_region = function
  ClauseInstr instr -> instr_to_region instr
| ClauseBlock block -> block.Region.region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let pattern_to_region = function
  P_Bytes   {region; _}
| P_Cons    {region; _}
| P_Ctor    {region; _}
| P_False    region
| P_Int     {region; _}
| P_List    {region; _}
| P_Nat     {region; _}
| P_Nil      region
| P_None     region
| P_Par     {region; _}
| P_Record  {region; _}
| P_Some    {region; _}
| P_String  {region; _}
| P_True     region
| P_Tuple   {region; _}
| P_Typed   {region; _}
| P_Unit     region
| P_Var     {region; _}
  -> region

let declaration_to_region = function
  D_Const    {region; _}
| D_Fun      {region; _}
| D_Module   {region; _}
| D_ModAlias {region; _}
| D_Type     {region; _} -> region
| D_Directive d -> Directive.to_region d

let lhs_to_region : lhs -> Region.t = function
  Path    path -> path_to_region path
| MapPath path -> path.region

let selection_to_region = function
  FieldName {region; _}
| Component {region; _} -> region
