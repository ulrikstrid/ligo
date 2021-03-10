%{
(* START HEADER *)

[@@@warning "-42"]

open Simple_utils.Region
module CST = Cst.Cameligo
open CST

(* Utilities *)

let first_region = function
    [] -> None
| x::_ -> Some x.Region.region

let mk_mod_path :
  (module_name, dot) Utils.nseq * 'a ->
  ('a -> Region.t) ->
  'a CST.module_path Region.reg =
  fun (nseq, field) to_region ->
    let (first, sep), tail = nseq in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item, next_sep) :: others ->
        trans ((prev_sep, item) :: seq, next_sep) others in
    let list, last_dot = trans ([], sep) tail in
    let module_path = first, List.rev list in
    let region = nsepseq_to_region to_region nsepseq
    and value = {module_path; selector=last_dot; field}
    in {value; region}

(* END HEADER *)
%}

(* Reductions on error *)

%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce call_expr_level
%on_error_reduce add_expr_level
%on_error_reduce cons_expr_level
%on_error_reduce cat_expr_level
%on_error_reduce disj_expr_level
%on_error_reduce conj_expr_level
%on_error_reduce bin_op(conj_expr_level,BOOL_AND,comp_expr_level)
%on_error_reduce bin_op(disj_expr_level,Or,conj_expr_level)
%on_error_reduce bin_op(disj_expr_level,BOOL_OR,conj_expr_level)
%on_error_reduce base_expr(expr)
%on_error_reduce base_expr(base_cond)
%on_error_reduce module_var_e
%on_error_reduce module_var_t
%on_error_reduce nsepseq(module_name,DOT)
%on_error_reduce core_expr
%on_error_reduce match_expr(base_cond)
%on_error_reduce ctor_expr
%on_error_reduce nsepseq(disj_expr_level,COMMA)
%on_error_reduce const_ctor
%on_error_reduce arguments
%on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level)
%on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level)
%on_error_reduce seq(Attr)
%on_error_reduce ctor_pattern
%on_error_reduce tail
%on_error_reduce nsepseq(sub_irrefutable,COMMA)
%on_error_reduce irrefutable
%on_error_reduce variant
%on_error_reduce nsepseq(variant,VBAR)
%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce fun_type
%on_error_reduce cartesian
%on_error_reduce sub_irrefutable

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <CST.t> contract
%type <CST.expr> interactive_expr

%%

(* RULES *)

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1 $3
    and value  = {lpar=$1; inside=$2; rpar=$3}
    in {region; value} }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. See module [Utils] for the types
   corresponding to the semantic actions of those rules.
 *)

(* Possibly empty sequence of items *)

seq(item):
  (**)           {     [] }
| item seq(item) { $1::$2 }

(* Non-empty sequence of items *)

nseq(item):
  item seq(item) { $1,$2 }

(* Non-empty separated sequence of items *)

nsepseq(item,sep):
  item                       {                        $1, [] }
| item sep nsepseq(item,sep) { let h,t = $3 in $1, ($2,h)::t }

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. *)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
    $1, None
  }
| nseq(item sep {$1,$2}) {
    let (first,sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item,next_sep)::others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Helpers *)

%inline type_name   : "<ident>"  { $1 }
%inline field_name  : "<ident>"  { $1 }
%inline struct_name : "<ident>"  { $1 }
%inline module_name : "<uident>" { $1 }
%inline ctor        : "<uident>" { $1 }

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empty semicolon-separated values between brackets *)

list__(item):
  "[" sep_or_term_list(item,";")? "]" {
    let compound = Some (Brackets ($1,$3))
    and region = cover $1 $3 in
    let elements, terminator =
      match $2 with
        None -> None, None
      | Some (elements, terminator) ->
          Some elements, terminator in
    let value = {compound; elements; terminator}
    in {region; value} }

(* Main *)

contract:
  declarations EOF { {decl=$1; eof=$2} }

(* Declarations *)

declarations:
  nseq(declaration) { $1 }

declaration:
  type_decl    { D_Type     $1 }
| let_decl     { D_Let      $1 }
| module_decl  { D_Module   $1 }
| module_alias { D_ModAlias $1 }

(* Module declarations *)

module_decl:
  "module" module_name "=" "struct" declarations "end" {
    let region = cover $1 $6 in
    let value  = {kwd_module=$1; name=$2; eq=$3; kwd_struct=$4;
                  structure=$5; kwd_end=$6}
    in {region; value} }

module_alias:
  "module" module_name "=" nsepseq(module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x.region) $4 in
    let region = cover $1 stop in
    let value  = {kwd_module=$1; alias=$2; eq=$3; mod_path=$4}
    in {region; value} }

(* Type declarations *)

type_decl:
  "type" type_name "=" type_expr {
    let region = cover $1 (type_expr_to_region $4) in
    let value  = {kwd_type=$1; name=$2; eq=$3; type_expr=$4}
    in {region; value} }

type_expr:
  fun_type | sum_type | record_type { $1 }

fun_type:
  cartesian "->" fun_type {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    T_Fun {region; value=$1,$2,$3}
  }
| cartesian { $1 }

cartesian:
  core_type { $1 }
| core_type "*" nsepseq(core_type,"*") {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region type_expr_to_region value
    in T_Prod {region; value} }

core_type:
  core_type type_name {
    let arg, ctor = $1, $2 in
    let start     = type_expr_to_region arg in
    let arg       = {region=start; value = (T_Unary arg)}
    and region    = cover start ctor.region in
    in T_Ctor {region; value = (ctor, arg)}
  }
| type_tuple type_name {
    let arg, ctor = $1, $2 in
    let region    = cover arg.region ctor.region
    in T_Ctor {region; value = (ctor, T_Tuple arg)}
  }
| type_name      { T_Var     $1 }
| "_"            { T_Wild    $1 }
| par(type_expr) { T_Par     $1 }
| "<string>"     { T_String  $1 }
| "<int>"        { T_Int     $1 }
| type_in_module { T_ModPath $1 }

type_tuple:
  par(tuple(type_expr)) { $1 }

sum_type:
  nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $1 in
    let value  = {variants=$1; attributes=[]; lead_vbar=None}
    in T_Sum {region; value}
  }
| seq("[@<attr>]") "|" nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $3 in
    let value  = {variants=$3; attributes=$1; lead_vbar = Some $2}
    in T_Sum {region; value} }

variant:
  nseq("[@<attr>]") ctor {
    let attr   = Utils.nseq_to_list $1 in
    let region = cover (fst $1).region $2.region
    and value  = {ctor=$2; arg=None; attributes=attr}
    in {region; value}
  }
| ctor {
    {$1 with value = {ctor=$1; arg=None; attributes=[]}}
  }
| nseq("[@<attr>]") ctor "of" fun_type {
    let attr   = Utils.nseq_to_list $1 in
    let stop   = type_expr_to_region $4 in
    let region = cover (fst $1).region stop
    and value  = {ctor=$2; arg = Some ($3,$4); attributes=attr}
    in {region; value}
  }
| ctor "of" fun_type {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {ctor=$1; arg = Some ($2,$3); attributes=[]}
    in {region; value} }

record_type:
  seq("[@<attr>]") "{" sep_or_term_list(field_decl,";") "}" {
    let ne_elements, terminator = $3 in
    let region = match first_region $1 with
                         None -> cover $2 $4
                 | Some start -> cover start $4
    and compound = Some (Braces ($2,$4)) in
    let value    = {compound; ne_elements; terminator; attributes=$1}
    in T_Record {region; value} }

type_in_module:
  module_path(type_name) { mk_mod_path $1 (fun x -> x.region) }

module_path(selected):
  module_name "." module_path(selected) {
    let (head, tail), selected = $3 in
    (($1,$2), head::tail), selected
  }
| module_name "." selected { (($1,$2), []), $3 }

field_decl:
  seq("[@<attr>]") field_name ":" type_expr {
    let stop   = type_expr_to_region $4 in
    let region = match first_region $1 with
                         None -> cover $2.region stop
                 | Some start -> cover start     stop
    and value  = {field_name=$2; colon=$3; field_type=$4; attributes=$1}
    in {region; value} }

(* Top-level definitions *)

let_decl:
  seq("[@<attr>]") "let" ioption("rec") let_binding {
    let attributes = $1
    and kwd_let    = $2
    and kwd_rec    = $3
    and binding    = $4 in
    let value      = kwd_let, kwd_rec, binding, attributes in
    let stop       = expr_to_region binding.let_rhs in
    let region     = match first_region $1 with
                             None -> cover $2    stop
                     | Some start -> cover start stop
    in {region; value} }

let_binding:
  "<ident>" nseq(sub_irrefutable) type_annotation? "=" expr {
    let binders = Utils.nseq_cons (PVar $1) $2 in
    {binders; lhs_type=$3; eq=$4; let_rhs=$5}
  }
| irrefutable type_annotation? "=" expr {
    {binders=$1,[]; lhs_type=$2; eq=$3; let_rhs=$4} }

type_annotation:
  ":" type_expr { $1,$2 }

(* Patterns *)

irrefutable:
  tuple(sub_irrefutable) {
    let region = nsepseq_to_region pattern_to_region $1
    in P_Tuple {region; value=$1}
  }
| sub_irrefutable { $1 }

sub_irrefutable:
  "<ident>"               { P_Var    $1 }
| "_"                     { P_Wild   $1 }
| unit                    { P_Unit   $1 }
| record_pattern          { P_Record $1 }
| par(closed_irrefutable) { P_Par    $1 }
| ctor                    { P_Ctor {$1 with value = $1,None} }

closed_irrefutable:
  ctor sub_pattern {
    let stop   = pattern_to_region $2 in
    let region = cover $1.region stop
    and value  = $1, Some $2 in
    P_Ctor {region; value}
  }
| irrefutable
| typed_pattern { $1 }

typed_pattern:
  irrefutable ":" type_expr  {
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = {pattern=$1; colon=$2; type_expr=$3}
    in P_Typed {region; value} }

pattern:
  sub_pattern "::" tail {
    let start  = pattern_to_region $1 in
    let stop   = pattern_to_region $3 in
    let region = cover start stop in
    P_Cons {region; value=$1,$2,$3}
  }
| tuple(sub_pattern) {
    let region = nsepseq_to_region pattern_to_region $1
    in P_Tuple {region; value=$1}
  }
| core_pattern { $1 }

sub_pattern:
  par(tail)    { P_Par $1 }
| core_pattern {       $1 }

core_pattern:
  "<ident>"      { P_Var      $1 }
| "_"            { P_Wild     $1 }
| "<int>"        { P_Int      $1 }
| "<nat>"        { P_Nat      $1 }
| "<bytes>"      { P_Bytes    $1 }
| "<string>"     { P_String   $1 }
| "<verbatim>"   { P_Verbatim $1 }
| unit           { P_Unit     $1 }
| par(ptuple)    { P_Par      $1 }
| list__(tail)   { P_List     $1 }
| ctor_pattern   { P_Ctor     $1 }
| record_pattern { P_Record   $1 }

record_pattern:
  "{" sep_or_term_list(field_pattern,";") "}" {
    let ne_elements, terminator = $2 in
    let region   = cover $1 $3
    and compound = Some (Braces ($1,$3)) in
    let value    = {compound; ne_elements; terminator; attributes=[]}
    in {region; value} }

field_pattern:
  field_name "=" sub_pattern {
    let start  = $1.region
    and stop   = pattern_to_region $3 in
    let region = cover start stop
    and value  = {field_name=$1; eq=$2; pattern=$3}
    in {region; value} }

ctor_pattern:
  "Some" sub_pattern {
    let stop   = pattern_to_region $2 in
    let region = cover $1 stop
    in PSomeApp {region; value=$1,$2}
  }
| ctor sub_pattern {
    let region = cover $1.region (pattern_to_region $2)
    in PCtorApp {region; value = ($1, Some $2)}
  }
| ctor    { PCtorApp {$1 with value = ($1, None)} }
| "None"  { PNone  $1 }
| "false" { PFalse $1 }
| "true"  { PTrue  $1 }

ptuple:
  tuple(tail) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1} }

unit:
  "(" ")" { {region = cover $1 $2; value=$1,$2} }

tail:
  sub_pattern "::" tail {
    let start  = pattern_to_region $1 in
    let stop   = pattern_to_region $3 in
    let region = cover start stop in
    PList (PCons {region; value=$1,$2,$3})
  }
| sub_pattern { $1 }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  base_cond__open(expr) | match_expr(base_cond) { $1 }

base_cond__open(x):
  base_expr(x) | conditional(x) { $1 }

base_cond:
  base_cond__open(base_cond) { $1 }

base_expr(right_expr):
  tuple_expr
| let_expr(right_expr)
| local_type_decl(right_expr)
| local_module_decl(right_expr)
| local_module_alias(right_expr)
| fun_expr(right_expr)
| disj_expr_level { $1 }

tuple_expr:
  tuple(disj_expr_level) {
    let region = nsepseq_to_region expr_to_region $1
    in ETuple {region; value=$1} }

conditional(right_expr):
  if_then_else(right_expr) | if_then(right_expr) { $1 }

if_then_else(right_expr):
  "if" expr "then" closed_if "else" right_expr {
    let region = cover $1 (expr_to_region $6)
    and value  =
      {kwd_if=$1; test=$2; kwd_then=$3; ifso=$4; ifnot = Some($5,$6)}
    in ECond {region; value} }

if_then(right_expr):
  "if" expr "then" right_expr {
    let stop   = expr_to_region $4 in
    let region = cover $1 stop
    and value  = {kwd_if=$1; test=$2; kwd_then=$3; ifso=$4; ifnot=None}
    in ECond {region; value} }

base_if_then_else__open(x):
  base_expr(x) | if_then_else(x) { $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else) { $1 }

closed_if:
  base_if_then_else__open(closed_if)
| match_expr(base_if_then_else) { $1 }

match_expr(right_expr):
  "match" expr "with" "|"? cases(right_expr) {
    let cases = {
      value  = Utils.nsepseq_rev $5;
      region = nsepseq_to_region (fun x -> x.region) $5
    }
    and stop =
      match $5 with
        single, [] -> single.region
      | _, others -> last fst others in
    let region = cover $1 stop
    and value =
      {kwd_match=$1; expr=$2; kwd_with=$3; lead_vbar=$4; cases}
    in ECase {region; value} }

cases(right_expr):
  case_clause(right_expr) {
    let start  = pattern_to_region $1.pattern
    and stop   = expr_to_region $1.rhs in
    let region = cover start stop
    in {region; value=$1}, []
  }
| cases(base_cond) "|" case_clause(right_expr) {
    let start            = match $1 with
                             single, [] -> single.region
                           | _, others  -> last fst others
    and stop             = expr_to_region $3.rhs in
    let region           = cover start stop in
    let fst_case         = {region; value=$3}
    and snd_case, others = $1
    in fst_case, ($2,snd_case)::others }

case_clause(right_expr):
  pattern "->" right_expr { {pattern=$1; arrow=$2; rhs=$3} }

let_expr(right_expr):
   seq("[@<attr>]") "let" ioption("rec") let_binding "in" right_expr  {
    let stop   = expr_to_region $6 in
    let region = match first_region $1 with
                         None -> cover $2    stop
                 | Some start -> cover start stop
    and value  =
      {attributes=$1; kwd_let=$2; kwd_rec=$3; binding=$4; kwd_in=$5; body=$6}
    in ELetIn {region; value} }

local_type_decl(right_expr):
  type_decl "in" right_expr {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {type_decl=$1.value; kwd_in=$2; body=$3}
    in ETypeIn {region; value} }

local_module_decl(right_expr):
  module_decl "in" right_expr {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {mod_decl=$1.value; kwd_in=$2; body=$3}
    in EModIn {region; value} }

local_module_alias(right_expr):
  module_alias "in" right_expr {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {mod_alias=$1.value; kwd_in=$2; body=$3}
    in EModAlias {region; value} }

fun_expr(right_expr):
  "fun" nseq(irrefutable) "->" right_expr {
    let stop   = expr_to_region $4 in
    let region = cover $1 stop in
    let value  =
      {kwd_fun=$1; binders=$2; lhs_type=None; arrow=$3; body=$4}
    in EFun {region; value} }

disj_expr_level:
  bin_op(disj_expr_level, "||", conj_expr_level)
| bin_op(disj_expr_level, "or", conj_expr_level) {
    ELogic (BoolExpr (Or $1))
  }
| conj_expr_level { $1 }

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1 in
    let stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

conj_expr_level:
  bin_op(conj_expr_level, "&&", comp_expr_level) {
    ELogic (BoolExpr (And $1))
  }
| comp_expr_level { $1 }

comp_expr_level:
  bin_op(comp_expr_level, "<", cat_expr_level) {
    ELogic (CompExpr (Lt $1))
  }
| bin_op(comp_expr_level, "<=", cat_expr_level) {
    ELogic (CompExpr (Leq $1))
  }
| bin_op(comp_expr_level, ">", cat_expr_level) {
    ELogic (CompExpr (Gt $1))
  }
| bin_op(comp_expr_level, ">=", cat_expr_level) {
    ELogic (CompExpr (Geq $1))
  }
| bin_op(comp_expr_level, "=", cat_expr_level) {
    ELogic (CompExpr (Equal $1))
  }
| bin_op(comp_expr_level, "<>", cat_expr_level) {
    ELogic (CompExpr (Neq $1))
  }
| cat_expr_level { $1 }

cat_expr_level:
  bin_op(cons_expr_level, "^", cat_expr_level)     { EString (Cat $1) }
| cons_expr_level                                  {               $1 }

cons_expr_level:
  bin_op(add_expr_level, "::", cons_expr_level)    { EList (ECons $1) }
| add_expr_level                                   {               $1 }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)     {  EArith (Add $1) }
| bin_op(add_expr_level, "-", mult_expr_level)     {  EArith (Sub $1) }
| mult_expr_level                                  {               $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*", unary_expr_level)   { EArith (Mult $1) }
| bin_op(mult_expr_level, "/", unary_expr_level)   {  EArith (Div $1) }
| bin_op(mult_expr_level, "mod", unary_expr_level) {  EArith (Mod $1) }
| unary_expr_level                                 {               $1 }

unary_expr_level:
  "-" call_expr_level {
    let start  = $1
    and stop   = expr_to_region $2 in
    let region = cover start stop
    and value  = {op=$1; arg=$2}
    in EArith (Neg {region; value})
  }
| "not" call_expr_level {
    let start  = $1
    and stop   = expr_to_region $2 in
    let region = cover start stop
    and value  = {op=$1; arg=$2} in
    ELogic (BoolExpr (Not {region; value}))
  }
| call_expr_level { $1 }

call_expr_level:
  call_expr
| core_expr { $1 }
| ctor_expr { ECtor $1 }

ctor_expr:
  "Some" argument {
    let region = cover $1 (expr_to_region $2)
    in ESomeApp {region; value=$1,$2}
  }
| ctor argument {
    let region = cover $1.region (expr_to_region $2) in
    ECtorApp {region; value = ($1, Some $2)}
  }
| const_ctor { $1 }

const_ctor:
  "None" { ENone $1                           }
| ctor   { ECtorApp {$1 with value=($1,None)} }

arguments:
  argument           { $1,[]                      }
| argument arguments { let h,t = $2 in ($1, h::t) }

argument:
  const_ctor { ECtor $1 }
| core_expr  {       $1 }

call_expr:
  core_expr arguments {
    let start  = expr_to_region $1 in
    let stop   = match $2 with
                   e, [] -> expr_to_region e
                 | _,  l -> last expr_to_region l in
    let region = cover start stop in
    ECall {region; value=$1,$2} }

core_expr:
  "<int>"         {               EArith (Int $1) }
| "<mutez>"       {             EArith (Mutez $1) }
| "<nat>"         {               EArith (Nat $1) }
| "<bytes>"       {                     EBytes $1 }
| "<ident>"       {                       EVar $1 }
| projection      {                      EProj $1 }
| value_in_module {                      E_ModPath $1 }
| "<string>"      {           EString (String $1) }
| "<verbatim>"    {         EString (Verbatim $1) }
| unit            {                      EUnit $1 }
| "false"         {  ELogic (BoolExpr (False $1)) }
| "true"          {  ELogic (BoolExpr (True  $1)) }
| list__(expr)    {          EList (EListComp $1) }
| sequence        {                       ESeq $1 }
| record_expr     {                    ERecord $1 }
| update_record   {                    EUpdate $1 }
| code_inj        {                   ECodeInj $1 }
| par(expr)       {                       EPar $1 }
| par(annot_expr) {                     EAnnot $1 }

code_inj:
  "[%<lang>" expr "]" {
    let region = cover $1.region $3
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

annot_expr:
  expr ":" type_expr { $1,$2,$3 }

projection:
  struct_name "." nsepseq(selection,".") {
    let start  = $1.region
    and stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover start stop
    and value  = {struct_name=$1; selector=$2; field_path=$3}
    in {region; value} }

value_in_module:
  module_path(selected_expr) { mk_mod_path $1 CST.expr_to_region }

selected_expr:
  "or"       { E_Var  {value="or"; region=$1} }
| field_name { E_Var  $1 }
| projection { E_Proj $1 }

selection:
  field_name { FieldName $1 }
| "<int>"    { Component $1 }

record_expr:
  "{" sep_or_term_list(field_assignment,";") "}" {
    let ne_elements, terminator = $2 in
    let region   = cover $1 $3 in
    let compound = Some (Braces ($1,$3)) in
    let value = {compound; ne_elements; terminator; attributes=[]}
    in {region; value} }

update_record:
  "{" path "with" sep_or_term_list(field_path_assignment,";") "}" {
    let ne_elements, terminator = $4 in
    let updates =
      {compound=None; ne_elements; terminator; attributes=[]} in
    let updates = {value = updates; region = cover $3 $5} in
    let value =
      {lbrace=$1; record=$2; kwd_with=$3; updates; rbrace=$5}
    and region = cover $1 $5
    in {region; value} }

field_path_assignment:
  path "=" expr {
    let region = cover (path_to_region $1) (expr_to_region $3)
    and value  = {field_path=$1; assignment=$2; field_expr=$3}
    in {region; value} }

field_assignment:
  field_name "=" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {field_name=$1; assignment=$2; field_expr=$3}
    in {region; value} }

path:
 "<ident>"   { Name $1 }
| projection { Path $1 }

(* Sequences *)

sequence:
  "begin" series? "end" {
    let region   = cover $1 $3
    and compound = Some (BeginEnd ($1,$3)) in
    let elements = $2 in
    let value    = {compound; elements; terminator=None}
    in {region; value} }

series:
  seq_expr ";" series { Utils.nsepseq_cons $1 $2 $3 }
| last_expr           { $1,[] }

last_expr:
  seq_expr
| fun_expr(last_expr)
| match_expr(last_expr)
| let_in_sequence       { $1 }

let_in_sequence:
  seq("[@<attr>]") "let" ioption("rec") let_binding "in" series  {
    let seq      = $6 in
    let stop     = nsepseq_to_region expr_to_region seq in
    let region   = match first_region $1 with
                     None -> cover $2 stop
                   | Some start -> cover start stop in
    let compound = None in
    let elements = Some seq in
    let value    = {compound; elements; terminator=None} in
    let body     = ESeq {region; value} in
    let value    =
      {attributes=$1; kwd_let=$2; kwd_rec=$3; binding=$4; kwd_in=$5; body}
    in ELetIn {region; value} }

seq_expr:
  disj_expr_level | if_then_else (seq_expr) { $1 }
