%{
(* START HEADER *)

[@@@warning "-42"]

open Region
open AST

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start program interactive_expr
%type <AST.t> program
%type <AST.expr> interactive_expr

%%

(* RULES *)

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

(* Compound constructs *)

par(X):
  LPAR X RPAR {
    let region = cover $1 $3
    and value  = {
      lpar   = $1;
      inside = $2;
      rpar   = $3}
    in {region; value}
  }
  
brackets(X):
  LBRACKET X RBRACKET {
    let region = cover $1 $3
    and value  = {
      lbracket = $1;
      inside   = $2;
      rbracket = $3}
    in {region; value}
  }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. The rule [sepseq] parses possibly empty
   sequences of items separated by some token (e.g., a comma), and
   rule [nsepseq] is for non-empty such sequences. See module [Utils]
   for the types corresponding to the semantic actions of those
   rules.
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

(* Possibly empy separated sequence of items *)

sepseq(item,sep):
  (**)              {    None }
| nsepseq(item,sep) { Some $1 }

(* Helpers *)

type_name   : Ident  { $1 }
field_name  : Ident  { $1 }
module_name : Constr { $1 }
struct_name : Ident  { $1 }

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item COMMA nsepseq(item,COMMA) { let h,t = $3 in $1,($2,h)::t }

(* Possibly empty semicolon-separated values between brackets *)

list(item):
  LBRACKET sep_or_term_list(item,SEMI) RBRACKET {
    let elements, terminator = $2 in {
      opening    = LBracket $1;
      elements   = Some elements;
      terminator;
      closing    = RBracket $3}
  }
| LBRACKET RBRACKET {
     {opening    = LBracket $1;
      elements   = None;
      terminator = None;
      closing    = RBracket $2} }

(* Main *)

program:
  declarations EOF               { {decl = Utils.nseq_rev $1; eof=$2} }

declarations:
  declaration                                                    { $1 }
| declaration declarations { Utils.(nseq_foldl (swap nseq_cons) $2 $1)}

declaration:
  LetEntry entry_binding                            { LetEntry $1, [] }
| type_decl                                         { TypeDecl $1, [] }
| let_declaration                                   {          $1, [] }

(* Type declarations *)

type_decl:
  Type type_name EQ type_expr {
    {kwd_type=$1; name=$2; eq=$3; type_expr=$4} }

type_expr:
  cartesian                                              {   TProd $1 }
| sum_type                                               {    TSum $1 }
| record_type                                            { TRecord $1 }

cartesian:
  nsepseq(fun_type, TIMES)                                       { $1 }

fun_type:
  core_type                                                 {      $1 }
| arrow_type                                                { TFun $1 }

arrow_type:
  core_type ARROW fun_type                                 { $1,$2,$3 }

core_type:
  type_projection {
    TAlias $1
  }
| core_type type_constr {
    let arg, constr = $1.value in
    let Region.{value=arg_val; _} = arg in
    let lpar, rpar = ghost, ghost in
    let value = {lpar; inside=arg_val,[]; rpar} in
    let arg = {arg with value} in
    TApp Region.{$1 with value = constr, arg}
  }
| type_tuple type_constr {
    let arg, constr = $1.value in
    TApp Region.{$1 with value = constr, arg}
  }
| par(cartesian) {
    let Region.{value={inside=prod; _}; _} = $1 in
    TPar {$1 with value={$1.value with inside = TProd prod}} }

type_projection:
  type_name {
    $1
  }
| module_name DOT type_name {
    let open Region in
    let module_name,_ , type_name = $1.value in
    let value = module_name.value ^ "." ^ type_name.value
    in {$1 with value} }

type_constr:
  type_name { $1                               }
| Set       { Region.{value="set";  region=$1} }
| Map       { Region.{value="map";  region=$1} }
| List      { Region.{value="list"; region=$1} }

type_tuple:
  par(tuple(type_expr)) { $1 }

sum_type:
  ioption(VBAR) nsepseq(variant,VBAR) { $2 }

variant:
  Constr Of cartesian { {constr=$1; args = Some ($2,$3)} }
| Constr                   { {constr=$1; args = None}         }

record_type:
  LBRACE sep_or_term_list(field_decl,SEMI) RBRACE {
    let elements, terminator = $2 in {
      opening = LBrace $1;
      elements = Some elements;
      terminator;
      closing = RBrace $3} }

field_decl:
  field_name COLON type_expr {
    {field_name=$1; colon=$2; field_type=$3} }

(* Entry points *)

entry_binding:
  Ident nseq(sub_irrefutable) type_annotation? EQ expr {
    let let_rhs = $5 in
    let pattern = PVar $1 in
    let (hd , tl) = $2 in
    {bindings = pattern :: hd :: tl; lhs_type=$3; eq=$4; let_rhs}
  }
  | Ident type_annotation? EQ fun_expr(expr) {
    let pattern = PVar $1 in
    {bindings = [pattern]; lhs_type=$2; eq=$3; let_rhs=$4} }

(* Top-level non-recursive definitions *)

let_declaration:
  Let let_binding {
    let kwd_let, binding = $1.value in
    Let {$1 with value = kwd_let, binding}
  }

let_binding:
  Ident nseq(sub_irrefutable) type_annotation? EQ expr {
    let let_rhs = $5 in
    let ident_pattern = PVar $1 in
    let (hd , tl) = $2 in
    {bindings= (ident_pattern :: hd :: tl); lhs_type=$3; eq=$4; let_rhs}
  }
| irrefutable type_annotation? EQ expr {
    let pattern = $1 in
    {bindings = [pattern]; lhs_type=$2; eq=$3; let_rhs=$4}
  }

type_annotation:
  COLON type_expr { $1,$2 }

(* Patterns *)

irrefutable:
  tuple(sub_irrefutable)                                 {  PTuple $1 }
| sub_irrefutable                                        {         $1 }

sub_irrefutable:
  Ident                                                  {    PVar $1 }
| WILD                                                   {   PWild $1 }
| unit                                                   {   PUnit $1 }
| record_pattern                                         { PRecord $1 }
| par(closed_irrefutable)                                {    PPar $1 }

closed_irrefutable:
  irrefutable                                            {         $1 }
| constr_pattern                                         { PConstr $1 }
| typed_pattern                                          {  PTyped $1 }

typed_pattern:
  irrefutable COLON type_expr  { {pattern=$1; colon=$2; type_expr=$3} }

pattern:
  sub_pattern CONS tail                             { PList (PCons $1) }
| tuple(sub_pattern)                                {        PTuple $1 }
| core_pattern                                      {               $1 }

sub_pattern:
  par(tail)                                              {    PPar $1 }
| core_pattern                                           {         $1 }

core_pattern:
  Ident                                                  {    PVar $1 }
| WILD                                                   {   PWild $1 }
| unit                                                   {   PUnit $1 }
| Int                                                    {    PInt $1 }
| True                                                   {   PTrue $1 }
| False                                                  {  PFalse $1 }
| Str                                                    { PString $1 }
| par(ptuple)                                            {    PPar $1 }
| list(tail)                                       { PList (Sugar $1) }
| constr_pattern                                         { PConstr $1 }
| record_pattern                                         { PRecord $1 }

record_pattern:
  LBRACE sep_or_term_list(field_pattern,SEMI) RBRACE {
    let elements, terminator = $2 in
    {opening = LBrace $1;
     elements = Some elements;
     terminator;
     closing = RBrace $3} }

field_pattern:
  field_name EQ sub_pattern {
    {field_name=$1; eq=$2; pattern=$3} }

constr_pattern:
  Constr sub_pattern                                   {  $1, Some $2 }
| Constr                                               {  $1, None    }

ptuple:
  tuple(tail)                                            {  PTuple $1 }

unit:
  LPAR RPAR                                                     { $1 }

tail:
  sub_pattern CONS tail                             { PList (PCons $1) }
| sub_pattern                                      {               $1 }

(* Expressions *)

interactive_expr:
  expr EOF                                                       { $1 }

expr:
  base_cond__open(expr)                                    {       $1 }
| match_expr(base_cond)                                    { ECase $1 }

base_cond__open(x):
  base_expr(x)
| conditional(x)                                                 { $1 }

base_cond:
  base_cond__open(base_cond)                                     { $1 }

base_expr(right_expr):
  let_expr(right_expr)
| fun_expr(right_expr)
| disj_expr_level                                         {        $1 }
| tuple(disj_expr_level)                                  { ETuple $1 }

conditional(right_expr):
  if_then_else(right_expr)
| if_then(right_expr)                                   {   ECond $1 }

if_then(right_expr):
  If expr Then right_expr {
    let the_unit = ghost, ghost in
    let ifnot = EUnit {region=ghost; value=the_unit} in
    {kwd_if=$1; test=$2; kwd_then=$3; ifso=$4;
     kwd_else=ghost; ifnot} }

if_then_else(right_expr):
  If expr Then closed_if Else right_expr {
    {kwd_if=$1; test=$2; kwd_then=$3; ifso=$4;
     kwd_else=$5; ifnot = $6} }

base_if_then_else__open(x):
  base_expr(x)                                             {       $1 }
| if_then_else(x)                                          { ECond $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else)               {       $1 }

closed_if:
  base_if_then_else__open(closed_if)                       {       $1 }
| match_expr(base_if_then_else)                            { ECase $1 }

match_expr(right_expr):
  Match expr With VBAR? cases(right_expr) {
    let cases = Utils.nsepseq_rev $5.value in
    {kwd_match = $1; expr = $2; opening = With $3;
     lead_vbar = $4; cases = {$5 with value=cases};
     closing = End ghost}
  }
| MatchNat expr With VBAR? cases(right_expr) {
    let cases = Utils.nsepseq_rev $5.value in
    let cast = EVar {region=ghost; value="assert_pos"} in
    let cast = ECall {region=ghost; value=cast,($2,[])} in
    {kwd_match = $1; expr = cast; opening = With $3;
     lead_vbar = $4; cases = {$5 with value=cases};
     closing = End ghost} }

cases(right_expr):
  case_clause(right_expr)                       { $1, [] }
| cases(base_cond) VBAR case_clause(right_expr) {
    let h,t = $1 in $3, ($2,h)::t }

case_clause(right_expr):
  pattern ARROW right_expr           { {pattern=$1; arrow=$2; rhs=$3} }

let_expr(right_expr):
  Let let_binding In right_expr {
    let kwd_let, binding , kwd_in, body = $1.value in
    let let_in = {kwd_let; binding; kwd_in; body}
    in ELetIn {region=$1.region; value=let_in} }

fun_expr(right_expr):
  Fun nseq(irrefutable) ARROW right_expr {
    let kwd_fun, bindings, arrow, body = $1.value in
    let (hd , tl) = bindings in
    let f = {
      kwd_fun ;
      params = hd :: tl ;
      p_annot = None ;
      arrow ;
      body ;
    } in
    EFun { region=$1.region; value=f }
  }

disj_expr_level:
  disj_expr                               { ELogic (BoolExpr (Or $1)) }
| conj_expr_level                                                { $1 }

bin_op(arg1,op,arg2):
  arg1 op arg2                            { {arg1=$1; op=$2; arg2=$3} }

disj_expr:
  bin_op(disj_expr_level, BOOL_OR, conj_expr_level)
| bin_op(disj_expr_level, Or,      conj_expr_level)         { $1 }

conj_expr_level:
  conj_expr                              { ELogic (BoolExpr (And $1)) }
| comp_expr_level                        {                         $1 }

conj_expr:
  bin_op(conj_expr_level, BOOL_AND, comp_expr_level)        { $1 }

comp_expr_level:
  lt_expr                              { ELogic (CompExpr (Lt $1))    }
| le_expr                              { ELogic (CompExpr (Leq $1))   }
| gt_expr                              { ELogic (CompExpr (Gt $1))    }
| ge_expr                              { ELogic (CompExpr (Geq $1))   }
| eq_expr                              { ELogic (CompExpr (Equal $1)) }
| ne_expr                              { ELogic (CompExpr (Neq $1))   }
| cat_expr_level                       {                           $1 }

lt_expr:
  bin_op(comp_expr_level, LT, cat_expr_level)  { $1 }

le_expr:
  bin_op(comp_expr_level, LE, cat_expr_level)  { $1 }

gt_expr:
  bin_op(comp_expr_level, GT, cat_expr_level)  { $1 }

ge_expr:
  bin_op(comp_expr_level, GE, cat_expr_level)  { $1 }

eq_expr:
  bin_op(comp_expr_level, EQ, cat_expr_level)  { $1 }

ne_expr:
  bin_op(comp_expr_level, NE, cat_expr_level) { $1 }

cat_expr_level:
  cat_expr                                        {  EString (Cat $1) }
(*| reg(append_expr)                                { EList (Append $1) } *)
| cons_expr_level                                 {                $1 }

cat_expr:
  bin_op(cons_expr_level, CAT, cat_expr_level)              { $1 }

(*
append_expr:
  cons_expr_level sym(APPEND) cat_expr_level               { $1,$2,$3 }
 *)

cons_expr_level:
  cons_expr                                         { EList (Cons $1) }
| add_expr_level                                    {              $1 }

cons_expr:
  bin_op(add_expr_level, CONS, cons_expr_level)                  { $1 }

add_expr_level:
  plus_expr                                         { EArith (Add $1) }
| minus_expr                                        { EArith (Sub $1) }
| mult_expr_level                                   {              $1 }

plus_expr:
  bin_op(add_expr_level, PLUS, mult_expr_level)             { $1 }

minus_expr:
  bin_op(add_expr_level, MINUS, mult_expr_level)            { $1 }

mult_expr_level:
  times_expr                                      {  EArith (Mult $1) }
| div_expr                                        {   EArith (Div $1) }
| mod_expr                                        {   EArith (Mod $1) }
| unary_expr_level                                {                $1 }

times_expr:
  bin_op(mult_expr_level, TIMES, unary_expr_level)          { $1 }

div_expr:
  bin_op(mult_expr_level, SLASH, unary_expr_level)          { $1 }

mod_expr:
  bin_op(mult_expr_level, Mod, unary_expr_level)            { $1 }

unary_expr_level:
  MINUS call_expr_level                  {                
    let region = cover $1 $2
    and value  = $2
    in EArith (Neg {region; value})      
}
| Not call_expr_level                    { 
    let region = cover $1 $2
    and value  = $2
    in ELogic (BoolExpr (Not ({region; value})))
}    
| call_expr_level                        {                         $1 }

call_expr_level:
  call_expr                                              {   ECall $1 }
| constr_expr                                            { EConstr $1 }
| core_expr                                                      { $1 }

constr_expr:
  Constr core_expr?                                           { $1,$2 }

call_expr:
  core_expr nseq(core_expr) { $1,$2 }

core_expr:
  Int                                               { EArith (Int $1) }
| Mtz                                               { EArith (Mtz $1) }
| Nat                                               { EArith (Nat $1) }
| Ident | module_field                                      { EVar $1 }
| projection                                               { EProj $1 }
| Str                                           { EString (String $1) }
| unit                                                     { EUnit $1 }
| False                               {  ELogic (BoolExpr (False $1)) }
| True                                {  ELogic (BoolExpr (True $1))  }
| list(expr)                                        { EList (List $1) }
| par(expr)                                              {    EPar $1 }
| sequence                                               {    ESeq $1 }
| record_expr                                            { ERecord $1 }
| par(expr COLON type_expr {$1,$3}) {
    EAnnot {$1 with value=$1.value.inside} }

module_field:
  module_name DOT field_name              { $1.value ^ "." ^ $3.value }

projection:
  struct_name DOT nsepseq(selection,DOT) {
    {struct_name = $1; selector = $2; field_path = $3}
  }
| module_name DOT field_name
  DOT nsepseq(selection,DOT) {
    let open Region in
    let module_name, field_name = $1.value in
    let value = module_name.value ^ "." ^ field_name.value in
    let struct_name = {$1 with value} in
    {struct_name; selector = $2; field_path = $3} }

selection:
  field_name    { FieldName $1 }
| par(Int) { Component $1 }

record_expr:
  LBRACE sep_or_term_list(field_assignment,SEMI) RBRACE {
    let elements, terminator = $2 in
    {opening = LBrace $1;
     elements = Some elements;
     terminator;
     closing = RBrace $3} }

field_assignment:
  field_name EQ expr {
    {field_name=$1; assignment=$2; field_expr=$3} }

sequence:
  Begin sep_or_term_list(expr,SEMI) End {
    let elements, terminator = $2 in
    {opening = Begin $1;
     elements = Some elements;
     terminator;
     closing = End $3} }
