%{
(* START HEADER *)

[@@@warning "-42"]

(* open Region *)
(* open AST *)

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <unit> contract
%type <unit> interactive_expr

%%

(* RULES *)

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. *)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
  }
| nseq(item sep { }) { }

(* Compound constructs *)

par(X):
  LPAR X RPAR { }

brackets(X):
  LBRACKET X RBRACKET {
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
   for the types corresponding to the semantic actions of those rules.
 *)

(* Possibly empty sequence of items *)

seq(X):
  (**)     {  }
| X seq(X) {  }

(* Non-empty sequence of items *)

nseq(X):
  X seq(X) {  }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {  }
| X Sep nsepseq(X,Sep) {  }

(* Possibly empy separated sequence of items *)

sepseq(X,Sep):
  (**)           {  }
| nsepseq(X,Sep) {  }

(* Inlines *)

%inline var         : Ident {  }
%inline type_name   : Ident {  }
%inline fun_name    : Ident {  }
%inline field_name  : Ident {  }
%inline struct_name : Ident {  }

(* Main *)

contract:
  nseq(declaration) EOF {
    
  }

declaration:
  type_decl   { }
| const_decl  { }
| fun_decl    { }

(* Type declarations *)

type_decl:
  type_name ASS type_expr { }

type_expr:
  Ident LBRACE nsepseq(type_expr, COMMA) RBRACE {}
| Ident { }

(* Function declarations *)

(* Function Declaration

def example(t1: int, t2: int) -> int:
    return 5
*)

fun_decl:
  Def fun_name parameters ARROW type_expr COLON
    seq(local_decl)
    block  { }

(* (t1: int = 5, t2: int = 9) *)

parameters:
  par(nsepseq(param_decl,COMMA)) { }

(* TODO: Add option(EQ expr) back into param_decl once LIGO supports
   default values for parameters *)

param_decl:
  Ident COLON type_expr { }

block:
  START sep_or_term_list(statement,SEMI) END { }

statement:
  instruction     { }
| open_data_decl  { }

open_data_decl:
  open_const_decl { }
| open_var_decl   { }

open_const_decl:
  Uident unqualified_decl { }

open_var_decl:
  Ident unqualified_decl { }

local_decl:
  fun_decl  { }
| data_decl { }

data_decl:
  const_decl { }
| var_decl   { }

unqualified_decl:
  COLON type_expr option(pair(EQ, expr)) { }

const_decl:
  open_const_decl SEMI { }
| open_const_decl {  }

var_decl:
  open_var_decl SEMI { }
| open_var_decl {  }

instruction:
  single_instr { }
| block        { }

single_instr:
  conditional  { }
| assignment   { }
| loop         { }
| fail_instr   { }
| assertion    { }
| Pass         { }

fail_instr:
  Raise expr { }

assertion:
  Assert expr option(pair(COMMA,expr)) { }

conditional:
  If control_clause sepseq(control_clause, Elif) option(pair(Else, pair(COLON,block))) { }

control_clause:
  expr COLON block { }

assignment:
  lhs EQ rhs { }

rhs:
  expr  { }

lhs:
  path       { }
| index_access { }

loop:
  while_loop {  }
| for_loop   {  }

while_loop:
  While control_clause { }

for_loop:
  For nsepseq(Ident, COMMA) In control_clause { }

(* Expressions *)

interactive_expr:
  expr EOF {  }

expr:
  comp_expr { }

comp_expr:
  comp_expr LT add_expr { }
| comp_expr LEQ add_expr { }
| comp_expr GT add_expr { }
| comp_expr GEQ add_expr { }
| comp_expr EQ add_expr { }
| comp_expr NEQ add_expr { }
| add_expr { }

add_expr:
  add_expr PLUS mult_expr { }
| add_expr MINUS mult_expr { }
| mult_expr {  }

mult_expr:
  mult_expr TIMES unary_expr { }
| mult_expr SLASH unary_expr { }
| mult_expr MOD unary_expr { }
| unary_expr { }

unary_expr:
  MINUS core_expr { ignore $2 }
| Not core_expr { ignore $2  }
| core_expr { ignore $1 }

core_expr:
  Int              { "" }
| Float            { "" } 
| Mtz              { "" }
| var              { "" }
| Str              { "" }
| Bytes            { "" }
(*| C_False          { "" }
| C_True           { "" }
| C_Unit           { "" }*)
| tuple_expr       { "" }
| list_expr        { "" }
(*| C_None           { "" }*)
| fun_call         { "" }
| map_expr         { "" }
| set_expr         { "" }
| record_expr      { "" }
| projection       { "" }
(*| C_Some arguments { "" }*)

index_access:
  path brackets(expr) { }

path:
  var        { }
| projection { }

projection:
  struct_name DOT nsepseq(selection,DOT) { }

selection:
  field_name { }
| Int        { }

record_expr:
  LBRACKET sep_or_term_list(field_assignment,COMMA) RBRACKET { }

field_assignment:
  field_name COLON type_expr EQ expr { }

set_expr:
  LBRACE nsepseq(expr,COMMA) RBRACE { }

map_expr:
  LBRACE nsepseq(mapping, COMMA) RBRACE { }

mapping:
  expr COLON expr { }

fun_call:
  fun_name arguments { }

tuple_expr:
  tuple_inj { }

tuple_inj:
  par(nsepseq(expr,COMMA)) { }

arguments:
  tuple_inj { }

list_expr:
  brackets(nsepseq(expr, COMMA)) { }
| LBRACKET RBRACKET { }

(*

(* Patterns *)

(* Matches in python are difficult, because the match block suggests a 
   control structure but is used as an expression. This violates a lot of the
   conventions for syntax in Python, so we take the approach of defining the 
   match block in a control structure separately from its use as an expression.
*)

match_block_decl:
  Match fun_name var COLON nsepseq(pattern_match, SEMI) END

(* Matches are called as though they were functions, so no specific syntax for
   their use. *)

pattern_match:
  pattern ARROW expr

pattern:
  nsepseq(core_pattern,CONS) { }

core_pattern:
  var                      { }
| WILD                     { }
| Int                      { }
| Str                      { }
| C_Unit                   { }
| C_False                  { }
| C_True                   { }
| C_None                   { }
| list_pattern             { }
| tuple_pattern            { }
| constr_pattern           { }
| C_Some par(core_pattern) {
    let region = cover $1 $2.region
    in PSome {region; value = $1,$2}}

list_pattern:
  injection(List,core_pattern) { }
| Nil                          { }
| par(cons_pattern)            { }

cons_pattern:
  core_pattern CONS pattern { $1,$2,$3 }

tuple_pattern:
  par(nsepseq(core_pattern,COMMA)) { $1 }

constr_pattern:
  Constr tuple_pattern {
    let region = cover $1.region $2.region
    in {region; value = $1, Some $2}
  }
| Constr {
    {region=$1.region; value = $1, None}
  }

*)
