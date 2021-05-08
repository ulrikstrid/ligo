(* Menhir specification of the parser of PascaLIGO *)

%{
(* START HEADER *)

[@@@warning "-42"]

(* Dependencies *)

open Simple_utils.Region
module CST = Cst.Pascaligo
open CST

(* Utilities *)

let first_region = function
    [] -> None
| x::_ -> Some x.region

let mk_set     region = `Set    region
let mk_list    region = `List   region
let mk_map     region = `Map    region
let mk_big_map region = `BigMap region
let mk_record  region = `Record region
let mk_wild    region = Region.{value="_"; region}

let mk_reg   region value = Region.{region; value}
let mk_E_Var region value = E_Var Region.{region; value}

let mk_mod_path :
  (module_name * dot) Utils.nseq * 'a ->
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
    let region = CST.nseq_to_region (fun (x,_) -> x.region) nseq in
    let region = Region.cover region (to_region field)
    and value  = {module_path; selector=last_dot; field}
    in {value; region}

let terminate_decl semi = function
  D_Const     d -> {d with value = {d.value with terminator=semi}}
| D_Directive d -> d
| D_Fun       d -> {d with value = {d.value with terminator=semi}}
| D_Module    d -> {d with value = {d.value with terminator=semi}}
| D_ModAlias  d -> {d with value = {d.value with terminator=semi}}
| D_Type      d -> {d with value = {d.value with terminator=semi}}

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <CST.t> contract
%type <CST.expr> interactive_expr

%on_error_reduce field_decl
%on_error_reduce field_assignment
%on_error_reduce test_clause(closed_instr)
%on_error_reduce base_expr(closed_expr)
%on_error_reduce base_expr(expr)
%on_error_reduce base_instr(closed_instr,closed_expr)
%on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level)
%on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level)
%on_error_reduce nseq(__anonymous_0(field(core_pattern),SEMI))
%on_error_reduce nsepseq(field(core_pattern),SEMI)
%on_error_reduce field(core_pattern)
(*%on_error_reduce selected_expr*)
%on_error_reduce nsepseq(module_name,DOT)
%on_error_reduce ctor_expr
%on_error_reduce nseq(__anonymous_0(field_decl,SEMI))
%on_error_reduce nseq(__anonymous_0(field_path_assignment,SEMI))
%on_error_reduce nseq(__anonymous_0(binding,SEMI))
%on_error_reduce nseq(__anonymous_0(field_assignment,SEMI))
%on_error_reduce nseq(__anonymous_0(core_pattern,SEMI))
%on_error_reduce nseq(__anonymous_0(expr,SEMI))
%on_error_reduce nsepseq(field_assignment,SEMI)
%on_error_reduce nseq(__anonymous_0(statement,SEMI))
%on_error_reduce nsepseq(case_clause(expr),VBAR)
%on_error_reduce nsepseq(core_pattern,SEMI)
%on_error_reduce pattern
%on_error_reduce nsepseq(core_pattern,SHARP)
%on_error_reduce nsepseq(case_clause(test_clause(instruction)),VBAR)
(*%on_error_reduce lhs*)
(*%on_error_reduce map_lookup*)
%on_error_reduce nsepseq(statement,SEMI)
%on_error_reduce ctor_pattern
%on_error_reduce update_expr_level
%on_error_reduce nsepseq(param_decl,SEMI)
%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce nsepseq(field_path_assignment,SEMI)
%on_error_reduce nsepseq(binding,SEMI)
%on_error_reduce nsepseq(expr,SEMI)
%on_error_reduce add_expr_level
%on_error_reduce unary_expr_level
%on_error_reduce const_decl
%on_error_reduce fun_decl
%on_error_reduce variant
%on_error_reduce core_type
%on_error_reduce nsepseq(field_decl,SEMI)
%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce type_decl
%on_error_reduce cartesian_level
%on_error_reduce fun_type_level
%on_error_reduce cons_expr_level
%on_error_reduce cat_expr_level
%on_error_reduce set_mem_level
%on_error_reduce disj_expr_level
%on_error_reduce nsepseq(variant,VBAR)
%on_error_reduce core_pattern
%on_error_reduce nsepseq(type_expr,COMMA)
%on_error_reduce expr
%on_error_reduce nsepseq(expr,COMMA)
%on_error_reduce option(SEMI)
%on_error_reduce option(VBAR)
(*%on_error_reduce projection*)
%on_error_reduce nseq(top_declaration)
%on_error_reduce local_path
%on_error_reduce nseq(Attr)

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
    let (first, sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item, next_sep) :: others ->
        trans ((prev_sep, item) :: seq, next_sep) others in
    let list, term = trans ([], sep) tail
    in (first, List.rev list), Some term }

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1 $3
    and value  = {lpar=$1; inside=$2; rpar=$3}
    in {region; value} }

brackets(X):
  "[" X "]" {
    let region = cover $1 $3
    and value  = {lbracket=$1; inside=$2; rbracket=$3}
    in {region; value} }

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
   rules. *)

(* Possibly empty sequence of items *)

%inline seq(X):
  (**)    { [] }
| nseq(X) { let hd,tl = $1 in hd::tl }

(* Non-empty sequence of items *)

nseq(X):
  X         { $1, [] }
| X nseq(X) { let hd,tl = $2 in $1, hd::tl }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                 $1,        [] }
| X Sep nsepseq(X,Sep) { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empy separated sequence of items *)

sepseq(X,Sep):
  (**)           { None    }
| nsepseq(X,Sep) { Some $1 }

(* Aliasing and inlining some tokens *)

%inline variable    : "<ident>"  { $1 }
%inline type_name   : "<ident>"  { $1 }
%inline fun_name    : "<ident>"  { $1 }
%inline field_name  : "<ident>"  { $1 }
%inline module_name : "<uident>" { $1 }
%inline ctor        : "<uident>" { $1 }

(* Unary operators *)

unary_op(op,arg):
  op arg {
    let region = cover $1 (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in {region; value} }

(* Binary operators *)

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1 in
    let stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

(* ENTRY *)

contract:
  nseq(top_declaration) EOF { {decl=$1; eof=$2} }

(* DECLARATIONS (top-level) *)

(* We define two versions of each declaration: a low one and a top
   one. The latter is the former with an optional semicolon, plus the
   preprocessing directives (which are never follwed by a
   semicolon). The low declarations bear names which are prefixed by
   [low_], for example, [low_module_decl]. See below. The reason
   for this is that in certain contexts, the semicolon is allowed, and
   it is not in others. In blocks, semicolons as separators or
   terminators are mandatory because declarations can be mixed with
   instructions (see statements). At the top-level (here),
   instructions are not allowed and, because declarations start with
   keywords, it is not mandatory to force them to be separated or
   terminated by a semicolon. *)

top_declaration:
  declaration ";"? { terminate_decl $2 $1 }
| "<directive>"    { D_Directive       $1 }

declaration:
  type_decl    { D_Type     $1 }
| const_decl   { D_Const    $1 }
| fun_decl     { D_Fun      $1 }
| module_decl  { D_Module   $1 }
| module_alias { D_ModAlias $1 }

declarations:
  nseq(declaration ";"? { terminate_decl $2 $1}) { $1 }

(* Module declarations *)

module_decl:
  "module" module_name "is" "block"? "{" declarations "}" {
    let enclosing = Braces ($4,$5,$7) in
    let region    = cover $1 $7
    and value     = {kwd_module=$1; name=$2; kwd_is=$3; enclosing;
                     declarations=$6; terminator=None}
    in {region; value}
  }
| "module" module_name "is" "begin" declarations "end" {
    let enclosing = BeginEnd ($4,$6) in
    let region    = cover $1 $6
    and value     = {kwd_module=$1; name=$2; kwd_is=$3; enclosing;
                     declarations=$5; terminator=None}
    in {region; value} }

module_alias:
  "module" module_name "is" nsepseq(module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x.region) $4 in
    let region = cover $1 stop in
    let value  = {kwd_module=$1; alias=$2; kwd_is=$3;
                  mod_path=$4; terminator=None}
    in {region; value} }

(* Type declarations *)

type_decl:
  "type" type_name "is" type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $1 stop in
    let value  = {kwd_type=$1; name=$2; kwd_is=$3;
                  type_expr=$4; terminator=None}
    in {region; value} }

(* Type expressions *)

type_expr:
  fun_type_level | sum_type | record_type { $1 }

fun_type_level:
  cartesian_level "->" fun_type_level {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    T_Fun {region; value=($1,$2,$3)}
  }
| cartesian_level { $1 }

cartesian_level:
  core_type "*" nsepseq(core_type,"*") {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region type_expr_to_region value
    in T_Prod {region; value}
  }
| core_type { $1 }

core_type:
  "_"            { T_Wild    $1 }
| "<string>"     { T_String  $1 }
| "<int>"        { T_Int     $1 }
| type_ctor_app  { T_Ctor    $1 }
| par(type_expr) { T_Par     $1 }
| type_name      { T_Var     $1 }
| type_in_module { T_ModPath $1 }

(* Type constructor applications *)

type_ctor_app:
  type_name type_tuple {
    let region = cover $1.region $2.region
    in mk_reg region ($1,$2)
  }
| "map" type_tuple {
    let region    = cover $1 $2.region in
    let type_ctor = mk_reg $1 "map"
    in mk_reg region (type_ctor, $2)
  }
| "big_map" type_tuple {
    let region    = cover $1 $2.region in
    let type_ctor = mk_reg $1 "big_map"
    in mk_reg region (type_ctor, $2)
  }
| "set" par(type_expr) {
    let total = cover $1 $2.region in
    let type_ctor = mk_reg $1 "set" in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = mk_reg region {lpar; inside=inside,[]; rpar}
    in mk_reg total (type_ctor, tuple)
  }
| "list" par(type_expr) {
    let total = cover $1 $2.region in
    let type_ctor = mk_reg $1 "list" in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = mk_reg region {lpar; inside=inside,[]; rpar}
    in mk_reg total (type_ctor, tuple) }

type_tuple:
  par(nsepseq(type_expr,",")) { $1 }

(* Type qualification *)

type_in_module:
  module_path(type_name) { mk_mod_path $1 (fun x -> x.region) }

module_path(selected):
  module_name "." module_path(selected) {
    let (head, tail), selected = $3 in
    (($1,$2), head::tail), selected
  }
| module_name "." selected { (($1,$2), []), $3 }

(* Sum type *)

sum_type:
  nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $1 in
    let value  = {attributes=[]; lead_vbar=None; variants=$1}
    in T_Sum {region; value}
  }
| seq("[@<attr>]") "|" nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $3 in
    let value  = {attributes=$1; lead_vbar = Some $2; variants=$3}
    in T_Sum {region; value} }

variant:
  nseq("[@<attr>]") ctor {
    let attributes = Utils.nseq_to_list $1 in
    let value      = {ctor=$2; arg=None; attributes}
    and region     = cover (fst $1).region $2.region
    in {region; value}
  }
| ctor {
    {$1 with value = {ctor=$1; arg=None; attributes=[]}}
  }
| nseq("[@<attr>]") ctor "of" fun_type_level {
    let attributes = Utils.nseq_to_list $1 in
    let value      = {ctor = $2; arg = Some ($3,$4); attributes}
    and stop       = type_expr_to_region $4 in
    let region     = cover (fst $1).region stop
    in {region; value}
  }
| ctor "of" fun_type_level {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {ctor=$1; arg = Some ($2,$3); attributes=[]}
    in {region; value} }

(* Record type *)

record_type:
  seq("[@<attr>]") "record" sep_or_term_list(field_decl,";") "end" {
    let fields, terminator = $3 in
    let region = match first_region $1 with
                         None -> cover $2 $4
                 | Some start -> cover start $4
    and value  = {kind        = `Record $2;
                  enclosing   = End $4;
                  ne_elements = fields;
                  terminator;
                  attributes  = $1}
    in T_Record {region; value}
  }
| seq("[@<attr>]") "record" "[" sep_or_term_list(field_decl,";") "]" {
    let fields, terminator = $4 in
    let region = match first_region $1 with
                         None -> cover $2 $5
                 | Some start -> cover start $5
    and value  = {kind        = `Record $2;
                  enclosing   = Brackets ($3,$5);
                  ne_elements = fields;
                  terminator;
                  attributes  = $1}
    in T_Record {region; value} }

field_decl:
  seq("[@<attr>]") field_name ":" type_expr {
    let stop   = type_expr_to_region $4 in
    let region = match first_region $1 with
                         None -> cover $2.region stop
                 | Some start -> cover start stop
    and value  =
      {attributes=$1; field_name=$2; field_type = Some ($3,$4)}
    in {region; value}
  }
| seq("[@<attr>]") field_name {
    let stop   = $2.region in
    let region = match first_region $1 with
                         None -> stop
                 | Some start -> cover start stop
    and value  = {attributes=$1; field_name=$2; field_type=None}
    in {region; value} }

(* Function declarations *)

fun_decl:
  seq("[@<attr>]") ioption("recursive") "function" fun_name parameters
  ioption(type_annotation) "is" expr {
    let stop   = expr_to_region $8 in
    let region = match first_region $1 with
                   Some start -> cover start stop
                 | None -> match $2 with
                             Some start -> cover start stop
                           |       None -> cover $3 stop
    and value  = {attributes=$1; kwd_recursive=$2; kwd_function=$3;
                  fun_name=$4; param=$5;
                  ret_type=$6; kwd_is=$7; return=$8; terminator=None}
    in {region; value} }

parameters:
  par(nsepseq(param_decl,";")) { $1 }

param_decl:
  "var" variable param_type? {
    let stop   = match $3 with
                         None -> $2.region
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover $1 stop
    and value  = {kwd_var=$1; var=$2; param_type=$3}
    in ParamVar {region; value}
  }
| "var" "_" param_type? {
    let stop   = match $3 with
                   None -> $2
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover $1 stop
    and value  = {kwd_var=$1; var = mk_wild $2; param_type=$3}
    in ParamVar {region; value}
  }
| "const" variable param_type? {
    let stop   = match $3 with
                         None -> $2.region
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover $1 stop
    and value  = {kwd_const=$1; var=$2; param_type=$3}
    in ParamConst {region; value}
  }
| "const" "_" param_type? {
    let stop   = match $3 with
                         None -> $2
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover $1 stop
    and value  = {kwd_const=$1; var = mk_wild $2; param_type=$3}
    in ParamConst {region; value} }

param_type:
  ":" fun_type_level { $1,$2 }

type_annotation:
  ":" type_expr { $1,$2 }

(* STATEMENTS *)

statement:
  declaration { S_Decl    $1 }
| var_decl    { S_VarDecl $1 } (* Not allowed at top-level *)
| instruction { S_Instr   $1 }

(* Declarations *)

const_decl:
  seq("[@<attr>]") "const" unqualified_decl("=") {
    let pattern, equal, init, stop = $3 in
    let region = match first_region $1 with
                   None -> cover $2 stop
                 | Some start -> cover start stop
    and value  = {attributes=$1; kwd_const=$2; pattern;
                  equal; init; terminator=None}
    in {region; value} }

var_decl:
  "var" unqualified_decl(":=") {
    let pattern, assign, init, stop = $2 in
    let region = cover $1 stop
    and value  = {kwd_var=$1; pattern; assign; init; terminator=None}
    in {region; value} }

unqualified_decl(OP):
  core_pattern OP expr {
    $1, $2, $3, expr_to_region $3
  }
| core_pattern type_annotation OP expr {
    let start   = pattern_to_region $1
    and stop    = type_expr_to_region (snd $2) in
    let region  = cover start stop
    and value   = {pattern=$1; type_annot=$2} in
    let pattern = P_Typed {region; value}
    in pattern, $3, $4, expr_to_region $4 }

(* Instructions *)

(* Note: The rule [base_instr] is parameterised by an expression
   [right_expr] because [assignment], [map_remove] and [set_remove]
   can derive [right_expr] to the right. This has an impact on the
   so-called "dangling else" problem. Compare with [base_expr] below,
   which is a bit simpler, as expressions do not depend on
   instructions. *)

instruction:
  base_instr(instruction,expr)
| if_then_instr(instruction) { $1 }

base_instr(right_instr,right_expr):
  if_then_else_instr(right_instr) { I_Cond        $1 }
| assignment(right_expr)          { I_Assign      $1 }
| map_remove(right_expr)          { I_MapRemove   $1 }
| set_remove(right_expr)          { I_SetRemove   $1 }
| call_instr                      { I_Call        $1 }
| case_instr                      { I_Case        $1 }
| for_int                         { I_For         $1 }
| for_in                          { I_ForIn       $1 }
| map_patch                       { I_MapPatch    $1 }
| record_patch                    { I_RecordPatch $1 }
| set_patch                       { I_SetPatch    $1 }
| while_loop                      { I_While       $1 }
| "skip"                          { I_Skip        $1 }

(* Conditional instruction (see [cond_expr] below) *)

if_then_instr(right_instr):
  "if" expr "then" test_clause(right_instr) {
     let region = cover $1 (test_clause_to_region $4) in
     let value = {kwd_if=$1; test=$2; kwd_then=$3; ifso=$4; ifnot=None }
     in I_Cond {region; value} }

if_then_else_instr(right_instr):
  "if" expr "then" test_clause(closed_instr)
  "else" test_clause(right_instr) {
     let region = cover $1 (test_clause_to_region $6) in
     let value = {
       kwd_if=$1; test=$2; kwd_then=$3; ifso=$4; ifnot = Some ($5,$6) }
     in {region; value} }

closed_instr:
  base_instr(closed_instr,closed_expr) { $1 }

(* Removals (set, map) *)

set_remove(right_expr):
  "remove" expr "from" "set" right_expr {
    let region = cover $1 (expr_to_region $5) in
    let value  = {kwd_remove=$1; element=$2;
                  kwd_from=$3; kwd_set=$4; set=$5}
    in {region; value} }

map_remove(right_expr):
  "remove" expr "from" "map" right_expr {
    let region = cover $1 (expr_to_region $5) in
    let value  = {kwd_remove=$1; key=$2;
                  kwd_from=$3; kwd_map=$4; map=$5}
    in {region; value} }

(* Patches (set, map, record) *)

record_patch:
  "patch" core_expr "with" record_expr {
    let region = cover $1 $4.region in
    let value  = {kwd_patch=$1; record=$2; kwd_with=$3; record_inj=$4}
    in {region; value} }

set_patch:
  "patch" core_expr "with" ne_injection("set",expr) {
    let set_inj = $4 mk_set in
    let region  = cover $1 set_inj.region in
    let value   = {kwd_patch=$1; set=$2; kwd_with=$3; set_inj}
    in {region; value} }

map_patch:
  "patch" core_expr "with" ne_injection("map",binding) {
    let map_inj = $4 mk_map in
    let region  = cover $1 map_inj.region in
    let value   = {kwd_patch=$1; map=$2; kwd_with=$3; map_inj}
    in {region; value} }

binding:
  expr "->" expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {source=$1; arrow=$2; image=$3}
    in {region; value} }

ne_injection(Kind,element):
  Kind sep_or_term_list(element,";") "end" {
    fun mk_kwd ->
      let kind, enclosing = mk_kwd $1, End $3
      and ne_elements, terminator = $2 in
      let region = cover $1 $3
      and value  = {kind; enclosing; ne_elements;
                    terminator; attributes=[]}
      in {region; value}
  }
| Kind "[" sep_or_term_list(element,";") "]" {
    fun mk_kwd ->
      let kind, enclosing = mk_kwd $1, Brackets ($2, $4)
      and ne_elements, terminator = $3 in
      let region = cover $1 $4
      and value = {kind; enclosing; ne_elements;
                   terminator; attributes=[]}
      in {region; value} }

(* Procedure calls *)

call_instr:
  call_expr { $1 }

(* Generic case construct. A case construct features pattern matching
and it can either be an instruction or an expression, depending on the
syntactic category allowed on the right-hand sides (rhs) of each
individual case clause. *)

case(rhs):
  "case" expr "of" "|"? cases(rhs) "end" {
    fun rhs_to_region ->
      let enclosing = End $6
      and cases     = $5 rhs_to_region in
      let region    = cover $1 $6 in
      let value     = {kwd_case=$1; expr=$2; kwd_of=$3;
                       enclosing; lead_vbar=$4; cases}
      in {region; value}
  }
| "case" expr "of" "[" "|"? cases(rhs) "]" {
    fun rhs_to_region ->
      let region    = cover $1 $7
      and enclosing = Brackets ($4,$7)
      and cases     = $6 rhs_to_region in
      let value     = {kwd_case=$1; expr=$2; kwd_of=$3;
                       enclosing; lead_vbar=$5; cases}
      in {region; value} }

cases(rhs):
  nsepseq(case_clause(rhs),"|") {
    fun rhs_to_region ->
      let mk_clause pre_clause = pre_clause rhs_to_region in
      let value  = Utils.nsepseq_map mk_clause $1 in
      let region = nsepseq_to_region (fun x -> x.region) value
      in {region; value} }

case_clause(rhs):
  pattern "->" rhs {
    fun rhs_to_region ->
      let start  = pattern_to_region $1 in
      let region = cover start (rhs_to_region $3)
      and value  = {pattern=$1; arrow=$2; rhs=$3}
      in {region; value} }

(* Case instructions (see [case_expr] below). The right-hand
   sides are [test_clause(instruction)].  *)

case_instr:
  case(test_clause(instruction)) { $1 test_clause_to_region }

test_clause(instr):
  instr { ClauseInstr $1 }
| block { ClauseBlock $1 }

block:
  "begin" sep_or_term_list(statement,";") "end" {
     let statements, terminator = $2
     and enclosing : block_enclosing = BeginEnd ($1,$3) in
     let region    = cover $1 $3
     and value     = {enclosing; statements; terminator}
     in {region; value}
  }
| "block"? "{" sep_or_term_list(statement,";") "}" {
     let statements, terminator = $3
     and enclosing : block_enclosing = Braces ($1,$2,$4) in
     let start  = match $1 with
                    None -> $2
                  | Some b -> b in
     let region = cover start $4
     and value  = {enclosing; statements; terminator}
     in {region; value} }

(* Assignments *)

assignment(right_expr):
  path_expr ":=" right_expr {
    let stop   = expr_to_region $3 in
    let region = cover (expr_to_region $1) stop
    and value  = {lhs=$1; assign=$2; rhs=$3}
    in {region; value} }

(* Loops *)

while_loop:
  "while" expr block {
    let region = cover $1 $3.region
    and value  = {kwd_while=$1; cond=$2; block=$3}
    in {region; value} }

for_int:
  "for" variable ":=" expr "to" expr ioption(step_clause) block {
    let region = cover $1 $8.region in
    let value  = {kwd_for=$1; binder=$2; assign=$3; init=$4;
                  kwd_to=$5; bound=$6; step=$7; block=$8}
    in {region; value} }

for_in:
  "for" variable "->" variable "in" "map" expr block {
    let bind_to    = Some ($3,$4)
    and collection = `Map $6 in
    let region     = cover $1 $8.region in
    let value      = {kwd_for=$1; var=$2; bind_to; kwd_in=$5;
                      collection; expr=$7; block=$8}
    in {region; value}
  }
| "for" variable "in" collection expr block {
    let region = cover $1 $6.region in
    let value  = {kwd_for=$1; var=$2; bind_to=None; kwd_in=$3;
                  collection=$4; expr=$5; block=$6}
    in {region; value} }

step_clause:
  "step" expr { $1,$2 }

collection:
  "set"  { `Set  $1 }
| "list" { `List $1 }

(* EXPRESSIONS *)

interactive_expr:
  expr EOF { $1 }

(* Our definition of [expr] aims at solving the "dangling else"
   problem in the grammar itself. Two subgoals are achieved:

     * separating the conditional expressions ([cond_expr]) from the
       others ([base_expr]),

     * identify the productions that are right-recursive.

     The first point is needed to distinguish if-then, called "open",
   from if-then-else conditionals, called "closed" because the "then"
   clause cannot derive an open one.

     The second point enables us to parameterise [base_expr] with
   [right_expr], for the benefit of those right-recursive
   rules. Depending on the right context, [base_expr] will be
   instanciated with different kinds of expressions. For example, we
   need [close_expr] to be an expression that is always suitable in
   the "then" branch of a *closed* conditional, without triggering an
   LR conflict because of a dangling else: see the definition of
   [if_then_else_expr]. The [right_expr] parameter is useful because,
   in the "then" branch of a closed conditional, we do not want to
   generate on the right an expression that is not closed, that is an
   open conditional, e.g.

      if a then fun x -> if x then b else c // dangling else!

   Instead, using [close_expr] in the "then" branch of a close
   conditional yields the intended goal as if we had written

      if a then (fun x -> if x then b else c)

   where "if x then b else c" has been derived by [expr].

   Note [disj_expr_level] in [base_expr]: this is the start of the
   stratification of the later in order to built in the grammar the
   priority of different operators/constructs in the usual handmade
   manner. So the sooner a non-terminal is defined, the lower its
   priority. For example, [disj_expr_level] is derived from [expr]
   before [conj_expr_level] because "or" has a lower priority than
   "and", as expected in Boolean algebras. *)

expr:
  base_expr(expr) | if_then_expr(expr) { $1 }

base_expr(right_expr):
  if_then_else_expr(right_expr) { E_Cond  $1 }
| block_with(right_expr)        { E_Block $1 }
| fun_expr(right_expr)          { E_Fun   $1 }
| case(expr)                    { E_Case ($1 expr_to_region) }
| disj_expr_level               { $1 }

(* Conditional expressions *)

if_then_expr(right_expr):
  "if" expr "then" right_expr {
     let region = cover $1 (expr_to_region $4) in
     let value = {kwd_if=$1; test=$2; kwd_then=$3; ifso=$4; ifnot=None }
     in E_Cond {region; value} }

if_then_else_expr(right_expr):
  "if" expr "then" closed_expr "else" right_expr {
     let region = cover $1 (expr_to_region $6) in
     let value = {
       kwd_if=$1; test=$2; kwd_then=$3; ifso=$4; ifnot = Some ($5,$6) }
     in {region; value} }

closed_expr:
  base_expr(closed_expr) { $1 }

(* Block expressions *)

block_with(right_expr):
  block "with" right_expr {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop in
    let value  = {block=$1; kwd_with=$2; expr=$3}
    in {region; value} }

(* Functional expressions (a.k.a. lambdas) *)

fun_expr(right_expr):
  "function" parameters ioption(type_annotation) "is" right_expr {
    let stop   = expr_to_region $5 in
    let region = cover $1 stop
    and value  = {kwd_function=$1; param=$2; ret_type=$3;
                  kwd_is=$4; return=$5}
    in {region; value} }

(* Resuming stratification of [base_expr] with Boolean expressions *)

disj_expr_level:
  disj_expr_level "or" conj_expr_level {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in E_Or {region; value}
  }
| conj_expr_level { $1 }

conj_expr_level:
  conj_expr_level "and" set_mem_level {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in E_And {region; value}
  }
| set_mem_level { $1 }

(* Set membership *)

set_mem_level:
  update_expr_level "contains" set_mem_level {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {set=$1; kwd_contains=$2; element=$3}
    in E_SetMem {region; value}
  }
| comp_expr_level { $1 }

(* Comparisons *)

comp_expr_level:
  bin_op(comp_expr_level, "<",   cat_expr_level) { E_Lt    $1 }
| bin_op(comp_expr_level, "<=",  cat_expr_level) { E_Leq   $1 }
| bin_op(comp_expr_level, ">",   cat_expr_level) { E_Gt    $1 }
| bin_op(comp_expr_level, ">=",  cat_expr_level) { E_Geq   $1 }
| bin_op(comp_expr_level, "=",   cat_expr_level) { E_Equal $1 }
| bin_op(comp_expr_level, "=/=", cat_expr_level) { E_Neq   $1 }
| cat_expr_level                                 { $1 }

(* Concatenation *)

cat_expr_level:
  bin_op(cons_expr_level, "^", cat_expr_level) { E_Cat $1 }
| cons_expr_level                              { $1 }

(* Consing *)

cons_expr_level:
  bin_op(add_expr_level, "#", cons_expr_level) { E_Cons $1 }
| add_expr_level                               { $1 }

(* Arithmetic expressions *)

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level) { E_Add $1 }
| bin_op(add_expr_level, "-", mult_expr_level) { E_Sub $1 }
| mult_expr_level                              { $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*",   unary_expr_level) { E_Mult $1 }
| bin_op(mult_expr_level, "/",   unary_expr_level) { E_Div  $1 }
| bin_op(mult_expr_level, "mod", unary_expr_level) { E_Mod  $1 }
| unary_expr_level                                 { $1 }

(* Unary expressions *)

unary_expr_level:
  unary_op("-",   update_expr_level) { E_Neg $1 }
| unary_op("not", update_expr_level) { E_Not $1 }
| update_expr_level                  { $1 }

(* The rationale for the existence of [update_expr_level] is that we
   use the keyword "with" in two very different contexts: record
   updates and patches (see [instruction]) and the latter can derive
   the former, leading to a conflict, like so:

     patch (e) with ... // Is "(e)" part of "(e) with" or the patch? *)

update_expr_level:
  record_update       { E_Update    $1 }
| core_expr           { $1 }

core_expr:
  "<int>"             { E_Int       $1 }
| "<nat>"             { E_Nat       $1 }
| "<mutez>"           { E_Mutez     $1 }
| "<string>"          { E_String    $1 }
| "<verbatim>"        { E_Verbatim  $1 }
| "<bytes>"           { E_Bytes     $1 }
| "False"             { E_False     $1 }
| "True"              { E_True      $1 }
| "Unit"              { E_Unit      $1 }
| "nil"               { E_Nil       $1 }
| "None"              { E_None      $1 }
| some_expr           { E_Some      $1 }
| tuple_expr          { E_Tuple     $1 }
| list_expr           { E_List      $1 }
| record_expr         { E_Record    $1 }
| code_inj            { E_CodeInj   $1 }
| ctor_expr           { E_Ctor      $1 }
| map_expr            { E_Map       $1 }
| big_map_expr        { E_BigMap    $1 }
| set_expr            { E_Set       $1 }
| par(typed_expr)     { E_Typed     $1 }
| call_expr           { E_Call      $1 }
| par(expr)           { E_Par       $1 }
| path_expr           { $1 }

(* Map expressions (literal) *)

map_expr:
  injection("map",binding) { $1 mk_map }

big_map_expr:
  injection("big_map",binding) { $1 mk_big_map }

injection(Kind,element):
  Kind option(sep_or_term_list(element,";")) "end" {
    fun mk_kwd ->
      let kind, enclosing = mk_kwd $1, End $3
      and elements, terminator =
        match $2 with
          Some (elts, term) -> Some elts, term
        |              None -> None, None in
      let region = cover $1 $3
      and value  = {kind; enclosing; elements; terminator}
      in {region; value}
  }
| Kind "[" option(sep_or_term_list(element,";")) "]" {
    fun mk_kwd ->
      let kind, enclosing = mk_kwd $1, Brackets ($2,$4)
      and elements, terminator =
        match $3 with
          Some (elts, term) -> Some elts, term
        |              None -> None, None in
      let region = cover $1 $4
      and value  = {kind; enclosing; elements; terminator}
      in {region; value} }

(* Set expressions (literal) *)

set_expr:
  injection("set",expr) { $1 mk_set }

(* Constructed expressions *)

ctor_expr:
  ctor arguments { mk_reg (cover $1.region $2.region) ($1, Some $2) }
| ctor           { {$1 with value = ($1, None)} }

some_expr:
  "Some" arguments { mk_reg (cover $1 $2.region) ($1,$2) }

(* Function calls *)

call_expr:
  path_expr arguments {
    let start  = expr_to_region $1 in
    let region = cover start $2.region
    in mk_reg region ($1,$2)
  }

(* Typed expressions *)

typed_expr:
  disj_expr_level type_annotation { $1,$2 }

(* Paths *)

path_expr:
  value_in_module brackets(expr)?
| local_path brackets(expr)? { failwith "TODO" }

(*
map_lookup:
  local_path brackets(expr) {
    let region = cover (expr_to_region $1) $2.region in
    let value  = {map=$1; index=$2}
    in {region; value} }
 *)

value_in_module:
  module_path(field_path) { mk_mod_path $1 CST.expr_to_region }
| module_name "." "or"    { mk_E_Var $1 "or"      }
| module_name "." "and"   { mk_E_Var $1 "and"     }

field_path:
  variable "." nsepseq(selection,".") {
    let start  = $1.region
    and stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover start stop
    and value  = {record=$1; selector=$2; field_path=$3}
    in E_Proj {region; value} (* TODO *)
  }
| variable { E_Var $1 } (* TODO *)

local_path:
  field_path { failwith "TODO" }
| par(expr) "." nsepseq(selection,".") {
    let start  = expr_to_region $1
    and stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover start stop
    and value  = {record=$1; selector=$2; field_path=$3}
    in E_Proj {region; value}
  }

selection:
  field_name { FieldName $1 }
| "<int>"    { Component $1 }

(*
value_in_module:
  module_name "." "map"     { mk_E_Var $1 "map"     }
| module_name "." "big_map" { mk_E_Var $1 "big_map" }
| module_name "." "set"     { mk_E_Var $1 "set"     }
| module_name "." "remove"  { mk_E_Var $1 "remove"  }

local_path:
  record "." nsepseq(selection,".") {
    let start  = expr_to_region $1
    and stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover start stop
    and value  = {record=$1; selector=$2; field_path=$3}
    in E_Proj {region; value}
  }
| variable { E_Var $1 }

record:
  variable  { E_Var $1 }
| par(expr) { E_Par $1 }
 *)

(* Record expressions *)

record_expr:
  ne_injection("record", field_assignment) { $1 mk_record }

field_assignment:
  field_name "=" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = Complete {field_name=$1; assign=$2;
                           field_rhs=$3; attributes=[]}
    in {region; value}
  }
| field_name { {$1 with value = Punned $1} }

record_update:
  core_expr "with" ne_injection("record", field_path_assignment) {
    let updates = $3 mk_record in
    let region  = cover (expr_to_region $1) updates.region
    and value   = {record=$1; kwd_with=$2; updates}
    in {region; value} }

field_path_assignment:
  field_path "=" expr {
    let region = cover (expr_to_region $1) (expr_to_region $3)
    and value  = {field_lhs=$1; assign=$2; field_rhs=$3}
    in {region; value} }

code_inj:
  "[%<lang>" expr "]" {
    let region = cover $1.region $3
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

tuple_expr:
  par(tuple(expr)) { $1 }

tuple(item):
  item "," nsepseq(item,",") { Utils.nsepseq_cons $1 $2 $3 }

arguments:
  par(nsepseq(expr,",")) { $1 }

list_expr:
  injection("list",expr) { $1 mk_list }

(* Patterns *)

pattern:
  core_pattern "#" nsepseq(core_pattern,"#") {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region pattern_to_region value
    in P_Cons {region; value}
  }
| core_pattern { $1 }

core_pattern:
  "<int>"            { P_Int           $1 }
| "<nat>"            { P_Nat           $1 }
| "<bytes>"          { P_Bytes         $1 }
| "<string>"         { P_String        $1 }
| "Unit"             { P_Unit          $1 }
| "False"            { P_False         $1 }
| "True"             { P_True          $1 }
| "None"             { P_None          $1 }
| "nil"              { P_Nil           $1 }
| "_"                { P_Var (mk_wild $1) }
| variable           { P_Var           $1 }
| some_pattern       { P_Some          $1 }
| list_pattern       { P_List          $1 }
| ctor_pattern       { P_Ctor          $1 }
| tuple_pattern      { P_Tuple         $1 }
| record_pattern     { P_Record        $1 }
| par(pattern)
| par(typed_pattern) { P_Par           $1 }

typed_pattern:
  pattern type_annotation  {
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region (snd $2) in
    let region = cover start stop in
    let value  = {pattern=$1; type_annot=$2}
    in P_Typed {region; value} }

some_pattern:
  "Some" par(core_pattern) { mk_reg (cover $1 $2.region) ($1,$2) }

record_pattern:
  ne_injection("record", field(core_pattern)) { $1 mk_record }

field(rhs):
  field_name "=" rhs {
    let start  = $1.region
    and stop   = pattern_to_region $3 in
    let region = cover start stop
    and value  = Complete {field_name=$1; assign=$2;
                           field_rhs=$3; attributes=[]}
    in {region; value} }
| field_name { {$1 with value = Punned $1} }

list_pattern:
  injection("list",core_pattern) { $1 mk_list }

ctor_pattern:
  ctor tuple_pattern {
    let region = cover $1.region $2.region
    in mk_reg region ($1, Some $2)
  }
| ctor { {$1 with value = ($1, None)} }

tuple_pattern:
  par(tuple(pattern)) { $1 }
