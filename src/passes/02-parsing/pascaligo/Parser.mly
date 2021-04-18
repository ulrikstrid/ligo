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

let mk_un_op op arg =
  let region = cover op (expr_to_region arg)
  and value  = {op; arg}
  in {region; value}

let mk_bin_op arg1 op arg2 =
  let start  = expr_to_region arg1
  and stop   = expr_to_region arg2 in
  let region = cover start stop
  and value  = {arg1; op; arg2}
  in Region.{value; region}

let mk_set     region = `Set    region
let mk_list    region = `List   region
let mk_map     region = `Map    region
let mk_big_map region = `BigMap region
let mk_record  region = `Record region

let mk_wild region = {value="_"; region}

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

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <CST.t> contract
%type <CST.expr> interactive_expr

%on_error_reduce selected_expr
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
%on_error_reduce nsepseq(case_clause(test_clause),VBAR)
%on_error_reduce lhs
%on_error_reduce map_lookup
%on_error_reduce nsepseq(statement,SEMI)
%on_error_reduce ctor_pattern
%on_error_reduce core_expr
%on_error_reduce nsepseq(param_decl,SEMI)
%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce nsepseq(field_path_assignment,SEMI)
%on_error_reduce nsepseq(binding,SEMI)
%on_error_reduce nsepseq(expr,SEMI)
%on_error_reduce add_expr
%on_error_reduce unary_expr
%on_error_reduce const_decl
%on_error_reduce open_const_decl
%on_error_reduce fun_decl
%on_error_reduce variant
%on_error_reduce core_type
%on_error_reduce nsepseq(field_decl,SEMI)
%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce type_decl
%on_error_reduce cartesian
%on_error_reduce fun_type
%on_error_reduce cons_expr
%on_error_reduce cat_expr
%on_error_reduce set_membership
%on_error_reduce disj_expr
%on_error_reduce nsepseq(variant,VBAR)
%on_error_reduce core_pattern
%on_error_reduce nsepseq(type_expr,COMMA)
%on_error_reduce expr
%on_error_reduce nsepseq(expr,COMMA)
%on_error_reduce option(SEMI)
%on_error_reduce option(VBAR)
%on_error_reduce projection
%on_error_reduce nseq(declaration)
%on_error_reduce option(arguments)
%on_error_reduce path
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
%inline struct_name : "<ident>"  { $1 }
%inline module_name : "<uident>" { $1 }
%inline ctor        : "<uident>" { $1 }

(* Main *)

contract:
  declarations EOF { {decl=$1; eof=$2} }

(* Declarations *)

declarations:
  nseq(declaration) { $1 }

declaration:
  type_decl     { D_Type      $1 }
| const_decl    { D_Const     $1 }
| fun_decl      { D_Fun       $1 }
| module_decl   { D_Module    $1 }
| module_alias  { D_ModAlias  $1 }
| "<directive>" { D_Directive $1 }

open_declaration:
  open_type_decl    { D_Type     $1 }
| open_const_decl   { D_Const    $1 }
| open_fun_decl     { D_Fun      $1 }
| open_module_decl  { D_Module   $1 }
| open_module_alias { D_ModAlias $1 }

(* Module declarations *)

open_module_decl:
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

module_decl:
  open_module_decl ";"? {
    {$1 with value = {$1.value with terminator=$2}} }

open_module_alias:
  "module" module_name "is" nsepseq(module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x.region) $4 in
    let region = cover $1 stop in
    let value  = {kwd_module=$1; alias=$2; kwd_is=$3;
                  mod_path=$4; terminator=None}
    in {region; value} }

module_alias:
  open_module_alias ";"? {
    {$1 with value = {$1.value with terminator=$2}} }

(* Type declarations *)

open_type_decl:
  "type" type_name "is" type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $1 stop in
    let value  = {kwd_type=$1; name=$2; kwd_is=$3;
                  type_expr=$4; terminator=None}
    in {region; value} }

type_decl:
  open_type_decl ";"? {
    {$1 with value = {$1.value with terminator=$2}} : type_decl Region.reg }

type_annot:
  ":" type_expr { $1,$2 }

type_expr:
  fun_type | sum_type | record_type { $1 }

fun_type:
  cartesian "->" fun_type {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    T_Fun {region; value = $1,$2,$3}
  }
| cartesian { $1 }

cartesian:
  core_type "*" nsepseq(core_type,"*") {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region type_expr_to_region value
    in T_Prod {region; value}
  }
| core_type { $1 }

core_type:
  type_name type_tuple {
    let region = cover $1.region $2.region
    in T_Ctor {region; value = $1,$2}
  }
| "map" type_tuple {
    let region    = cover $1 $2.region in
    let type_ctor = {value="map"; region=$1}
    in T_Ctor {region; value = type_ctor, $2}
  }
| "big_map" type_tuple {
    let region    = cover $1 $2.region in
    let type_ctor = {value="big_map"; region=$1}
    in T_Ctor {region; value = type_ctor, $2}
  }
| "set" par(type_expr) {
    let total = cover $1 $2.region in
    let type_ctor = {value="set"; region=$1} in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = {region; value={lpar; inside=inside,[]; rpar}}
    in T_Ctor {region=total; value = type_ctor, tuple}
  }
| "list" par(type_expr) {
    let total = cover $1 $2.region in
    let type_ctor = {value="list"; region=$1} in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = {region; value={lpar; inside=inside,[]; rpar}}
    in T_Ctor {region=total; value = type_ctor, tuple}
  }
| type_name      { T_Var     $1 }
| "_"            { T_Wild    $1 }
| "<string>"     { T_String  $1 }
| "<int>"        { T_Int     $1 }
| type_in_module { T_ModPath $1 }
| par(type_expr) { T_Par     $1 }

type_in_module:
  module_path(type_name) { mk_mod_path $1 (fun x -> x.region) }

module_path(selected):
  module_name "." module_path(selected) {
    let (head, tail), selected = $3 in
    (($1,$2), head::tail), selected
  }
| module_name "." selected { (($1,$2), []), $3 }

type_tuple:
  par(nsepseq(type_expr,",")) { $1 }

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
| nseq("[@<attr>]") ctor "of" fun_type {
    let attributes = Utils.nseq_to_list $1 in
    let value      = {ctor = $2; arg = Some ($3,$4); attributes}
    and stop       = type_expr_to_region $4 in
    let region     = cover (fst $1).region stop
    in {region; value}
  }
| ctor "of" fun_type {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {ctor       = $1;
                  arg        = Some ($2,$3);
                  attributes = []}
    in {region; value} }

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
    and value  = {attributes=$1; field_name=$2; field_type=($3,$4)}
    in {region; value} }

fun_expr:
  "function" parameters ioption(type_annot) "is" expr {
    let stop   = expr_to_region $5 in
    let region = cover $1 stop
    and value  = {kwd_function=$1; param=$2; ret_type=$3;
                  kwd_is=$4; return=$5}
    in {region; value} }

(* Function declarations *)

open_fun_decl:
  seq("[@<attr>]") ioption("recursive") "function" fun_name parameters
  ioption(type_annot) "is" expr {
    let stop   = expr_to_region $8 in
    let region = match first_region $1 with
                   Some start -> cover start stop
                 | None -> match $2 with
                             Some start -> cover start stop
                           |       None -> cover $3 stop
    and value  = {attributes=$1; kwd_recursive=$2; kwd_function=$3;
                  fun_name=$4; param=$5;
                  ret_type=$6; kwd_is=$7; return=$8;terminator=None}
    in {region; value} }

fun_decl:
  open_fun_decl ";"? {
    {$1 with value = {$1.value with terminator=$2}} }

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
    and value  = {kwd_var    = $1;
                  var        = mk_wild $2;
                  param_type = $3}
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
    and value  = {kwd_const  = $1;
                  var        = mk_wild $2;
                  param_type = $3}
    in ParamConst {region; value} }

param_type:
  ":" fun_type { $1,$2 }

block:
  "begin" sep_or_term_list(statement,";") "end" {
     let statements, terminator = $2
     and enclosing : block_enclosing = BeginEnd ($1,$3) in
     let region    = cover $1 $3
     and value     = {enclosing; statements; terminator}
     in {region; value}
  }
| "block" "{" sep_or_term_list(statement,";") "}" {
     let statements, terminator = $3
     and enclosing : block_enclosing = Braces (Some $1, $2, $4) in
     let region    = cover $1 $4
     and value     = {enclosing; statements; terminator}
     in {region; value}
  }
| "{" sep_or_term_list(statement,";") "}" {
     let statements, terminator = $2
     and enclosing : block_enclosing = Braces (None,$1,$3) in
     let region    = cover $1 $3
     and value     = {enclosing; statements; terminator}
     in {region; value} }

statement:
  instruction      { S_Instr   $1 }
| open_declaration { S_Decl    $1 }
| open_var_decl    { S_VarDecl $1 }

open_const_decl:
  seq("[@<attr>]") "const" unqualified_decl("=") {
    let name, const_type, equal, init, stop = $3 in
    let region= match first_region $1 with
                  None -> cover $2 stop
                | Some start -> cover start stop
    and value  = {attributes=$1; kwd_const=$2; name;
                  const_type; equal; init; terminator=None}
    in {region; value} }

open_var_decl:
  "var" unqualified_decl(":=") {
    let name, var_type, assign, init, stop = $2 in
    let region = cover $1 stop
    and value  = {kwd_var=$1; name; var_type; assign; init;
                  terminator=None}
    in {region; value} }

unqualified_decl(OP):
  variable ioption(type_annot) OP expr {
    let region = expr_to_region $4
    in $1, $2, $3, $4, region }

const_decl:
  open_const_decl ";"? {
    {$1 with value = {$1.value with terminator=$2}} }

instruction:
  assignment   { I_Assign      $1 }
| proc_call    { I_Call        $1 }
| case_instr   { I_Case        $1 }
| conditional  { I_Cond        $1 }
| for_int      { I_For         $1 }
| for_in       { I_ForIn       $1 }
| map_patch    { I_MapPatch    $1 }
| map_remove   { I_MapRemove   $1 }
| record_patch { I_RecordPatch $1 }
| "skip"       { I_Skip        $1 }
| set_patch    { I_SetPatch    $1 }
| set_remove   { I_SetRemove   $1 }
| while_loop   { I_While       $1 }

set_remove:
  "remove" expr "from" "set" path {
    let region = cover $1 (path_to_region $5) in
    let value  = {kwd_remove=$1; element=$2;
                  kwd_from=$3; kwd_set=$4; set=$5}
    in {region; value} }

map_remove:
  "remove" expr "from" "map" path {
    let region = cover $1 (path_to_region $5) in
    let value  = {kwd_remove=$1; key=$2;
                  kwd_from=$3; kwd_map=$4; map=$5}
    in {region; value} }

set_patch:
  "patch" path "with" ne_injection("set",expr) {
    let set_inj = $4 mk_set in
    let region  = cover $1 set_inj.region in
    let value   = {kwd_patch=$1; path=$2; kwd_with=$3; set_inj}
    in {region; value} }

map_patch:
  "patch" path "with" ne_injection("map",binding) {
    let map_inj = $4 mk_map in
    let region  = cover $1 map_inj.region in
    let value   = {kwd_patch=$1; path=$2; kwd_with=$3; map_inj}
    in {region; value} }

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

binding:
  expr "->" expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {source=$1; arrow=$2; image=$3}
    in {region; value} }

record_patch:
  "patch" path "with" record_expr {
    let region = cover $1 $4.region in
    let value  = {kwd_patch=$1; path=$2; kwd_with=$3; record_inj=$4}
    in {region; value} }

proc_call:
  fun_call { $1 }

(* Conditionals instructions *)

conditional:
  "if" expr "then" test_clause ";"? "else" test_clause {
    let region = cover $1 (test_clause_to_region $7) in
    let value : test_clause conditional = {
      kwd_if=$1; test=$2; kwd_then=$3; ifso=$4; terminator=$5;
      kwd_else=$6; ifnot=$7}
    in {region; value} }

test_clause:
  instruction { ClauseInstr $1 }
| block       { ClauseBlock $1 }

(* Case instructions and expressions *)

case_instr:
  case(test_clause) { $1 test_clause_to_region }

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

assignment:
  lhs ":=" rhs {
    let stop   = expr_to_region $3 in
    let region = cover (lhs_to_region $1) stop
    and value  = {lhs=$1; assign=$2; rhs=$3}
    in {region; value} }

rhs:
  expr { $1 }

lhs:
  path       {    Path $1 }
| map_lookup { MapPath $1 }

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

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  case(expr) { E_Case ($1 expr_to_region) }
| fun_expr   { E_Fun   $1                 }
| block_with { E_Block $1                 }
| cond_expr  { E_Cond  $1                 }
| disj_expr  { $1                         }

block_with:
  block "with" expr {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop in
    let value  = {block=$1; kwd_with=$2; expr=$3}
    in {region; value} }

cond_expr:
  "if" expr "then" expr ";"? "else" expr {
    let region = cover $1 (expr_to_region $7) in
    let value : expr conditional = {
      kwd_if=$1; test=$2; kwd_then=$3; ifso=$4; terminator=$5;
      kwd_else=$6; ifnot=$7}
    in {region; value} }

disj_expr:
  disj_expr "or" conj_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3} in
    E_Or {region; value}
  }
| conj_expr { $1 }

conj_expr:
  conj_expr "and" set_membership {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in E_And {region; value}
  }
| set_membership { $1 }

set_membership:
  core_expr "contains" set_membership {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {set=$1; kwd_contains=$2; element=$3}
    in E_SetMem {region; value}
  }
| comp_expr { $1 }

comp_expr:
  comp_expr "<"   cat_expr { E_Lt    (mk_bin_op $1 $2 $3) }
| comp_expr "<="  cat_expr { E_Leq   (mk_bin_op $1 $2 $3) }
| comp_expr ">"   cat_expr { E_Gt    (mk_bin_op $1 $2 $3) }
| comp_expr ">="  cat_expr { E_Geq   (mk_bin_op $1 $2 $3) }
| comp_expr "="   cat_expr { E_Equal (mk_bin_op $1 $2 $3) }
| comp_expr "=/=" cat_expr { E_Neq   (mk_bin_op $1 $2 $3) }
| cat_expr                 { $1 }

cat_expr:
  cons_expr "^" cat_expr { E_Cat (mk_bin_op $1 $2 $3) }
| cons_expr              { $1 }

cons_expr:
  add_expr "#" cons_expr { E_Cons (mk_bin_op $1 $2 $3) }
| add_expr               { $1 }

add_expr:
  add_expr "+" mult_expr { E_Add (mk_bin_op $1 $2 $3) }
| add_expr "-" mult_expr { E_Sub (mk_bin_op $1 $2 $3) }
| mult_expr              { $1 }

mult_expr:
  mult_expr "*"   unary_expr { E_Mult (mk_bin_op $1 $2 $3) }
| mult_expr "/"   unary_expr { E_Div  (mk_bin_op $1 $2 $3) }
| mult_expr "mod" unary_expr { E_Mod  (mk_bin_op $1 $2 $3) }
| unary_expr                 { $1 }

unary_expr:
  "-" core_expr   { E_Neg (mk_un_op $1 $2) }
| "not" core_expr { E_Not (mk_un_op $1 $2) }
| core_expr       { $1 }

core_expr:
  "<int>"             { E_Int       $1 }
| "<nat>"             { E_Nat       $1 }
| "<mutez>"           { E_Mutez     $1 }
| "<ident>"           { E_Var       $1 }
| "<string>"          { E_String    $1 }
| "<verbatim>"        { E_Verbatim  $1 }
| "<bytes>"           { E_Bytes     $1 }
| "False"             { E_False     $1 }
| "True"              { E_True      $1 }
| "Unit"              { E_Unit      $1 }
| "nil"               { E_Nil       $1 }
| "None"              { E_None      $1 }
| par(annot_expr)     { E_Annot     $1 }
| tuple_expr          { E_Tuple     $1 }
| list_expr           { E_List      $1 }
| value_in_module     { E_ModPath   $1 }
| map_lookup          { E_MapLookup $1 }
| record_expr         { E_Record    $1 }
| record_update       { E_Update    $1 }
| code_inj            { E_CodeInj   $1 }
| some_expr           { E_Some      $1 }
| ctor_expr           { E_Ctor      $1 }
| map_expr            { E_Map       $1 }
| big_map_expr        { E_BigMap    $1 }
| set_expr            { E_Set       $1 }
| call_or_par_or_proj {             $1 }

map_expr:
  injection("map",binding) { $1 mk_map }

big_map_expr:
  injection("big_map",binding) { $1 mk_big_map }

set_expr:
  injection("set",expr) { $1 mk_set }

map_lookup:
  path brackets(expr) {
    let region = cover (path_to_region $1) $2.region in
    let value  = {path=$1; index=$2}
    in {region; value} }

ctor_expr:
  ctor arguments {
    let region = cover $1.region $2.region in
    {region; value = ($1, Some $2)}
  }
| ctor { {$1 with value=$1,None} }

some_expr:
  "Some" arguments {
    let region = cover $1 $2.region in
    {region; value = ($1,$2)} }

call_or_par_or_proj:
  par(expr) arguments? {
    let parens = E_Par $1 in
    match $2 with
      None -> parens
    | Some args ->
        let region = cover $1.region args.region
        in E_Call {region; value = (parens, args)}
  }
| projection arguments? {
    let project = E_Proj $1 in
    match $2 with
      None -> project
    | Some args ->
        let region = cover $1.region args.region
        in E_Call {region; value = (project, args)}
  }
| fun_call { E_Call $1 }

annot_expr:
  disj_expr type_annot { $1,$2 }

path:
  variable   { Name $1 }
| projection { Path $1 }

projection:
  struct_name "." nsepseq(selection,".") {
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1.region stop
    and value  = {struct_name=$1; selector=$2; field_path=$3}
    in {region; value} }

selection:
  field_name { FieldName $1 }
| "<int>"    { Component $1 }

value_in_module:
  module_path(selected_expr) { mk_mod_path $1 CST.expr_to_region }

selected_expr:
  field_name    { E_Var $1                          }
| "map"         { E_Var {value="map";    region=$1} }
| "or"          { E_Var {value="or";     region=$1} }
| "and"         { E_Var {value="and";    region=$1} }
| "remove"      { E_Var {value="remove"; region=$1} }
| projection    { E_Proj $1                         }

record_expr:
  ne_injection("record",field_assignment) { $1 mk_record }

field_assignment:
  field_name "=" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {field_name=$1; assignment=$2; field_expr=$3}
    in {region; value} }

record_update:
  path "with" ne_injection("record", field_path_assignment) {
    let updates = $3 mk_record in
    let region  = cover (path_to_region $1) updates.region in
    let value   = {record=$1; kwd_with=$2; updates}
    in {region; value} }

field_path_assignment:
  path "=" expr {
    let region = cover (path_to_region $1) (expr_to_region $3)
    and value  = {field_path=$1; assignment=$2; field_expr=$3}
    in {region; value} }

code_inj:
  "[%<lang>" expr "]" {
    let region = cover $1.region $3
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

fun_call:
  fun_name arguments {
    let region = cover $1.region $2.region
    in {region; value = (E_Var $1), $2}
  }
| value_in_module arguments {
    let region = cover $1.region $2.region
    in {region; value = (E_ModPath $1), $2} }

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
  "_"           { P_Var (mk_wild $1) }
| "<int>"       { P_Int           $1 }
| "<nat>"       { P_Nat           $1 }
| "<bytes>"     { P_Bytes         $1 }
| "<string>"    { P_String        $1 }
| "Unit"        { P_Unit          $1 }
| "False"       { P_False         $1 }
| "True"        { P_True          $1 }
| "None"        { P_None          $1 }
| "nil"         { P_Nil           $1 }
| variable      { P_Var           $1 }
| some_pattern  { P_Some          $1 }
| list_pattern  { P_List          $1 }
| ctor_pattern  { P_Ctor          $1 }
| tuple_pattern { P_Tuple         $1 }
| par(pattern)  { P_Par           $1 }

some_pattern:
  "Some" par(core_pattern) {
    let region = cover $1 $2.region
    in {region; value=($1,$2)} }

list_pattern:
  injection("list",core_pattern) { $1 mk_list }

ctor_pattern:
  ctor tuple_pattern {
    let region = cover $1.region $2.region in
    {region; value = ($1, Some $2)}
  }
| ctor { {$1 with value = ($1, None)} }

tuple_pattern:
  par(tuple(pattern)) { $1 }
