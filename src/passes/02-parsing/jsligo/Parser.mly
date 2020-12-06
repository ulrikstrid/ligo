%{
(* START HEADER *)

[@@@warning "-42"]
[@@@warning "-33"]
[@@@warning "-32"]

open Simple_utils.Region
module CST = Cst.Jsligo
open CST

(* Utilities *)


let first_region = function
  [] -> None
| x::_ -> Some x.Region.region

(* END HEADER *)
%}

(* To solve the dangling else problem. *)
%nonassoc below_ELSE
%nonassoc Else

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <Cst.Jsligo.t> contract
%type <Cst.Jsligo.expr> interactive_expr

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

(*
%inline type_name        : "<ident>"  { $1 }
%inline field_name       : "<ident>"  { $1 }
%inline struct_name      : "<ident>"  { $1 }
%inline module_name      : "<constr>" { $1 }
*)

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empty semicolon-separated values between brackets *)

list__(item):
  "[" sep_or_term_list(item,",")? "]" {
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

variable_statement:
  "<ident>" {  SVar $1 }

block_statement:
  "{" statements "}" {
    let region = cover $1 $3
    and compound = Some (Braces ($1, $3)) in
    let elements = Some $2 in
    let value = {compound; elements; terminator=None}
    in SBlock {region; value}
  }

return_statement: 
  "return" expr? {
    let region = cover $1 Region.ghost in
    let value = {
      kwd_return  = $1;
      expr        = $2;
    }
    in
    SReturn {region; value}
  }

if_else_statement:
  "if" par(expr) statement "else" statement {
    let region = cover $1 Region.ghost in
    let value = {
      kwd_if = $1;
      test   = $2.value;
      ifso   = $3;
      ifnot  = Some ($4, $5);
    }
    in
    SCond {region; value}
}
| "if" par(expr) statement %prec below_ELSE {
    let region = cover $1 Region.ghost in
    let value = {
      kwd_if = $1;
      test   = $2.value;
      ifso   = $3;
      ifnot  = None;
    }
    in
    SCond {region; value}
  }

initializer_:
  "=" expr {
    ($1, $2)
  }

rest:
  "..." "<ident>" { 
    let region = cover $1 $2.region in
    let value = {
      ellipsis = $1;
      rest     = $2;
    } in
    PRest {
      region;
      value
    }
  }

object_binding_property:
  "<ident>" initializer_?  { 
    match $2 with 
    | Some (eq, expr) -> 
      let region = cover $1.region Region.ghost in
      let value = {
         property = $1;
         eq;
         value = expr;
      } in
      PAssign {
        region; 
        value
      }
    | None -> 
      PVar $1 
  }
| "<ident>" ":" binding   { 
    let region = cover $1.region Region.ghost in
    let value = {
      property    = $1;
      colon  = $2;
      target = $3;
    } in
    PDestruct {
      region;
      value
    }
}

object_binding_pattern_items:
  object_binding_property "," object_binding_pattern_items? {
    match $3 with 
    | Some s -> Utils.nsepseq_cons $1 $2 s
    | None -> ($1, [])
  }
| object_binding_property { ($1, []) }
| rest                    { ($1, []) }

object_binding_pattern: 
  "{" object_binding_pattern_items "}" { 
    let region = cover $1 $3 in
    let value = {
      lbrace = $1;
      inside = $2;
      rbrace = $3;
    } in
    PObject { region; value };
  }

array_binding_pattern_item:
  /* empty  */          { PWild }
| rest                  { $1 }
| "<ident>"             { PVar $1 }
| array_binding_pattern { $1 }

array_binding_pattern_items:
  array_binding_pattern_item "," array_binding_pattern_items { Utils.nsepseq_cons $1 $2 $3 }
| array_binding_pattern_item                                 { ($1, []) }

array_binding_pattern:
  "[" array_binding_pattern_items "]"  {
    let region = cover $1 $3 in
    let value = {
      lbracket = $1;
      inside = $2;
      rbracket = $3
    } in
    PArray { region; value }
  }

binding:
  "<ident>" initializer_?              { 
    {
      binders  = PVar $1;
      lhs_type = None; (* TODO *)
      let_rhs  = match $2 with 
      | Some (eq, expr) -> Some { eq; expr }
      | None -> None
    }
   }
| object_binding_pattern initializer_? { 
  {
    binders  = $1;
    lhs_type = None; (* TODO *)
    let_rhs  = match $2 with 
    | Some (eq, expr) -> Some { eq; expr }
    | None -> None
  }
}
| array_binding_pattern initializer_? {  
  {
    binders  = $1;
    lhs_type = None; (* TODO *)
    let_rhs = match $2 with 
    | Some (eq, expr) -> Some { eq; expr }
    | None -> None
  }
}

binding_list:
  nsepseq(binding, ",") {
    $1
  }

declaration:
  "let" binding_list {
    let region = cover $1 Region.ghost in
    let value = {
      kwd_let    = $1;
      bindings   = $2;
      attributes = []
    }
    in 
    SLet { region; value }
  }
| "const" binding_list {
    let region = cover $1 Region.ghost in
    let value = {
      kwd_const  = $1;
      bindings   = $2;
      attributes = []
    }
    in 
    SConst { region; value }
}

type_statement:
  "type" { (* TODO *) failwith "333" }

switch_statement:
  "switch" "(" expr ")" "{" nseq(case_block) "}" {
    let region = cover $1 $7 in
    let value = {
      kwd_switch  = $1;
      lpar        = $2;
      expr        = $3;
      rpar        = $4;
      lbrace      = $5;
      cases       = $6;
      rbrace      = $7;
    } in 
    SSwitch {
      region;
      value
    }
  }

case_block: 
  "case" expr ":" statements? {
    Switch_case {
      kwd_case   = $1;
      expr       = $2;
      colon      = $3;
      statements = $4;
    }
  }
| "default" ":" statements? {
  Switch_default_case {
    kwd_default = $1;
    colon       = $2;
    statements  = $3;
  }
}

statement:
  variable_statement
// | expression_statement
| block_statement
| if_else_statement
| switch_statement
| type_statement
| return_statement 
| declaration
  { $1 }

statements:
  statement ";" statements? {
    match $3 with 
    | Some s ->  Utils.nsepseq_cons $1 $2 s
    | None -> ($1, [])
  }
| statement { $1, [] }  

contract:
  statements EOF { {statements=$1; eof=$2} }

(* Expressions *)

expr:

  "(" { failwith "1" }

// expression_statement:
//   (* not: {, function, async [no LineTerminator here] function, class, let [ *)


interactive_expr:
  EOF { failwith "a2" }
