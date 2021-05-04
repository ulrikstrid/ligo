(* PRINTING THE TOKENS *)

(* This module produces tokens reconstructed from the Concrete Syntax
   Tree (CST) in the same way they should be produced by the lexer. In
   other words, the leaves of the CST are printed as tokens. This
   enables to test the transmission of the tokens from the lexer to
   the parser. *)

[@@@warning "-42"]
[@@@coverage exclude_file]

(* Internal dependencies *)

open CST

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region
open! Region

(* Utilities *)

let sprintf = Printf.sprintf

type           'a nseq = 'a Utils.nseq
type ('a,'sep) nsepseq = ('a,'sep) Utils.nsepseq
type ('a,'sep)  sepseq = ('a,'sep) Utils.sepseq

(* STATE *)

(* The printing of the tokens makes use of a threaded data structure:
   the _state_. The printing is done to the string buffer bound to the
   field [buffer], which is imperatively updated (see module
   [Stdlib.Buffer].) *)

type state = <
  offsets : bool;
  mode    : [`Point | `Byte];
  buffer  : Buffer.t
>

let mk_state ?(buffer=Buffer.create 131) ~offsets mode =
  object
    method offsets  = offsets;
    method mode     = mode;
    method buffer   = buffer
  end

(* The names of the printing functions are all prefixed by
   "print_". The rest of the name is either

     * the name of the type whose value is printed, for example
       [print_declaration] prints values of type [declaration];

     * the name of a CST constructor, for example, [print_E_Int],
       meaning that the argument of the constructor [CST.E_Int] is
       printed; when some constructors belong to different types, we
       compromise.

     * a generic name, like [print_token] for tokens which are
       keywords or symbols; another example is [print_token_opt] when
       we have an optional token. Or higher-order functions like
       [print_injection], [print_option], [print_nsepseq] etc.

   Another design pattern we used here was to make all pattern
   matching on CST constructors a simple routing function, that is,
   without other logic. For example:

   and print_type_expr state = function
     T_Ctor    t -> print_T_Ctor    state t
   | T_Fun     t -> print_T_Fun     state t
   ...

   This means that those functions can be ignored by the maintainers
   if they know the data constructor. *)

let compact state (region: Region.t) =
  region#compact ~offsets:state#offsets state#mode

(* HIGHER-ORDER PRINTERS *)

(* Printing optional values *)

let print_option : state -> (state -> 'a -> unit) -> 'a option -> unit =
  fun state print -> function
         None -> ()
  | Some node -> print state node

(* The functions [print_nsepseq], [print_sepseq] and [print_nseq]
   print values of types [Utils.nsepseq], [Utils.sepseq] and
   [Utils.nseq], respectively. *)

let print_nsepseq :
  state -> (state -> 'a -> unit) -> string -> ('a, region) nsepseq -> unit =
  fun state print sep (head, tail) ->
    let print_aux (sep_reg, item) =
      let sep_line =
        sprintf "%s: %s\n" (compact state sep_reg) sep in
      Buffer.add_string state#buffer sep_line;
      print state item
    in print state head; List.iter print_aux tail

let print_sepseq :
  state -> (state -> 'a -> unit) -> string -> ('a, region) sepseq -> unit =
  fun state print sep ->
    print_option state (fun state -> print_nsepseq state print sep)

let print_nseq : state -> (state -> 'a -> unit) -> 'a nseq -> unit =
  fun state print -> Utils.nseq_iter (print state)

(* When printing a sequence, e.g., by means of [nsepseq], whose items
   are of type [Region.reg], the function [strip] is useful to strip
   the items from their region (type [Region.t]). *)

let strip : 'a.(state -> 'a -> unit) -> state -> 'a reg -> unit =
  fun print state node -> print state node.value

(* PRINTING TOKENS (LEAVES) *)

(* Guideline: When destructuring a value [v] of type [Region.t], use
   the following order: [let {value; region} = v in ...]

   The following functions are used by others to make strings,
   vertbatim strings, integers and bytes. See for example functions
   [print_T_String], [print_E_Nat], [print_P_Int] etc.

   Note that, contrary to the following utility functions,
   higher-order printers, like [print_case], [print_conditional]
   etc. are defined when they are first needed.*)

(* Keywords and symbols *)

let print_token state token region =
  let line =
    sprintf "%s: %s\n" (compact state region) token
  in Buffer.add_string state#buffer line

let print_token_opt state lexeme =
  print_option state (fun state -> print_token state lexeme)

(* Strings *)

let print_string state {region; value} =
  let line =
    sprintf "%s: String %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_verbatim state {region; value} =
  let line =
    sprintf "%s: Verbatim %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

(* Integers and natural numbers *)

let print_int state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Int (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let print_nat state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Nat (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

(* Bytes *)

let print_bytes state {region; value} =
  let lexeme, abstract = value in
  let line = sprintf "%s: Bytes (%S, \"0x%s\")\n"
                     (compact state region) lexeme
                     (Hex.show abstract)
  in Buffer.add_string state#buffer line

(* Mutez *)

let print_mutez state {region; value=lex,z} =
  let line =
    sprintf "Mutez %s (%s)" lex (Z.to_string z)
  in print_token state line region

(* Identifiers *)

let print_ident state {region; value} =
  let line =
    sprintf "%s: Ident %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_UIdent state {region; value} =
  let line =
    sprintf "%s: UIdent %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_ctor = print_UIdent

(* Attributes *)

let print_attribute state {region; value} =
  let line =
    sprintf "%s: Attr %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

(* HIGHER-ORDER PRINTERS *)

let print_par : state -> (state -> 'a -> unit) -> 'a par reg -> unit =
  fun state print {value; _} ->
    print_token state "LPAR" value.lpar;
    print       state value.inside;
    print_token state "RPAR" value.rpar

let print_brackets : 'state -> (state -> 'a -> unit) -> 'a brackets reg -> unit =
  fun state print ({value; _}: 'a brackets reg) ->
    print_token state "LBRACKET" value.lbracket;
    print       state value.inside;
    print_token state "LBRACKET" value.rbracket

(* PRINTING THE CST *)

let rec print_cst state (node : cst) =
  print_nseq  state print_declaration node.decl;
  print_token state "EOF" node.eof

(* DECLARATIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_declaration state = function
  D_Directive d -> print_D_Directive state d
| D_Let       d -> print_D_Let       state d
| D_Module    d -> print_D_Module    state d
| D_ModAlias  d -> print_D_ModAlias  state d
| D_Type      d -> print_D_Type      state d

(* Preprocessing directives *)

and print_D_Directive state dir =
  let s =
    Directive.to_string ~offsets:state#offsets state#mode dir
  in Buffer.add_string state#buffer s

(* Constant declarations *)

and print_D_Let state (node : let_decl reg) =
  let kwd_let, kwd_rec, let_binding, attributes = node.value in
  print_attributes   state attributes;
  print_token        state "Let" kwd_let;
  print_token_opt    state "Rec" kwd_rec;
  print_let_binding  state let_binding

and print_attributes state = List.iter (print_attribute state)

and print_let_binding state (node : let_binding) =
  print_nseq   state print_pattern node.binders;
  print_option state print_type_annot node.lhs_type;
  print_token  state "EQ" node.eq;
  print_expr   state node.let_rhs

and print_type_annot state (colon, type_expr) =
  print_token     state "COLON" colon;
  print_type_expr state type_expr;

(* Module declarations *)

and print_D_Module state (node : module_decl reg) =
  let node = node.value in
  print_token  state "Module" node.kwd_module;
  print_var    state node.name;
  print_token  state "EQ" node.eq;
  print_token  state "Struct" node.kwd_struct;
  print_tokens state node.structure;
  print_token  state "End" node.kwd_end

(* Module aliases *)

and print_D_ModAlias state (node : module_alias reg) =
  let node = node.value in
  print_token   state "Module" node.kwd_module;
  print_var     state node.alias;
  print_token   state "EQ" node.eq;
  print_nsepseq state print_var "DOT" node.mod_path

(* Type declarations *)

and print_D_Type state (node : type_decl reg) =
  let node = node.value in
  print_token     state "Type" node.kwd_type;
  print_var       state node.name;
  print_token     state "EQ" node.eq;
  print_type_expr state node.type_expr

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_T_Ctor] comes before [print_T_Fun]. *)

and print_type_expr state = function
  T_Ctor    t -> print_T_Ctor    state t
| T_Fun     t -> print_T_Fun     state t
| T_Int     t -> print_T_Int     state t
| T_ModPath t -> print_T_ModPath state t
| T_Par     t -> print_T_Par     state t
| T_Prod    t -> print_T_Prod    state t
| T_Record  t -> print_T_Record  state t
| T_String  t -> print_T_String  state t
| T_Sum     t -> print_T_Sum     state t
| T_Var     t -> print_T_Var     state t
| T_Wild    t -> print_T_Wild    state t

(* General constructors *)

and print_T_Ctor state (node : (type_ctor * t_ctor_args) reg) =
  let type_ctor, t_ctor_args = node.value in
  match t_ctor_args with
    T_Unary t_expr ->
      print_type_expr  state t_expr;
      print_var        state type_ctor
  | T_Tuple t_tuple ->
      print_type_tuple state t_tuple;
      print_var        state type_ctor

and print_type_tuple state (node : type_tuple) =
  let print state = print_nsepseq state print_type_expr "COMMA"
  in print_par state print node.value

(* Functional types *)

and print_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  let domain, arrow, range = node.value in
  print_type_expr state domain;
  print_token     state "ARROW" arrow;
  print_type_expr state range

(* The integer type *)

and print_T_Int state = print_int state

(* Module paths *)

and print_T_ModPath state = print_module_path state print_ident

and print_module_path :
  'a.state -> (state -> 'a -> unit ) -> 'a module_path reg -> unit =
  fun state print {value; _} ->
    print_nsepseq state print_UIdent "DOT" value.module_path;
    print_token   state "DOT" value.selector;
    print         state value.field

(* Parenthesised type expressions *)

and print_T_Par state = print_par state print_type_expr

(* Product types *)

and print_T_Prod state (node : cartesian) =
  print_nsepseq state print_type_expr "TIMES" node.value

(* Record types *)

and print_T_Record state = print_ne_injection state print_field_decl

and print_field_decl state (node : field_decl reg) =
  let node = node.value in
  print_attributes state node.attributes;
  print_var        state node.field_name;
  print_token      state "COLON" node.colon;
  print_type_expr  state node.field_type

and print_ne_injection :
      'a.state -> (state -> 'a -> unit) -> 'a ne_injection reg -> unit =
  fun state print {value; _} ->
  print_attributes state value.attributes;
  print_option     state print_open_compound value.compound;
  print_nsepseq    state print "SEMI" value.ne_elements;
  print_option     state print_semi value.terminator;
  print_option     state print_close_compound value.compound

and print_open_compound state = function
    BeginEnd (kwd_begin, _) -> print_token state "Begin"    kwd_begin
  | Braces   (lbrace,    _) -> print_token state "LBRACE"   lbrace
  | Brackets (lbracket,  _) -> print_token state "LBRACKET" lbracket

and print_close_compound state = function
    BeginEnd (_,  kwd_end) -> print_token state "End"      kwd_end
  | Braces   (_,   rbrace) -> print_token state "RBRACE"   rbrace
  | Brackets (_, rbracket) -> print_token state "RBRACKET" rbracket

and print_T_String state = print_string state

(* Sum types *)

and print_T_Sum state (node : sum_type reg) =
  let node = node.value in
  print_attributes state node.attributes;
  print_token_opt  state "VBAR" node.lead_vbar;
  print_nsepseq    state (strip print_variant) "VBAR" node.variants

and print_variant state (node : variant) =
  print_attributes state node.attributes;
  print_ctor       state node.ctor;
  print_option     state print_of_type_expr node.arg

and print_of_type_expr state (kwd_of, type_expr) =
  print_token     state "Of" kwd_of;
  print_type_expr state type_expr

(* A type variable *)

and print_T_Var state = print_ident state

(* A catch-all variable (a.k.a. joker) *)

and print_T_Wild state = print_token state "WILD"

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_I_Bytes] comes before [print_P_Cons]. *)

and print_pattern state = function
  P_Bytes  p -> print_P_Bytes  state p
| P_Cons   p -> print_P_Cons   state p
| P_Ctor   p -> print_P_Ctor   state p
| P_False  p -> print_P_False  state p
| P_Int    p -> print_P_Int    state p
| P_List   p -> print_P_List   state p
| P_Nat    p -> print_P_Nat    state p
| P_None   p -> print_P_None   state p
| P_Par    p -> print_P_Par    state p
| P_Record p -> print_P_Record state p
| P_Some   p -> print_P_Some   state p
| P_String p -> print_P_String state p
| P_True   p -> print_P_True   state p
| P_Tuple  p -> print_P_Tuple  state p
| P_Typed  p -> print_P_Typed  state p
| P_Unit   p -> print_P_Unit   state p
| P_Var    p -> print_P_Var    state p

(* Bytes as literals in patterns *)

and print_P_Bytes state = print_bytes state

(* A series of cons operators in patterns *)

and print_P_Cons state (node : (pattern, cons) nsepseq reg) =
  print_nsepseq state print_pattern "CONS" node.value

(* A constructor application (or constant constructor) in patterns *)

and print_P_Ctor state (node : (ctor * pattern option) reg) =
  let ctor, arg_opt = node.value in
  print_ctor   state ctor;
  print_option state print_pattern arg_opt

(* The Boolean untruth as a pattern *)

and print_P_False state = print_token state "False"

(* Integers in patterns *)

and print_P_Int state = print_int state

(* Patterns of lists by extension *)

and print_P_List state = print_injection state print_pattern

and print_injection :
      'a.state -> (state -> 'a -> unit) -> 'a injection reg -> unit =
  fun state print {value; _} ->
    print_option state print_open_compound value.compound;
    print_sepseq state print "SEMI" value.elements;
    print_option state print_semi value.terminator;
    print_option state print_close_compound value.compound

and print_semi state = print_token state "SEMI"

(* Natural numbers in patterns *)

and print_P_Nat state = print_nat state

(* The pattern for the predefined constructor [None] *)

and print_P_None state = print_token state "Ctor_None"

(* Parenthesised patterns *)

and print_P_Par state = print_par state print_pattern

(* Record patterns *)

and print_P_Record state =
  print_ne_injection state (swap print_field print_pattern)

and print_field :
      'rhs.state -> (state -> 'rhs -> unit) -> 'rhs field reg -> unit =
  fun state print (node : 'rhs field reg) ->
  match node.value with
    Punned field_name ->
    print_ident state field_name
  | Complete {field_name; assignment; field_rhs} ->
     print_ident   state field_name;
     print_token   state "EQ" assignment;
     print         state field_rhs

(* The pattern for the application of the predefined constructor
   [Some] *)

and print_P_Some state (node : (kwd_Some * pattern reg) reg) =
  let ctor_Some, patterns = node.value in
  print_token   state "Ctor_Some" ctor_Some;
  print_pattern state pattern

(* String literals as patterns *)

and print_P_String state = print_string state

(* The Boolean for truth in patterns *)

and print_P_True state = print_token state "True"

(* The pattern matching a tuple *)

and print_P_Tuple state (node : tuple_pattern) =
  let print state = print_nsepseq state print_pattern "COMMA"
  in print_par state print node

(* Typed pattern *)

and print_P_Typed state (node : typed_pattern reg) =
  let node = node.value in
  print_pattern    state node.pattern;
  print_type_annot state node.type_annot

(* The pattern matching the unique value of the type "unit". *)

and print_P_Unit state (node : the_unit reg) =
  let opening, closing = node.value in
  print_token state "LPAR";
  print_token state "RPAR"

(* A pattern variable *)

and print_P_Var state = print_ident state

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_E_Add] comes before [print_E_And]. *)

and print_expr state = function
  E_Add      e -> print_E_Add      state e
| E_Annot    e -> print_E_Annot    state e
| E_Bytes    e -> print_E_Bytes    state e
| E_Call     e -> print_E_Call     state e
| E_Cat      e -> print_E_Cat      state e
| E_CodeInj  e -> print_E_CodeInj  state e
| E_Cond     e -> print_E_Cond     state e
| E_Conj     e -> print_E_Conj     state e
| E_Cons     e -> print_E_Cons     state e
| E_Ctor     e -> print_E_Ctor     state e
| E_Disj     e -> print_E_Disj     state e
| E_Div      e -> print_E_Div      state e
| E_Equal    e -> print_E_Equal    state e
| E_False    e -> print_E_False    state e
| E_Fun      e -> print_E_Fun      state e
| E_Geq      e -> print_E_Geq      state e
| E_Gt       e -> print_E_Gt       state e
| E_Int      e -> print_E_Int      state e
| E_Leq      e -> print_E_Leq      state e
| E_LetIn    e -> print_E_LetIn    state e
| E_List     e -> print_E_List     state e
| E_Lt       e -> print_E_Lt       state e
| E_Match    e -> print_E_Match    state e
| E_Mod      e -> print_E_Mod      state e
| E_ModAlias e -> print_E_ModAlias state e
| E_ModIn    e -> print_E_ModIn    state e
| E_ModPath  e -> print_E_ModPath  state e
| E_Mult     e -> print_E_Mult     state e
| E_Mutez    e -> print_E_Mutez    state e
| E_Nat      e -> print_E_Nat      state e
| E_Neg      e -> print_E_Neg      state e
| E_Neq      e -> print_E_Neq      state e
| E_None     e -> print_E_None     state e
| E_Not      e -> print_E_Not      state e
| E_Or       e -> print_E_Or       state e
| E_Par      e -> print_E_Par      state e
| E_Proj     e -> print_E_Proj     state e
| E_Record   e -> print_E_Record   state e
| E_Seq      e -> print_E_Seq      state e
| E_Some     e -> print_E_Some     state e
| E_String   e -> print_E_String   state e
| E_Sub      e -> print_E_Sub      state e
| E_True     e -> print_E_True     state e
| E_Tuple    e -> print_E_Tuple    state e
| E_TypeIn   e -> print_E_TypeIn   state e
| E_Unit     e -> print_E_Unit     state e
| E_Update   e -> print_E_Update   state e
| E_Var      e -> print_E_Var      state e
| E_Verbatim e -> print_E_Verbatim state e

(*
| E_LetIn    e -> print_let_in      state e
| E_TypeIn   e -> print_type_in     state e
| E_ModIn    e -> print_mod_in      state e
| E_ModAlias e -> print_mod_alias   state e
| E_Tuple    e -> print_csv         state print_expr e
| E_Fun      e -> print_fun_expr    state e
| E_String   e -> print_string_expr state e
| E_Var      e -> print_var         state e
| E_Proj     e -> print_projection  state e
| E_ModA     e -> print_mod_access  state print_expr e
| E_Update   e -> print_update      state e
| E_Unit     e -> print_unit        state e
| E_Par      e -> print_par_expr    state e
| E_List     e -> print_list_expr   state e
| E_Seq      e -> print_sequence    state e
| E_Record   e -> print_record_expr state e
| E_Ctor     e -> print_ctor_expr   state e
 *)

(* Arithmetic addition *)

and print_E_Add state = print_op2 state "PLUS"

and print_op2 state lexeme (node : keyword bin_op reg) =
  let node = node.value in
  print_expr  state node.arg1;
  print_token state lexeme node.op;
  print_expr  state node.arg2

(* Expressions annotated with a type *)

and print_E_Annot state = print_par state print_annot_expr

and print_annot_expr state (expr, type_annot) =
  print_expr       state expr;
  print_type_annot state type_annot

(* Bytes as expressions *)

and print_E_Bytes state = print_bytes state

(* Function calls *)

and print_E_Call state (node : (expr * expr nseq) reg) =
  let func, args = node.value in
  print_expr state func;
  print_nseq state print_expr args

(* String catenation *)

and print_E_Cat state = print_op2 state "CARET"


(* Code Injection *)

(* NOTE: This printer is an exception to the rule. Indeed, we here
   deconstruct this token into its constituents and we print them as
   concrete syntax, instead of tokens. This is hopefully better for
   debugging tokens whose lexemes have been scanned with not trivial
   rules. *)

and print_E_CodeInj state (node : code_inj reg) =
  let {value; region} = node.value.language in
  let header_stop = region#start#shift_bytes 1 in
  let header_reg  = Region.make ~start:region#start ~stop:header_stop in
  print_token  state "[%" header_reg;
  print_string state value;
  print_expr   state node.value.code;
  print_token  state "]" node.value.rbracket

(* Conditionals *)

and print_E_Cond state (node : cond_expr reg) =
  let {kwd_if; test; kwd_then; ifso; ifnot} = node.value in
  print_token  state "If" kwd_if ;
  print_expr   state test;
  print_token  state "Then" kwd_then;
  print_expr   state ifso;
  print_option state print_else ifnot

and print_else state (kwd_else, ifnot) =
  print_token state "Else" kwd_else;
  print_expr  state ifnot

(* Boolean disjunction *)

and print_E_Conj state = print_op2 state "BOOL_AND"

(* Consing (that is, pushing an item on top of a stack/list *)

and print_E_Cons state = print_op2 state "CONS"

(* Constructor application (or constant constructor) as expressions *)

and print_E_Ctor state (node : (ctor * expr option) reg) =
  let ctor, arg_opt = node.value in
  print_ctor   state ctor;
  print_option state print_expr arg_opt

(* Boolean disjunction *)

and print_E_Disj state = print_op2 state "BOOL_OR"

(* The Euclidean quotient *)

and print_E_Div state = print_op2 state "SLASH"

(* Equality *)

and print_E_Equal state = print_op2 state "EQ"

(* The Boolean untruth *)

and print_E_False state = print_token state "False"

(* Functional expressions *)

and print_E_Fun state (node : fun_expr reg) =
  let {kwd_fun; binders; lhs_type; arrow; body} = node.value in
  print_token  state "Fun" kwd_fun;
  print_nseq   state print_pattern binders;
  print_option state print_type_annot lhs_type;
  print_token  state "ARROW" arrow;
  print_expr   state body

(* Greater or Equal *)

and print_E_Geq state = print_op2 state "Geq"

(* Greater Than *)

and print_E_Gt state = print_op2 state "Gt"

(* Integer literals as expressions *)

and print_E_Int state = print_int state

(* Lower or Equal *)

and print_E_Leq state = print_op2 state "Leq"

(* Local definitions (let-in) *)

and print_E_LetIn state (node : let_in reg) =
  let node = node.value in
  let {kwd_let; kwd_rec; binding; kwd_in; body; attributes} = node in
  print_attributes   state attributes;
  print_token        state "Let" kwd_let ;
  print_token_opt    state "Rec" kwd_rec;
  print_let_binding  state binding;
  print_token        state "In" kwd_in;
  print_expr         state body

(* Lists of expressions defined intensionally *)

and print_E_List state = print_injection state print_expr

(* Lower Than *)

and print_E_Lt state = print_op2 state "Lt"


(* Pattern matching *)

and print_E_Match state (node : match_expr reg) =
  let node = node.value in
  let {kwd_match; expr; kwd_with; lead_vbar; cases} = value in
  print_token     state "Match" kwd_match ;
  print_expr      state expr;
  print_token     state "With" kwd_with;
  print_token_opt state "VBAR" lead_vbar;
  print_cases     state cases

and print_cases state (node : (case_clause reg, vbar) nsepseq reg) =
  print_nsepseq state print_case_clause "VBAR" node.value

and print_case_clause state (node : case_clause reg) =
  let {pattern; arrow; rhs} = node.value in
  print_pattern state pattern;
  print_token   state "ARROW" arrow;
  print_expr    state rhs

(* Euclidean reminder ("modulo") *)

and print_E_Mod state = print_op2 state "Mod"



(* XXX *)



and print_projection state (node : projection reg) =
  print_var     state node.value.struct_name;
  print_token   state "." node.value.selector;
  print_nsepseq state print_selection "." node.value.field_path

and print_mod_access :
      'a.state -> (state -> 'a -> unit ) -> 'a module_access reg -> unit =
  fun state print node ->
  print_var   state node.value.module_name;
  print_token state "." node.value.selector;
  print       state node.value.field

and print_update state (node : update reg) =
  print_token        state "{" node.value.lbrace;
  print_path         state node.value.record;
  print_token        state "with" node.value.kwd_with;
  print_ne_injection state print_field_path_assignment node.value.updates;
  print_token        state "}" node.value.rbrace

and print_path state = function
    Name var  -> print_var        state var
  | Path path -> print_projection state path

and print_selection state = function
    FieldName id -> print_var state id
  | Component c  -> print_int state c

and print_type_annot state (colon, type_expr) =
  print_token     state ":" colon;
  print_type_expr state type_expr

and print_par : 'a.state -> (state -> 'a -> unit) -> 'a par -> unit =
  fun state print node ->
  print_token state "(" node.lpar;
  print       state node.inside;
  print_token state ")" node.rpar


and print_ctor_pattern state = function
    PNone    p -> print_none_pattern     state p
  | PSomeApp p -> print_psome_app state p
  | PFalse   p -> print_token            state "false" p
  | PTrue    p -> print_token            state "true" p
  | PCtorApp p -> print_pctor_app state p

and print_none_pattern state = print_token state "None"

and print_psome_app state (node : (kwd_Some * pattern) reg) =
  let ctor_Some, argument = node.value in
  print_token   state "Some" ctor_Some;
  print_pattern state argument

and print_pctor_app state (node : (ctor * pattern option) reg) =
  let ctor, param = node.value in
  print_ctor   state ctor;
  print_option state print_pattern param


and print_ctor_expr state = function
    ENone    e -> print_none_expr     state e
  | E_SomeApp e -> print_some_app_expr state e
  | E_CtorApp e -> print_ctor_app_expr state e

and print_none_expr state = print_token state "None"

and print_some_app_expr state (node : (kwd_Some * expr) reg) =
  let ctor_Some, argument = node.value in
  print_token state "Some" ctor_Some;
  print_expr  state argument

and print_par_expr state (node : expr par reg) =
  print_par state print_expr node.value

and print_unit state (node : the_unit reg) =
  let lpar, rpar = node.value in
  print_token state "(" lpar;
  print_token state ")" rpar

and print_list_expr state = function
    ECons   e -> print_op2 state "::" e
  | E_ListComp e -> if   e.value.elements = None
                   then print_token state "[]" e.region
                   else print_injection state print_expr e

and print_op1 state lexeme (node : keyword un_op reg) =
  print_token state lexeme node.value.op;
  print_expr  state node.value.arg

and print_arith_expr state = function
  | E_Add   e -> print_op2   state "+" e
  | E_Sub   e -> print_op2   state "-" e
  | E_Mult  e -> print_op2   state "*" e
  | E_Div   e -> print_op2   state "/" e
  | E_Mod   e -> print_op2   state "mod" e
  | E_Neg   e -> print_op1   state "-" e
  | E_Int   e -> print_int   state e
  | E_Nat   e -> print_nat   state e
  | E_Mutez e -> print_mutez state e

and print_string_expr state = function
  | String   e -> print_string   state e
  | Verbatim e -> print_verbatim state e

and print_bool_expr state = function
    Or    e -> print_op2   state "||"    e
  | And   e -> print_op2   state "&&"    e
  | Not   e -> print_op1   state "not"   e
  | True  e -> print_token state "true"  e
  | False e -> print_token state "false" e

and print_comp_expr state = function
    Lt    e -> print_op2 state "<"  e
  | Leq   e -> print_op2 state "<=" e
  | Gt    e -> print_op2 state ">"  e
  | Geq   e -> print_op2 state ">=" e
  | Neq   e -> print_op2 state "<>" e
  | Equal e -> print_op2 state "="  e

and print_record_expr state =
  print_ne_injection state print_field_assignment

and print_code_inj state (node : code_inj reg) =
  let {value; region} = node.value.language in
  let header_stop = region#start#shift_bytes 1 in
  let header_reg  = Region.make ~start:region#start ~stop:header_stop in
  print_token  state "[%" header_reg;
  print_string state value;
  print_expr   state node.value.code;
  print_token  state "]" node.value.rbracket

and print_field_assignment state (node : field_assignment reg) =
  print_var   state node.value.field_name;
  print_token state "EQ" node.value.assignment;
  print_expr  state node.value.field_expr

and print_field_path_assignment state {value; _} =
  let {field_path; assignment; field_expr} = value in
  print_path  state field_path;
  print_token state "EQ" assignment;
  print_expr  state field_expr

and print_sequence state seq =
  print_injection state print_expr seq

and print_type_in state {value; _} =
  let {type_decl; kwd_in; body} = value in
  let {kwd_type; name; eq; type_expr} = type_decl in
  print_token     state "type" kwd_type;
  print_var       state name;
  print_token     state "eq" eq;
  print_type_expr state type_expr;
  print_token     state "in" kwd_in;
  print_expr      state body

and print_mod_in state {value; _} =
  let {mod_decl; kwd_in; body} = value in
  let {kwd_module; name; eq; kwd_struct; structure; kwd_end} = mod_decl in
  print_token  state "module" kwd_module;
  print_var    state name;
  print_token  state "eq" eq;
  print_token  state "struct" kwd_struct;
  print_tokens state structure;
  print_token  state "end" kwd_end;
  print_token  state "in" kwd_in;
  print_expr   state body

and print_mod_alias state {value; _} =
  let {mod_alias; kwd_in; body} = value in
  let {kwd_module; alias; eq; mod_path} = mod_alias in
  print_token   state "module" kwd_module;
  print_var     state alias;
  print_token   state "eq" eq;
  print_nsepseq state print_var "." mod_path;
  print_token   state "in" kwd_in;
  print_expr    state body

(* Conversion to string *)

let to_string ~offsets ~mode print node =
  let buffer = Buffer.create 131 in
  let state = mk_state ~offsets ~mode ~buffer in
  let () = print state node
  in Buffer.contents buffer

let tokens_to_string = to_string print_tokens

let pattern_to_string = to_string print_pattern

let expr_to_string = to_string print_expr

let type_expr_to_string = to_string print_type_expr
