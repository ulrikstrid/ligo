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

type ('a, 'sep) nsepseq = ('a, 'sep) Utils.nsepseq

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
  state -> (state -> 'a -> unit) -> string -> ('a, region) Utils.nsepseq -> unit =
  fun state print sep (head, tail) ->
    let print_aux (sep_reg, item) =
      let sep_line =
        sprintf "%s: %s\n" (compact state sep_reg) sep in
      Buffer.add_string state#buffer sep_line;
      print state item
    in print state head; List.iter print_aux tail

let print_sepseq :
  state -> (state -> 'a -> unit) -> string -> ('a, region) Utils.sepseq -> unit =
  fun state print sep ->
    print_option state (fun state -> print_nsepseq state print sep)

let print_nseq : state -> (state -> 'a -> unit) -> 'a Utils.nseq -> unit =
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

(*
  T_Prod   t -> print_cartesian   state t
| T_Sum    t -> print_sum_type    state t
| T_Record t -> print_record_type state t
| T_Par    t -> print_type_par    state t
| TVar    t -> print_var         state t
| TWild   t -> print_token       state "_" t
| TString t -> print_string      state t
| TInt    t -> print_int         state t
| TModA   t -> print_mod_access  state print_type_expr t*)

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
  let print state = print_nsepseq state print_type_expr ","
  in print_par state print node.value

(* Functional types *)

and print_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  let domain, arrow, range = node.value in
  print_type_expr state domain;
  print_token     state "->" arrow;
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



and print_sum_type state (node : sum_type reg) =
  print_attributes state node.value.attributes;
  print_token_opt  state "|" node.value.lead_vbar;
  print_nsepseq    state print_variant "|" node.value.variants

and print_type_par state (node : type_expr par reg) =
  print_par state print_type_expr node.value

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

and print_of_type_expr state (kwd_of, t_expr) =
  print_token     state "of" kwd_of;
  print_type_expr state t_expr

and print_variant state (node : variant reg) =
  print_attributes state node.value.attributes;
  print_ctor       state node.value.ctor;
  print_option     state print_of_type_expr node.value.arg

and print_field_decl state (node : field_decl reg) =
  print_attributes state node.value.attributes;
  print_var        state node.value.field_name;
  print_token      state ":" node.value.colon;
  print_type_expr  state node.value.field_type

and print_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection reg -> unit =
  fun state print node ->
    print_option state print_open_compound node.value.compound;
    print_sepseq state print ";" node.value.elements;
    print_option state print_semi node.value.terminator;
    print_option state print_close_compound node.value.compound

and print_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection reg -> unit =
  fun state print node ->
    print_attributes state node.value.attributes;
    print_option     state print_open_compound node.value.compound;
    print_nsepseq    state print ";" node.value.ne_elements;
    print_option     state print_semi node.value.terminator;
    print_option     state print_close_compound node.value.compound

and print_record_type state =
  print_ne_injection state print_field_decl

and print_open_compound state = function
  BeginEnd (kwd_begin, _) -> print_token state "begin" kwd_begin
| Braces   (lbrace, _)    -> print_token state "{"     lbrace
| Brackets (lbracket, _)  -> print_token state "["     lbracket

and print_close_compound state = function
  BeginEnd (_, kwd_end)  -> print_token state "end" kwd_end
| Braces   (_, rbrace)   -> print_token state "}"   rbrace
| Brackets (_, rbracket) -> print_token state "]"   rbracket

and print_semi state = print_token state ";"

and print_type_annot state (colon, type_expr) =
  print_token     state ":" colon;
  print_type_expr state type_expr

and print_let_binding state (node : let_binding) =
  print_nseq   state print_pattern node.binders;
  print_option state print_type_annot node.lhs_type;
  print_token  state "=" node.eq;
  print_expr   state node.let_rhs

and print_par : 'a.state -> (state -> 'a -> unit) -> 'a par -> unit =
  fun state print node ->
    print_token state "(" node.lpar;
    print       state node.inside;
    print_token state ")" node.rpar

and print_pattern state = function
  PTuple    p -> print_csv            state print_pattern p
| PList     p -> print_list_pattern   state p
| PVar      p -> print_pvar           state p
| PInt      p -> print_int            state p
| PNat      p -> print_nat            state p
| PBytes    p -> print_bytes          state p
| PString   p -> print_string         state p
| PVerbatim p -> print_verbatim       state p
| PWild     p -> print_token          state "_" p
| PCtor     p -> print_ctor_pattern   state p
| PRecord   p -> print_record_pattern state p
| PTyped    p -> print_typed_pattern  state p
| PUnit     p -> print_unit           state p
| PPar      p -> print_par            state print_pattern p.value

and print_list_pattern state = function
  PListComp p -> print_injection state print_pattern p
| PCons     p -> print_raw       state p

and print_raw state (node : (pattern * cons * pattern) reg) =
  let head, cons, tail = node.value in
  print_pattern state head;
  print_token   state "::" cons;
  print_pattern state tail

and print_typed_pattern state (node : typed_pattern reg) =
  print_pattern   state node.value.pattern;
  print_token     state ":" node.value.colon;
  print_type_expr state node.value.type_expr

and print_record_pattern state =
  print_ne_injection state print_field_pattern

and print_field_pattern state (node : field_pattern reg) =
  print_var     state node.value.field_name;
  print_token   state "=" node.value.eq;
  print_pattern state node.value.pattern

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

and print_expr state = function
  ELetIn    e -> print_let_in      state e
| ETypeIn   e -> print_type_in     state e
| EModIn    e -> print_mod_in      state e
| EModAlias e -> print_mod_alias   state e
| ECond     e -> print_conditional state e
| ETuple    e -> print_csv         state print_expr e
| ECase     e -> print_match_expr  state e
| EFun      e -> print_fun_expr    state e
| EAnnot    e -> print_annot_expr  state e
| ELogic    e -> print_logic_expr  state e
| EArith    e -> print_arith_expr  state e
| EString   e -> print_string_expr state e
| ECall     e -> print_fun_call    state e
| EVar      e -> print_var         state e
| EProj     e -> print_projection  state e
| EModA     e -> print_mod_access  state print_expr e
| EUpdate   e -> print_update      state e
| EUnit     e -> print_unit        state e
| EBytes    e -> print_bytes       state e
| EPar      e -> print_par_expr    state e
| EList     e -> print_list_expr   state e
| ESeq      e -> print_sequence    state e
| ERecord   e -> print_record_expr state e
| ECtor     e -> print_ctor_expr   state e
| ECodeInj  e -> print_code_inj    state e

and print_ctor_expr state = function
  ENone    e -> print_none_expr     state e
| ESomeApp e -> print_some_app_expr state e
| ECtorApp e -> print_ctor_app_expr state e

and print_none_expr state = print_token state "None"

and print_some_app_expr state (node : (kwd_Some * expr) reg) =
  let ctor_Some, argument = node.value in
  print_token state "Some" ctor_Some;
  print_expr  state argument

and print_ctor_app_expr state (node : (ctor * expr option) reg) =
  let ctor, argument = node.value in
  print_ctor   state ctor;
  print_option state print_expr argument

and print_par_expr state (node : expr par reg) =
  print_par state print_expr node.value

and print_unit state (node : the_unit reg) =
  let lpar, rpar = node.value in
  print_token state "(" lpar;
  print_token state ")" rpar

and print_fun_call state (node : (expr * expr Utils.nseq) reg) =
  let func, args = node.value in
  print_expr state func;
  print_nseq state print_expr args

and print_annot_expr state (node : annot_expr par reg) =
  let print state (expr, colon, type_expr) =
    print_expr      state expr;
    print_token     state ":" colon;
    print_type_expr state type_expr
  in print_par state print node.value

and print_list_expr state = function
    ECons   e -> print_op2 state "::" e
| EListComp e -> if   e.value.elements = None
                then print_token state "[]" e.region
                else print_injection state print_expr e

and print_op1 state lexeme (node : keyword un_op reg) =
  print_token state lexeme node.value.op;
  print_expr  state node.value.arg

and print_op2 state lexeme (node : keyword bin_op reg) =
  print_expr  state node.value.arg1;
  print_token state lexeme node.value.op;
  print_expr  state node.value.arg2

and print_arith_expr state = function
  Add   e -> print_op2   state "+" e
| Sub   e -> print_op2   state "-" e
| Mult  e -> print_op2   state "*" e
| Div   e -> print_op2   state "/" e
| Mod   e -> print_op2   state "mod" e
| Neg   e -> print_op1   state "-" e
| Int   e -> print_int   state e
| Nat   e -> print_nat   state e
| Mutez e -> print_mutez state e

and print_string_expr state = function
  Cat      e -> print_op2      state "^" e
| String   e -> print_string   state e
| Verbatim e -> print_verbatim state e

and print_logic_expr state = function
  BoolExpr e -> print_bool_expr state e
| CompExpr e -> print_comp_expr state e

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
  print_token state "=" node.value.assignment;
  print_expr  state node.value.field_expr

and print_field_path_assignment state {value; _} =
  let {field_path; assignment; field_expr} = value in
  print_path  state field_path;
  print_token state "=" assignment;
  print_expr  state field_expr

and print_sequence state seq =
  print_injection state print_expr seq

and print_match_expr state {value; _} =
  let {kwd_match; expr; kwd_with; lead_vbar; cases} = value in
  print_token     state "match" kwd_match ;
  print_expr      state expr;
  print_token     state "with" kwd_with;
  print_token_opt state "|" lead_vbar;
  print_cases     state cases

and print_cases state {value; _} =
  print_nsepseq state print_case_clause "|" value

and print_case_clause state {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern state pattern;
  print_token   state "->" arrow;
  print_expr    state rhs

and print_let_in state {value; _} =
  let {kwd_let; kwd_rec; binding; kwd_in; body; attributes} = value in
  print_attributes   state attributes;
  print_token        state "let" kwd_let ;
  print_token_opt    state "rec" kwd_rec;
  print_let_binding  state binding;
  print_token        state "in" kwd_in;
  print_expr         state body

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

and print_fun_expr state {value; _} =
  let {kwd_fun; binders; lhs_type; arrow; body} = value in
  print_token  state "fun" kwd_fun;
  print_nseq   state print_pattern binders;
  print_option state print_type_annot lhs_type;
  print_token  state "->" arrow;
  print_expr   state body

and print_else state (kwd_else, ifnot) =
  print_token state "else" kwd_else;
  print_expr  state ifnot

and print_conditional state {value; _} =
  let {kwd_if; test; kwd_then; ifso; ifnot} = value in
  print_token  state "(" ghost;
  print_token  state "if" kwd_if ;
  print_expr   state test;
  print_token  state "then" kwd_then;
  print_expr   state ifso;
  print_option state print_else ifnot;
  print_token  state ")" ghost

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
