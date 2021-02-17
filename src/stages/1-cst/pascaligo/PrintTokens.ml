(* PRINTING THE TOKENS *)

(* This module produces tokens reconstructed from the Concrete Syntax
   Tree (CST) in the same way they should be produced by the lexer. In
   other words, the leaves of the CST are printed as tokens. This
   enables to test the transmission of the tokens from the lexer to
   the parser. *)

[@@@coverage exclude_file]

open CST

module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils
open! Region

let sprintf = Printf.sprintf

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
       meaning that the argument of the constructor is printed;

     * a generic name, like [print_token] for tokens which are
       keywords or symbols; another example is [print_token_opt] when
       we have an optional keyword. Or higher-order functions like
       [print_injection], [print_option], [print_nsepseq] etc. *)

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

(* Printing tokens like the lexer would do. *)

let print_token state token region =
  let line =
    sprintf "%s: %s\n" (compact state region) token
  in Buffer.add_string state#buffer line

let print_token_opt state lexeme =
  print_option state (fun state -> print_token state lexeme)

let print_T_String state {region; value} =
  let line =
    sprintf "%s: String %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_E_String = print_T_String
let print_P_String = print_T_String

let print_E_Verbatim state {region; value} =
  let line =
    sprintf "%s: Verbatim %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_E_Bytes state {region; value} =
  let lexeme, abstract = value in
  let line = sprintf "%s: Bytes (%S, \"0x%s\")\n"
                     (compact state region) lexeme
                     (Hex.show abstract)
  in Buffer.add_string state#buffer line

let print_P_Bytes = print_E_Bytes

let print_E_Int state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Int (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let print_P_Int     = print_E_Int
let print_T_Int     = print_E_Int
let print_Component = print_E_Int

let print_E_Nat state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Nat (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let print_P_Nat = print_E_Nat

let print_E_Mutez state {region; value=lex,z} =
  let line =
    sprintf "Mutez %s (%s)" lex (Z.to_string z)
  in print_token state line region

let print_variable state {region; value} =
  let line =
    sprintf "%s: Ident %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_UIdent state {region; value} =
  let line =
    sprintf "%s: UIdent %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_module_name = print_UIdent
let print_ctor = print_UIdent

let print_attribute state {region; value} =
  let line =
    sprintf "%s: Attr %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_language state {region; value} =
  let line =
    sprintf "%s: Lang %S" (compact state region)
            value.Region.value
  in Buffer.add_string state#buffer line

(* HIGHER-ORDER PRINTERS *)

let print_par : 'a.state -> (state -> 'a -> unit) -> 'a par -> unit =
  fun state print par ->
    print_token state "LPAR" par.lpar;
    print       state par.inside;
    print_token state "RPAR" par.rpar

let print_braces : 'a.state -> (state -> 'a -> unit) -> 'a braces -> unit =
  fun state print braces ->
    print_token state "LBRACE" braces.lbrace;
    print       state braces.inside;
    print_token state "RBRACE" braces.rbrace

let print_brackets : 'a.state -> (state -> 'a -> unit) -> 'a brackets -> unit =
  fun state print brackets ->
    print_token state "LBRACKET" brackets.lbracket;
    print       state brackets.inside;
    print_token state "LBRACKET" brackets.rbracket

(* PRINTING THE CST *)

let rec print_cst state (node : cst) =
  print_nseq  state print_declaration node.decl;
  print_token state "EOF" node.eof

(* Declarations *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_declaration state = function
  D_Const    d -> print_const_decl   state d.value
| D_Fun      d -> print_fun_decl     state d.value
| D_Module   d -> print_module_decl  state d.value
| D_ModAlias d -> print_module_alias state d.value
| D_Type     d -> print_type_decl    state d.value

and print_const_decl state (node : const_decl) =
  print_attributes state node.attributes;
  print_token      state "Const" node.kwd_const;
  print_variable   state node.name;
  print_option     state print_type_annot node.const_type;
  print_token      state "EQ" node.equal;
  print_expr       state node.init;
  print_token_opt  state "SEMI" node.terminator

and print_type_decl state (node : type_decl) =
  print_token     state "Type" node.kwd_type;
  print_variable  state node.name;
  print_token     state "Is" node.kwd_is;
  print_type_expr state node.type_expr;
  print_token_opt state "SEMI" node.terminator

and print_module_decl state (node : module_decl) =
  match node.enclosing with
    Brace (lbrace, rbrace) ->
      print_token     state "Module" node.kwd_module;
      print_variable  state node.name;
      print_token     state "Is" node.kwd_is;
      print_token     state "LBRACE" lbrace;
      print_nseq      state print_declaration node.declarations;
      print_token     state  "RBRACE" rbrace;
      print_token_opt state "SEMI" node.terminator
  | BeginEnd (kwd_begin, kwd_end) ->
      print_token     state "Module" node.kwd_module;
      print_variable  state node.name;
      print_token     state "Is" node.kwd_is;
      print_token     state "Begin" kwd_begin;
      print_nseq      state print_declaration node.declarations;
      print_token     state "End" kwd_end;
      print_token_opt state "SEMI" node.terminator

and print_module_alias state (node : module_alias) =
  print_token     state "Module" node.kwd_module;
  print_variable  state node.alias;
  print_token     state "Is" node.kwd_is;
  print_nsepseq   state print_variable "DOT" node.mod_path;
  print_token_opt state "SEMI" node.terminator

(* Type expressions *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_type_expr state = function
  T_Ctor    t -> print_T_Ctor       state t
| T_Fun     t -> print_T_Fun        state t
| T_Int     t -> print_T_Int        state t
| T_ModPath t -> print_module_path  state print_type_expr t.value
| T_Par     t -> print_par          state print_type_expr t.value
| T_Prod    t -> print_cartesian    state t
| T_Record  t -> print_ne_injection state (strip print_field_decl) t.value
| T_String  t -> print_T_String     state t
| T_Sum     t -> print_sum_type     state t.value
| T_Var     t -> print_variable     state t
| T_Wild    t -> print_token        state "WILD" t

and print_type_annot state (colon, type_expr) =
  print_token     state "COLON" colon;
  print_type_expr state type_expr;

and print_cartesian state (node : cartesian) =
  print_nsepseq state print_type_expr "TIMES" node.value

and print_of_type_expr state (kwd_of, type_expr) =
  print_token     state "Of" kwd_of;
  print_type_expr state type_expr

and print_variant state (node : variant) =
  print_attributes state node.attributes;
  print_ctor       state node.ctor;
  print_option     state print_of_type_expr node.arg

and print_sum_type state (node : sum_type) =
  print_attributes state node.attributes;
  print_token_opt  state "VBAR" node.lead_vbar;
  print_nsepseq    state (strip print_variant) "VBAR" node.variants

and print_T_Ctor state (node : (type_ctor * type_tuple) reg) =
  let type_name, type_tuple = node.value in
  print_variable   state type_name;
  print_type_tuple state type_tuple

and print_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  let domain, arrow, codomain = node.value in
  print_type_expr state domain;
  print_token     state "ARROW" arrow;
  print_type_expr state codomain

and print_field_decl state (node : field_decl) =
  print_attributes state node.attributes;
  print_variable   state node.field_name;
  print_token      state "COLON" node.colon;
  print_type_expr  state node.field_type

and print_type_tuple state (node : type_tuple) =
  let print state = print_nsepseq state print_type_expr "COMMA"
  in print_par state print node.value

and print_fun_decl state (node : fun_decl) =
  print_attributes state node.attributes;
  print_token      state "Function" node.kwd_function;
  print_variable   state node.fun_name;
  print_parameters state node.param;
  print_option     state print_type_annot node.ret_type;
  print_token      state "Is" node.kwd_is;
  print_expr       state node.return;
  print_token_opt  state "SEMI" node.terminator;

and print_fun_expr state (node : fun_expr) =
  print_token      state "Function" node.kwd_function;
  print_parameters state node.param;
  print_option     state print_type_annot node.ret_type;
  print_token      state "Is" node.kwd_is;
  print_expr       state node.return

and print_block_with state (node : block_with) =
  print_block state node.block;
  print_token state "With" node.kwd_with;
  print_expr  state node.expr;

and print_parameters state (node : parameters) =
  let print state = print_nsepseq state print_param_decl "SEMI"
  in print_par state print node.value

and print_param_decl state = function
  ParamConst p -> print_param_const state p
| ParamVar   p -> print_param_var   state p

and print_param_const state (node : param_const reg) =
  print_token    state "Const" node.value.kwd_const;
  print_variable state node.value.var;
  print_option   state print_type_annot node.value.param_type

and print_param_var state (node : param_var reg) =
  print_token    state "Var" node.value.kwd_var;
  print_variable state node.value.var;
  print_option   state print_type_annot node.value.param_type

(* Blocks *)

and print_block state (node : block reg) =
  match node.value.enclosing with
    Block (kwd_block, lbrace, rbrace) ->
      print_token      state "Block" kwd_block;
      print_token      state "LBRACE" lbrace;
      print_statements state node.value.statements;
      print_token_opt  state "SEMI" node.value.terminator;
      print_token      state "RBRACE" rbrace
  | BeginEnd (kwd_begin, kwd_end) ->
      print_token      state "Begin" kwd_begin;
      print_statements state node.value.statements;
      print_token_opt  state "SEMI" node.value.terminator;
      print_token      state "End" kwd_end

and print_var_decl state (node : var_decl) =
  print_token     state "Var" node.kwd_var;
  print_variable  state node.name;
  print_option    state print_type_annot node.var_type;
  print_token     state "ASSIGN" node.assign;
  print_expr      state node.init;
  print_token_opt state "SEMI" node.terminator

and print_attributes state = List.iter (print_attribute state)

and print_statements state =
  print_nsepseq state print_statement "SEMI"

and print_statement state = function
  S_Instr   s -> print_instruction state s
| S_Decl    s -> print_declaration state s
| S_VarDecl s -> print_var_decl    state s.value

(* Instructions *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_instruction state = function
  I_Assign      i -> print_assignment   state i.value
| I_Call        i -> print_fun_call     state i
| I_Case        i -> print_case         state print_test_clause i.value
| I_Cond        i -> print_conditional  state print_test_clause i.value
| I_For         i -> print_for_int      state i.value
| I_ForIn       i -> print_for_in         state i.value
| I_MapPatch    i -> print_map_patch    state i.value
| I_MapRemove   i -> print_map_remove   state i.value
| I_RecordPatch i -> print_record_patch state i.value
| I_Skip        i -> print_token        state "Skip" i
| I_SetPatch    i -> print_set_patch    state i.value
| I_SetRemove   i -> print_set_remove   state i.value
| I_While       i -> print_while_loop   state i.value

and print_case : 'a.state -> (state -> 'a -> unit) -> 'a case -> unit =
  fun state print node ->
    print_token state "Case" node.kwd_case;
    print_expr  state node.expr;
    print_token state "Of" node.kwd_of;
    match node.enclosing with
      Brackets (lbracket, rbracket) ->
        print_token     state "LBRACKET" lbracket;
        print_token_opt state "VBAR" node.lead_vbar;
        print_cases     state print node.cases.value;
        print_token     state "RBRACKET" rbracket
    | End kwd_end ->
        print_token_opt state "VBAR" node.lead_vbar;
        print_cases     state print node.cases.value;
        print_token     state "End" kwd_end

and print_cases :
  'a.state -> (state -> 'a -> unit) ->
  ('a case_clause reg, vbar) Utils.nsepseq -> unit =
    fun state print node ->
      let print state node = print_case_clause state print node.value
      in print_nsepseq state print "VBAR" node

and print_case_clause :
  'a.state -> (state -> 'a -> unit) -> 'a case_clause -> unit =
  fun state print node ->
    print_pattern state node.pattern;
    print_token   state "ARROW" node.arrow;
    print         state node.rhs

and print_test_clause state = function
  ClauseInstr instr -> print_instruction  state instr
| ClauseBlock block -> print_clause_block state block

and print_clause_block state = function
  LongBlock block ->
    print_block state block
| ShortBlock block ->
    let print state (stmts, terminator) =
      print_statements state stmts;
      print_token_opt  state "SEMI" terminator
    in print_braces state print block.value

and print_assignment state (node : assignment) =
  print_lhs   state node.lhs;
  print_token state "ASSIGN" node.assign;
  print_expr  state node.rhs

and print_lhs state = function
  Path    path -> print_path       state path
| MapPath path -> print_map_lookup state path.value

and print_while_loop state (node : while_loop) =
  print_token state "While" node.kwd_while;
  print_expr  state node.cond;
  print_block state node.block

and print_step state (kwd_step, expr) =
  print_token state "Step" kwd_step;
  print_expr  state expr

and print_for_int state (node : for_int) =
  print_token    state "For" node.kwd_for;
  print_variable state node.binder;
  print_token    state "ASSIGN" node.assign;
  print_expr     state node.init;
  print_token    state "To" node.kwd_to;
  print_expr     state node.bound;
  print_option   state print_step node.step;
  print_block    state node.block

and print_for_in state (node : for_in) =
  print_token      state "For" node.kwd_for;
  print_variable   state node.var;
  print_option     state print_bind_to node.bind_to;
  print_token      state "In" node.kwd_in;
  print_collection state node.collection;
  print_expr       state node.expr;
  print_block      state node.block

and print_collection state = function
  `List kwd -> print_token state "List" kwd
| `Map  kwd -> print_token state "Map"  kwd
| `Set  kwd -> print_token state "Set"  kwd

and print_bind_to state (arrow, variable) =
  print_token state "ARROW" arrow;
  print_variable state variable

(* Expressions *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_expr state = function
  E_Add       e -> print_bin_op      state "PLUS" e.value
| E_And       e -> print_bin_op      state "And" e.value
| E_Annot     e -> print_par         state print_annot_expr e.value
| E_BigMap    e -> print_injection   state (strip print_binding) e.value
| E_Block     e -> print_block_with  state e.value
| E_Bytes     e -> print_E_Bytes     state e
| E_Call      e -> print_fun_call    state e
| E_Case      e -> print_case        state print_expr e.value
| E_Cat       e -> print_bin_op      state "CARET" e.value
| E_CodeInj   e -> print_language    state e.value.language
| E_Equal     e -> print_bin_op      state "EQ" e.value
| E_Cond      e -> print_conditional state print_expr e.value
| E_Cons      e -> print_bin_op      state "SHARP" e.value
| E_Ctor      e -> print_E_Ctor      state e
| E_Div       e -> print_bin_op      state "SLASH" e.value
| E_False     e -> print_token       state "False" e
| E_Fun       e -> print_fun_expr    state e.value
| E_Geq       e -> print_bin_op      state "Geq" e.value
| E_Gt        e -> print_bin_op      state "Gt" e.value
| E_Int       e -> print_E_Int       state e
| E_Leq       e -> print_bin_op      state "Leq" e.value
| E_List      e -> print_injection   state print_expr e.value
| E_Lt        e -> print_bin_op      state "Lt" e.value
| E_Map       e -> print_injection   state (strip print_binding) e.value
| E_MapLookUp e -> print_map_lookup  state e.value
| E_Mod       e -> print_bin_op      state "Mod" e.value
| E_ModPath   e -> print_module_path state print_expr e.value
| E_Mult      e -> print_bin_op      state "TIMES" e.value
| E_Mutez     e -> print_E_Mutez     state e
| E_Nat       e -> print_E_Nat       state e
| E_Neg       e -> print_un_op       state "MINUS" e.value
| E_Nil       e -> print_token       state "Nil" e
| E_Neq       e -> print_bin_op      state "NE" e.value
| E_None      e -> print_token       state "Ctor_None" e
| E_Not       e -> print_un_op       state "Not" e.value
| E_Or        e -> print_bin_op      state "Or" e.value
| E_Par       e -> print_par         state print_expr e.value
| E_Proj      e -> print_projection  state e.value
| E_Record    e -> print_record      state e.value
| E_Set       e -> print_injection   state print_expr e.value
| E_SetMem    e -> print_set_mem     state e.value
| E_Some      e -> print_E_Some      state e
| E_String    e -> print_E_String    state e
| E_Sub       e -> print_bin_op      state "MINUS" e.value
| E_True      e -> print_token       state "True" e
| E_Tuple     e -> print_tuple_expr  state e
| E_Unit      e -> print_token       state "Unit" e
| E_Update    e -> print_update      state e.value
| E_Var       e -> print_variable    state e
| E_Verbatim  e -> print_E_Verbatim  state e

and print_conditional :
  'a.state -> (state -> 'a -> unit) -> 'a conditional -> unit =
  fun state print node ->
    print_token     state "If" node.kwd_if;
    print_expr      state node.test;
    print_token     state "Then" node.kwd_then;
    print           state node.ifso;
    print_token_opt state "SEMI" node.terminator;
    print_token     state "Else" node.kwd_else;
    print           state node.ifnot

and print_annot_expr state (expr, type_annot) =
  print_expr       state expr;
  print_type_annot state type_annot

and print_set_mem state (node : set_mem) =
  print_expr  state node.set;
  print_token state "Contains" node.kwd_contains;
  print_expr  state node.element

and print_map_lookup state (node : map_lookup) =
  print_path     state node.path;
  print_brackets state print_expr node.index.value

and print_path state = function
  Name var  -> print_variable   state var
| Path path -> print_projection state path.value

and print_un_op state lexeme (node : keyword un_op) =
  print_token state lexeme node.op;
  print_expr  state node.arg

and print_bin_op state lexeme (node : keyword bin_op) =
  print_expr  state node.arg1;
  print_token state lexeme node.op;
  print_expr  state node.arg2

and print_record state =
  print_ne_injection state (strip print_field_assignment)

and print_field_assignment state (node : field_assignment) =
  print_variable state node.field_name;
  print_token    state "EQ" node.assignment;
  print_expr     state node.field_expr

and print_field_path_assignment state (node : field_path_assignment) =
  print_path  state node.field_path;
  print_token state "EQ" node.assignment;
  print_expr  state node.field_expr

and print_update state (node : update) =
  print_path         state node.record;
  print_token        state "With" node.kwd_with;
  print_ne_injection state (strip print_field_path_assignment)
                     node.updates.value

and print_projection state (node : projection) =
  print_variable state node.struct_name;
  print_token    state "DOT" node.selector;
  print_nsepseq  state print_selection "DOT" node.field_path

and print_module_path :
  'a.state -> (state -> 'a -> unit ) -> 'a module_path -> unit =
  fun state print node ->
    print_module_name state node.module_name;
    print_token       state "DOT" node.selector;
    print             state node.field

and print_selection state = function
  FieldName name -> print_variable  state name
| Component int  -> print_Component state int

and print_record_patch state (node : record_patch) =
  print_token        state "Patch" node.kwd_patch;
  print_path         state node.path;
  print_token        state "With" node.kwd_with;
  print_ne_injection state (strip print_field_assignment)
                           node.record_inj.value

and print_set_patch state (node : set_patch) =
  print_token        state "Patch" node.kwd_patch;
  print_path         state node.path;
  print_token        state "With" node.kwd_with;
  print_ne_injection state print_expr node.set_inj.value

and print_map_patch state (node : map_patch) =
  print_token        state "Patch" node.kwd_patch;
  print_path         state node.path;
  print_token        state "With" node.kwd_with;
  print_ne_injection state (strip print_binding) node.map_inj.value

and print_map_remove state (node : map_remove) =
  print_token state "Remove" node.kwd_remove;
  print_expr  state node.key;
  print_token state "From" node.kwd_from;
  print_token state "Map" node.kwd_map;
  print_path  state node.map

and print_set_remove state (node : set_remove) =
  print_token state "Remove" node.kwd_remove;
  print_expr  state node.element;
  print_token state "From" node.kwd_from;
  print_token state "Set" node.kwd_set;
  print_path  state node.set

and print_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection -> unit =
  fun state print node ->
    print_injection_kwd state node.kind;
    match node.enclosing with
      Brackets (lbracket, rbracket) ->
        print_token     state "LBRACKET" lbracket;
        print_sepseq    state print "SEMI" node.elements;
        print_token_opt state "SEMI" node.terminator;
        print_token     state "RBRACKET" rbracket
    | End kwd_end ->
        print_sepseq    state print "SEMI" node.elements;
        print_token_opt state "SEMI" node.terminator;
        print_token     state "End" kwd_end

and print_injection_kwd state = function
  `BigMap kwd -> print_token state "BigMap" kwd
| `List   kwd -> print_token state "List"   kwd
| `Map    kwd -> print_token state "Map"    kwd
| `Set    kwd -> print_token state "Set"    kwd

and print_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection -> unit =
  fun state print node ->
    print_attributes       state node.attributes;
    print_ne_injection_kwd state node.kind;
    match node.enclosing with
      Brackets (lbracket, rbracket) ->
        print_token     state "LBRACKET" lbracket;
        print_nsepseq   state print "SEMI" node.ne_elements;
        print_token_opt state "SEMI" node.terminator;
        print_token     state "RBRACKET" rbracket
    | End kwd_end ->
        print_nsepseq   state print "SEMI" node.ne_elements;
        print_token_opt state "SEMI" node.terminator;
        print_token     state "End" kwd_end

and print_ne_injection_kwd state = function
  `Map    kwd -> print_token state "Map"    kwd
| `Record kwd -> print_token state "Record" kwd
| `Set    kwd -> print_token state "Set"    kwd

and print_binding state (node : binding) =
  print_expr  state node.source;
  print_token state "ARROW" node.arrow;
  print_expr  state node.image

and print_tuple_expr state (node : tuple_expr) =
  let print state = print_nsepseq state print_expr "COMMA"
  in print_par state print node.value

and print_fun_call state (node : fun_call) =
  let expr, arguments = node.value in
  print_expr       state expr;
  print_tuple_expr state arguments

and print_E_Ctor state (node : (ctor * arguments option) reg) =
  let ctor, arguments = node.value in
  print_ctor   state ctor;
  print_option state print_tuple_expr arguments

and print_E_Some state (node : (kwd_Some * arguments) reg) =
  let ctor_Some, arguments = node.value in
  print_token      state "Ctor_Some" ctor_Some;
  print_tuple_expr state arguments

(* Patterns *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_pattern state = function
  P_Bytes    p -> print_P_Bytes       state p
| P_Cons     p -> print_nsepseq       state print_pattern "SHARP" p.value
| P_Ctor     p -> print_P_Ctor        state p
| P_False    p -> print_token         state "False" p
| P_Int      p -> print_P_Int         state p
| P_List     p -> print_injection     state print_pattern p.value
| P_Nat      p -> print_P_Nat         state p
| P_Nil      p -> print_token         state "Nil" p
| P_None     p -> print_token         state "Ctor_None" p
| P_ParCons  p -> print_P_ParCons     state p
| P_Some     p -> print_P_Some        state p
| P_String   p -> print_P_String      state p
| P_True     p -> print_token         state "True" p
| P_Tuple    p -> print_tuple_pattern state p
| P_Unit     p -> print_token         state "Unit" p
| P_Var      p -> print_variable      state p
| P_Wild     p -> print_token         state "WILD" p

and print_P_Ctor state (node : (ctor * tuple_pattern option) reg) =
  let ctor, arg_opt = node.value in
  print_ctor   state ctor;
  print_option state print_tuple_pattern arg_opt

and print_P_Some state (node : (kwd_Some * pattern par reg) reg) =
  let ctor_Some, patterns = node.value in
  print_token state "Ctor_Some" ctor_Some;
  print_par   state print_pattern patterns.value

and print_P_ParCons state (node : (pattern * cons * pattern) par reg) =
  let print state (head, cons, tail) =
    print_pattern state head;
    print_token   state "SHARP" cons;
    print_pattern state tail;
  in print_par state print node.value

and print_tuple_pattern state (node : tuple_pattern) =
  let print state = print_nsepseq state print_pattern "COMMA"
  in print_par state print node.value

(* Printing tokens (client-slide) *)

type ('src, 'dst) printer = state -> 'src -> 'dst

let print_to_buffer state cst = print_cst state cst; state#buffer
let print_to_string state cst = print_to_buffer state cst |> Buffer.contents

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
