[@@@coverage exclude_file]

open CST

module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils
open! Region

let sprintf = Printf.sprintf

(* STATE *)

(* Both the printing of the tokens and of the CST itself make use of
   the same threaded data structure: the state. Some parts are used by
   the former, other by the latter. For example, the fields for
   padding ([pad_path] and [pad_node]) are only used when printing the
   CST, not the tokens, which are simply a list. In both cases, the
   printing is done to the field [buffer], which is a string buffer,
   which is imperatively updated (see module [Stdlib.Buffer].) *)

type state = <
  offsets  : bool;
  mode     : [`Point | `Byte];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

let mk_state ~offsets ~mode ~buffer =
  object
    method offsets  = offsets;
    method mode     = mode;
    method buffer   = buffer
    val pad_path    = ""
    method pad_path = pad_path
    val pad_node    = ""
    method pad_node = pad_node

    (* The method [pad] updates the current padding, which is
       comprised of two components: the padding to reach the new node
       (space before reaching a subtree, then a vertical bar for it)
       and the padding for the new node itself. (Is it the last child
       of its parent?)
     *)
    method pad arity rank =
      {< pad_path =
           pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
         pad_node =
           pad_node ^ (if rank = arity-1 then "    " else "|   ")
      >}
  end

let compact state (region: Region.t) =
  region#compact ~offsets:state#offsets state#mode

(* PRINTING THE TOKENS *)

(* Note: The name of the printing functions is prefixed by
   "print_". The rest of the name is either

     * the name of the type whose value is printed, for example
       [print_declaration],

     * the name of a token, for example, [print_

 *)

(* Some higher-order printers for optional and special lists *)

let print_option : state -> (state -> 'a -> unit) -> 'a option -> unit =
  fun state print -> function
         None -> ()
  | Some node -> print state node

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

let strip : 'a.(state -> 'a -> unit) -> state -> 'a reg -> unit =
  fun print state node -> print state node.value

(* Printing tokens *)

let print_token state lexeme region =
  let line =
    sprintf "%s: %s\n" (compact state region) lexeme
  in Buffer.add_string state#buffer line

let print_token_opt state lexeme =
  print_option state (fun state -> print_token state lexeme)

let print_String state {region; value} =
  let line =
    sprintf "%s: String %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_Verbatim state {region; value} =
  let line =
    sprintf "%s: Verbatim %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_Bytes state {region; value} =
  let lexeme, abstract = value in
  let line = sprintf "%s: Bytes (%S, \"0x%s\")\n"
                     (compact state region) lexeme
                     (Hex.show abstract)
  in Buffer.add_string state#buffer line

let print_Int state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Int (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let print_Nat state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Nat (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let print_Mutez state {region; value=lex,z} =
  let line =
    sprintf "Mutez %s (%s)" lex (Z.to_string z)
  in print_token state line region

let print_Ident state {region; value} =
  let line =
    sprintf "%s: Ident %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_UIdent state {region; value} =
  let line =
    sprintf "%s: UIdent %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_Attr state {region; value} =
  let line =
    sprintf "%s: Attr %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_Lang state {region; value} =
  let line =
    sprintf "%s: Lang %S" (compact state region)
            value.Region.value
  in Buffer.add_string state#buffer line

(* Printing the tokens from the CST *)

let rec print_tokens state (node : cst) =
  print_nseq  state print_declaration node.decl;
  print_token state "EOF" node.eof

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
  print_Ident      state node.name;
  print_option     state print_type_annot node.const_type;
  print_token      state "EQ" node.equal;
  print_expr       state node.init;
  print_token_opt  state "SEMI" node.terminator

and print_type_decl state (node : type_decl) =
  print_token     state "Type" node.kwd_type;
  print_Ident     state node.name;
  print_token     state "Is" node.kwd_is;
  print_type_expr state node.type_expr;
  print_token_opt state "SEMI" node.terminator

and print_module_decl state (node : module_decl) =
  match node.enclosing with
    Brace (lbrace, rbrace) ->
      print_token     state "Module" node.kwd_module;
      print_Ident     state node.name;
      print_token     state "Is" node.kwd_is;
      print_token     state "LBRACE" lbrace;
      print_nseq      state print_declaration node.declarations;
      print_token     state  "RBRACE" rbrace;
      print_token_opt state "SEMI" node.terminator
  | BeginEnd (kwd_begin, kwd_end) ->
      print_token     state "Module" node.kwd_module;
      print_Ident     state node.name;
      print_token     state "Is" node.kwd_is;
      print_token     state "Begin" kwd_begin;
      print_nseq      state print_declaration node.declarations;
      print_token     state "End" kwd_end;
      print_token_opt state "SEMI" node.terminator

and print_module_alias state (node : module_alias) =
  print_token     state "Module" node.kwd_module;
  print_Ident     state node.alias;
  print_token     state "Is" node.kwd_is;
  print_nsepseq   state print_Ident "DOT" node.mod_path;
  print_token_opt state "SEMI" node.terminator

and print_par : 'a.state -> (state -> 'a -> unit) -> 'a par -> unit =
  fun state print par ->
    print_token state "LPAR" par.lpar;
    print       state par.inside;
    print_token state "RPAR" par.rpar

and print_braces : 'a.state -> (state -> 'a -> unit) -> 'a braces -> unit =
  fun state print braces ->
    print_token state "LBRACE" braces.lbrace;
    print       state braces.inside;
    print_token state "RBRACE" braces.rbrace

and print_brackets : 'a.state -> (state -> 'a -> unit) -> 'a brackets -> unit =
  fun state print brackets ->
    print_token state "LBRACKET" brackets.lbracket;
    print       state brackets.inside;
    print_token state "LBRACKET" brackets.rbracket

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

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_type_expr state = function
  T_Ctor    t -> print_T_Ctor      state t.value
| T_Fun     t -> print_T_Fun       state t.value
| T_Int     t -> print_Int         state t
| T_ModPath t -> print_module_path state print_type_expr t.value
| T_Par     t -> print_T_Par       state t.value
| T_Prod    t -> print_cartesian   state t
| T_Record  t -> print_T_Record    state t.value
| T_String  t -> print_String      state t
| T_Sum     t -> print_sum_type    state t.value
| T_Var     t -> print_Ident       state t
| T_Wild    t -> print_wild        state t

and print_wild state = print_token state "WILD"

and print_type_annot state (colon, type_expr) =
  print_token     state "COLON" colon;
  print_type_expr state type_expr;

and print_cartesian state (node : cartesian) =
  print_nsepseq state print_type_expr "TIMES" node.value

and print_of_type_expr state (kwd_of, t_expr) =
  print_token     state "Of" kwd_of;
  print_type_expr state t_expr

and print_variant state (node: variant) =
  print_attributes state node.attributes;
  print_UIdent     state node.ctor;
  print_option     state print_of_type_expr node.arg

and print_sum_type state (node : sum_type) =
  print_attributes state node.attributes;
  print_token_opt  state "VBAR" node.lead_vbar;
  print_nsepseq    state (strip print_variant) "VBAR" node.variants

and print_T_Record state =
  print_ne_injection state (strip print_field_decl)

and print_T_Ctor state (node : type_ctor * type_tuple) =
  let type_name, type_tuple = node in
  print_Ident      state type_name;
  print_type_tuple state type_tuple

and print_T_Fun state (node : type_expr * arrow * type_expr) =
  let type_expr_a, arrow, type_expr_b = node in
  print_type_expr state type_expr_a;
  print_token     state "ARROW" arrow;
  print_type_expr state type_expr_b

and print_T_Par state = print_par state print_type_expr

and print_field_decl state (node : field_decl) =
  print_attributes state node.attributes;
  print_Ident      state node.field_name;
  print_token      state "COLON" node.colon;
  print_type_expr  state node.field_type

and print_type_tuple state (node : type_tuple) =
  let print state = print_nsepseq state print_type_expr "COMMA"
  in print_par state print node.value

and print_fun_decl state (node : fun_decl) =
  print_attributes state node.attributes;
  print_token      state "Function" node.kwd_function;
  print_Ident      state node.fun_name;
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
  print_token  state "Const" node.value.kwd_const;
  print_Ident  state node.value.var;
  print_option state print_type_annot node.value.param_type

and print_param_var state (node : param_var reg) =
  print_token  state "Var" node.value.kwd_var;
  print_Ident  state node.value.var;
  print_option state print_type_annot node.value.param_type

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

and print_var_decl state (node : var_decl reg) =
  print_token     state "Var" node.value.kwd_var;
  print_Ident     state node.value.name;
  print_option    state print_type_annot node.value.var_type;
  print_token     state "ASSIGN" node.value.assign;
  print_expr      state node.value.init;
  print_token_opt state "SEMI" node.value.terminator

and print_attributes state (node : attribute list) =
  List.iter (print_Attr state) node

and print_statements state =
  print_nsepseq state print_statement "SEMI"

and print_statement state = function
  S_Instr   s -> print_instruction state s
| S_Decl    s -> print_declaration state s
| S_VarDecl s -> print_var_decl    state s

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_instruction state = function
  I_Assign      i -> print_assignment   state i.value
| I_Call        i -> print_fun_call     state i
| I_Case        i -> print_case         state print_test_clause i.value
| I_Cond        i -> print_conditional  state print_test_clause i.value
| I_For         i -> print_for_int      state i
| I_Iter        i -> print_iter         state i
| I_MapPatch    i -> print_map_patch    state i.value
| I_MapRemove   i -> print_map_remove   state i.value
| I_RecordPatch i -> print_record_patch state i.value
| I_Skip        i -> print_token        state "Skip" i
| I_SetPatch    i -> print_set_patch    state i.value
| I_SetRemove   i -> print_set_remove   state i.value
| I_While       i -> print_while_loop   state i.value

and print_test_clause state = function
  ClauseInstr instr -> print_instruction  state instr
| ClauseBlock block -> print_clause_block state block

and print_clause_block state = function
  LongBlock block ->
    print_block state block
| ShortBlock block ->
    let print state (stmts, terminator) =
      print_statements state stmts;
      print_token_opt state "SEMI" terminator
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

and print_for_int state (node : for_int reg) =
  print_token  state "For" node.value.kwd_for;
  print_Ident  state node.value.binder;
  print_token  state "ASSIGN" node.value.assign;
  print_expr   state node.value.init;
  print_token  state "To" node.value.kwd_to;
  print_expr   state node.value.bound;
  print_option state print_step node.value.step;
  print_block  state node.value.block

and print_iter state (node : iter reg) =
  print_token      state "For" node.value.kwd_for;
  print_Ident      state node.value.var;
  print_option     state print_bind_to node.value.bind_to;
  print_token      state "In" node.value.kwd_in;
  print_collection state node.value.collection;
  print_expr       state node.value.expr;
  print_block      state node.value.block

and print_collection state = function
  `List kwd -> print_token state "List" kwd
| `Map  kwd -> print_token state "Map"  kwd
| `Set  kwd -> print_token state "Set"  kwd

and print_bind_to state (arrow, variable) =
  print_token state "ARROW" arrow;
  print_Ident state variable

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_expr state = function
  E_Add       e -> print_bin_op      state "PLUS" e.value
| E_And       e -> print_bin_op      state "And" e.value
| E_Annot     e -> print_par         state print_annot_expr e.value
| E_BigMap    e -> print_injection   state (strip print_binding) e.value
| E_Block     e -> print_block_with  state e.value
| E_Bytes     e -> print_Bytes       state e
| E_Call      e -> print_fun_call    state e
| E_Case      e -> print_case        state print_expr e.value
| E_Cat       e -> print_bin_op      state "CARET" e.value
| E_CodeInj   e -> print_Lang        state e.value.language
| E_Equal     e -> print_bin_op      state "EQ" e.value
| E_Cond      e -> print_conditional state print_expr e.value
| E_Cons      e -> print_bin_op      state "SHARP" e.value
| E_Ctor      e -> print_E_Ctor      state e
| E_Div       e -> print_bin_op      state "SLASH" e.value
| E_False     e -> print_token       state "False" e
| E_Fun       e -> print_fun_expr    state e.value
| E_Geq       e -> print_bin_op      state "Geq" e.value
| E_Gt        e -> print_bin_op      state "Gt" e.value
| E_Int       e -> print_Int         state e
| E_Leq       e -> print_bin_op      state "Leq" e.value
| E_List      e -> print_injection   state print_expr e.value
| E_Lt        e -> print_bin_op      state "Lt" e.value
| E_Map       e -> print_injection   state (strip print_binding) e.value
| E_MapLookUp e -> print_map_lookup  state e.value
| E_Mod       e -> print_bin_op      state "Mod" e.value
| E_ModPath   e -> print_module_path state print_expr e.value
| E_Mult      e -> print_bin_op      state "TIMES"   e.value
| E_Mutez     e -> print_Mutez       state e
| E_Nat       e -> print_Nat         state e
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
| E_String    e -> print_String      state e
| E_Sub       e -> print_bin_op      state "MINUS" e.value
| E_True      e -> print_token       state "True" e
| E_Tuple     e -> print_tuple_expr  state e
| E_Unit      e -> print_token       state "Unit" e
| E_Update    e -> print_update      state e.value
| E_Var       e -> print_Ident       state e
| E_Verbatim  e -> print_Verbatim    state e

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
  print_expr state expr;
  print_type_annot state type_annot

and print_set_mem state (node : set_mem) =
  print_expr  state node.set;
  print_token state "Contains" node.kwd_contains;
  print_expr  state node.element

and print_map_lookup state (node : map_lookup) =
  print_path     state node.path;
  print_brackets state print_expr node.index.value

and print_path state = function
  Name var  -> print_Ident   state var
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
  print_Ident state node.field_name;
  print_token state "=" node.assignment;
  print_expr  state node.field_expr

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
  print_Ident   state node.struct_name;
  print_token   state "DOT" node.selector;
  print_nsepseq state print_selection "DOT" node.field_path

and print_module_path :
  'a.state -> (state -> 'a -> unit ) -> 'a module_path -> unit =
  fun state print node ->
    print_Ident state node.module_name;
    print_token state "DOT" node.selector;
    print       state node.field

and print_selection state = function
  FieldName name -> print_Ident state name
| Component int  -> print_Int state int

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
| `List   kwd -> print_token state "List"    kwd
| `Map    kwd -> print_token state "Map"     kwd
| `Set    kwd -> print_token state "Set"     kwd

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
  print_UIdent state ctor;
  print_option state print_tuple_expr arguments

and print_E_Some state (node : (kwd_Some * arguments) reg) =
  let ctor_Some, arguments = node.value in
  print_token      state "Ctor_Some" ctor_Some;
  print_tuple_expr state arguments

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_pattern state = function
  P_Bytes    p -> print_Bytes         state p
| P_Cons     p -> print_nsepseq       state print_pattern "SHARP" p.value
| P_Ctor     p -> print_P_Ctor        state p
| P_False    p -> print_token         state "False" p
| P_Int      p -> print_Int           state p
| P_List     p -> print_injection     state print_pattern p.value
| P_Nat      p -> print_Nat           state p
| P_Nil      p -> print_token         state "Nil" p
| P_None     p -> print_token         state "Ctor_None"  p
| P_ParCons  p -> print_P_ParCons     state p
| P_Some     p -> print_P_Some        state p
| P_String   p -> print_String        state p
| P_True     p -> print_token         state "True"  p
| P_Tuple    p -> print_tuple_pattern state p
| P_Unit     p -> print_token         state "Unit"  p
| P_Var      p -> print_Ident         state p
| P_Wild     p -> print_token         state "WILD" p

and print_P_Ctor state (node : (ctor * tuple_pattern option) reg) =
  let ctor, arg_opt = node.value in
  print_UIdent state ctor;
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

(* CONVERSIONS TO STRING *)

let to_string ~offsets ~mode print node =
  let buffer = Buffer.create 131 in
  let state = mk_state ~offsets ~mode ~buffer in
  let () = print state node
  in Buffer.contents buffer

let tokens_to_string = to_string print_tokens

let path_to_string = to_string print_path

let pattern_to_string = to_string print_pattern

let instruction_to_string = to_string print_instruction

let type_expr_to_string = to_string print_type_expr

(* PRINTING THE CST *)

let pp_ident state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s%s (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let pp_node state name =
  let node = sprintf "%s%s\n" state#pad_path name
  in Buffer.add_string state#buffer node

let pp_string state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s%S (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let pp_verbatim state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s{|%s|} (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let pp_loc_node state name region =
  pp_ident state {value=name; region}

let rec pp_cst state (node : cst) =
  let apply len rank =
    pp_declaration (state#pad len rank) in
  let decls = Utils.nseq_to_list node.decl in
  pp_node state "<cst>";
  List.iteri (List.length decls |> apply) decls

and pp_declaration state = function
  D_Const {value; region} ->
    pp_loc_node   state "D_Const" region;
    pp_const_decl state value
| D_Fun {value; region} ->
    pp_loc_node state "D_Fun" region;
    pp_fun_decl state value
| D_Module {value; region} ->
    pp_loc_node    state "D_Module" region;
    pp_module_decl state value
| D_ModAlias {value; region} ->
    pp_loc_node     state "D_ModAlias" region;
    pp_module_alias state value
| D_Type {value; region} ->
    pp_loc_node  state "D_Type" region;
    pp_type_decl state value

and pp_type_decl state (decl : type_decl) =
  let () = pp_ident (state#pad 2 0) decl.name in
  let () =
    let state = state#pad 2 1 in
    pp_node      state "<type_expr>";
    pp_type_expr (state#pad 1 0) decl.type_expr
  in ()

and pp_module_decl state (decl : module_decl) =
  let ()    = pp_ident (state#pad 2 0) decl.name in
  let state = state#pad 2 1 in
  let ()    = pp_node state "<structure>" in
  let apply len rank =
    pp_declaration (state#pad len rank) in
  let decls = Utils.nseq_to_list decl.declarations in
  List.iteri (List.length decls |> apply) decls

and pp_module_alias state (node : module_alias) =
  let () = pp_ident (state#pad 2 0) node.alias in
  let () =
    let state          = state#pad 2 1 in
    let mod_path       = Utils.nsepseq_to_list node.mod_path in
    let len            = List.length mod_path in
    let apply len rank = pp_ident (state#pad len rank) in
    pp_node state "<path>";
    List.iteri (apply len) mod_path
  in ()

and pp_fun_decl state (node : fun_decl) =
  let arity = if node.kwd_recursive = None then 0 else 1 in
  let arity = if node.ret_type = None then arity else arity+1 in
  let arity = if node.attributes = [] then arity else arity+1 in
  let arity = arity + 3
  and rank  = 0 in
  let rank  = if node.kwd_recursive = None then rank
              else let state = state#pad arity rank in
                   pp_node state "recursive";
                   rank+1 in
  let rank =
    let state = state#pad arity rank in
    pp_ident state node.fun_name;
    rank + 1 in
  let rank =
    let state = state#pad arity rank in
    pp_node state "<parameters>";
    pp_parameters state node.param;
    rank + 1 in
  let rank =
    match node.ret_type with
      None -> rank
    | Some (_, t_expr) ->
       let state = state#pad arity rank in
       pp_node state "<return type>";
       pp_type_expr (state#pad 1 0) t_expr;
       rank+1 in
  let rank =
    let state = state#pad arity rank in
    pp_node state "<return>";
    pp_expr (state#pad 1 0) node.return;
    rank+1 in
  let () =
    let attr = node.attributes in
    if attr <> [] then
      let state = state#pad arity rank in
      pp_node state "<attributes>";
      let length         = List.length attr in
      let apply len rank = pp_ident (state#pad len rank)
      in List.iteri (apply length) attr
  in ()

and pp_const_decl state (node : const_decl) =
  let arity = if node.const_type = None then 0 else 1 in
  let arity = if node.attributes = [] then arity else arity+1 in
  let arity = arity + 2 in
  let rank = 0 in
  let rank =
    pp_ident (state#pad arity 0) node.name; rank+1 in
  let rank =
    pp_type_annot (state#pad arity rank) rank node.const_type in
  let rank =
    pp_expr (state#pad arity rank) node.init; rank+1 in
  let rank =
    let attr = node.attributes in
    if attr <> [] then
      let state = state#pad arity rank in
      pp_node state "<attributes>";
      let length         = List.length attr in
      let apply len rank = pp_ident (state#pad len rank)
      in List.iteri (apply length) attr; rank+1
    else rank
  in ignore rank

and pp_type_expr state = function
  T_Ctor e -> pp_T_Ctor state e
| T_Fun e -> pp_T_Fun state e
| T_Int s ->
    pp_node state "T_Int";
    pp_int  (state#pad 1 0) s
| T_ModPath {value; region} ->
    pp_loc_node    state "T_ModPath" region;
    pp_module_path state pp_type_expr value
| T_Par {value; region} ->
    pp_loc_node  state "T_Par" region;
    pp_type_expr (state#pad 1 0) value.inside
| T_Prod cartesian ->
    pp_loc_node  state "T_Prod" cartesian.region;
    pp_cartesian state cartesian
| T_Record {value; region} ->
    pp_loc_node     state "T_Record" region;
    pp_ne_injection state pp_field_decl value
| T_String s ->
    pp_node   state "T_String";
    pp_string (state#pad 1 0) s
| T_Sum {value; region} ->
    pp_loc_node state "T_Sum" region;
    pp_sum_type state value
| T_Var v ->
    pp_node  state "T_Var";
    pp_ident (state#pad 1 0) v
| T_Wild wild ->
    pp_node     state "T_Wild";
    pp_loc_node state "T_Wild" wild

and pp_T_Ctor state (node : (type_ctor * type_tuple) reg) =
  let {value=(name, tuple); region} = node in
  pp_loc_node   state "T_Ctor" region;
  pp_ident      (state#pad 1 0) name;
  pp_type_tuple (state#pad 2 1) tuple

and pp_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  pp_loc_node state "T_Fun" node.region;
  let apply len rank = pp_type_expr (state#pad len rank)
  and domain, _, range = node.value in
  List.iteri (apply 2) [domain; range]

and pp_sum_type state (node : sum_type) =
  let variants = Utils.nsepseq_to_list node.variants in
  let arity    = List.length variants in
  let arity    = if node.attributes = [] then arity else arity+1 in
  let apply arity rank variant =
    let state = state#pad arity rank in
    pp_variant state variant.value in
  let () = List.iteri (apply arity) variants in
  if node.attributes <> [] then
    let state = state#pad arity (arity-1)
    in pp_attributes state node.attributes

and pp_cartesian state (node : cartesian) =
  let apply len rank = pp_type_expr (state#pad len rank) in
  let components = Utils.nsepseq_to_list node.value
  in List.iteri (List.length components |> apply) components

and pp_attributes state attributes =
  pp_node state "<attributes>";
  let length         = List.length attributes in
  let apply len rank = pp_ident (state#pad len rank)
  in List.iteri (apply length) attributes

and pp_variant state (node : variant) =
  let arity = if node.attributes = [] then 0 else 1 in
  let arity = if node.arg = None then arity else arity + 1 in
  let rank  = 0 in
  let ()    = pp_ident state node.ctor in
  let rank =
    match node.arg with
      None -> rank
    | Some (_, c) ->
        pp_type_expr (state#pad arity rank) c; rank+1 in
  if node.attributes <> [] then
    pp_attributes (state#pad arity rank) node.attributes

and pp_field_decl state (node : field_decl reg) =
  let arity = if node.value.attributes = [] then 1 else 2 in
  pp_ident     state node.value.field_name;
  pp_type_expr (state#pad arity 0) node.value.field_type;
  if node.value.attributes <> [] then
    pp_attributes (state#pad arity 1) node.value.attributes

and pp_type_tuple state
                  (node : (type_expr, comma) Utils.nsepseq par reg) =
  let components = Utils.nsepseq_to_list node.value.inside in
  let apply len rank = pp_type_expr (state#pad len rank)
  in List.iteri (List.length components |> apply) components

and pp_fun_expr state (expr: fun_expr) =
  let arity = if expr.ret_type = None then 2 else 3 in
  let rank = 0 in
  let rank =
    let state = state#pad arity rank in
    pp_node       state "<parameters>";
    pp_parameters state expr.param;
    rank + 1 in
  let rank =
    match expr.ret_type with
      None -> rank
    | Some (_, t_expr) ->
        let state = state#pad arity rank in
        pp_node      state "<return type>";
        pp_type_expr (state#pad 1 0) t_expr;
        rank + 1 in
  let () =
    let state = state#pad arity rank in
    pp_node state "<return>";
    pp_expr (state#pad 1 0) expr.return
  in ()

and pp_code_inj state (node : code_inj) =
  let () =
    let state = state#pad 2 0 in
    pp_node   state "<language>";
    pp_string (state#pad 1 0) node.language.value in
  let () =
    let state = state#pad 2 1 in
    pp_node state "<code>";
    pp_expr (state#pad 1 0) node.code
  in ()

and pp_block_with state (node : block_with) =
  let () =
    let state = state#pad 2 0 in
    pp_node       state "<block>";
    pp_statements state node.block.value.statements in
  let () =
    let state = state#pad 2 1 in
    pp_node state "<expr>";
    pp_expr (state#pad 1 0) node.expr
  in ()

and pp_parameters state
                  (node : (param_decl, semi) Utils.nsepseq par reg) =
  let params = Utils.nsepseq_to_list node.value.inside in
  let arity  = List.length params in
  let apply len rank = pp_param_decl (state#pad len rank)
  in List.iteri (apply arity) params

and pp_param_decl state = function
  ParamConst {value; region} ->
    let arity = if value.param_type = None then 1 else 2 in
    pp_loc_node state "ParamConst" region;
    pp_ident    (state#pad arity 0) value.var;
    ignore (pp_type_annot (state#pad arity 1) 1 value.param_type)
| ParamVar {value; region} ->
    let arity = if value.param_type = None then 1 else 2 in
    pp_loc_node state "ParamVar" region;
    pp_ident    (state#pad 2 0) value.var;
    ignore (pp_type_annot (state#pad arity 1) 1 value.param_type)

and pp_statements state statements =
  let statements     = Utils.nsepseq_to_list statements in
  let length         = List.length statements in
  let apply len rank = pp_statement (state#pad len rank)
  in List.iteri (apply length) statements

and pp_statement state = function
  S_Instr instr ->
    pp_node        state "S_Instr";
    pp_instruction (state#pad 1 0) instr
| S_Decl decl ->
    pp_node state "S_Decl";
    pp_declaration (state#pad 1 0) decl
| S_VarDecl decl ->
    pp_node state "S_VarDecl";
    pp_var_decl (state#pad 1 0) decl.value

and pp_instruction state = function
  I_Cond {value; region} ->
    pp_loc_node   state "I_Cond" region;
    pp_cond_instr state value
| I_Case {value; region} ->
    pp_loc_node state "I_Case" region;
    pp_case     state pp_test_clause value
| I_Assign {value; region} ->
    pp_loc_node   state "I_Assign" region;
    pp_assignment state value
| I_While {value; _} ->
    pp_node state "<while>";
    let () =
      let state = state#pad 2 0 in
      pp_node state "<condition>";
      pp_expr (state#pad 1 0) value.cond in
    let () =
      let state = state#pad 2 1 in
      let statements = value.block.value.statements in
      pp_node state "<statements>";
      pp_statements state statements
    in ()
| I_For {value; region} ->
    pp_loc_node state "I_For" region;
    pp_for_int state value
| I_Iter {value; region} ->
    pp_loc_node state "I_Iter" region;
    pp_iter state value
| I_Call {value; region} ->
    pp_loc_node state "I_Call" region;
    pp_fun_call state value
| I_Skip region ->
    pp_loc_node state "I_Skip" region
| I_RecordPatch {value; region} ->
    pp_loc_node     state "I_RecordPatch" region;
    pp_record_patch state value
| I_MapPatch {value; region} ->
    pp_loc_node  state "I_MapPatch" region;
    pp_map_patch state value
| I_SetPatch {value; region} ->
    pp_loc_node  state "I_SetPatch" region;
    pp_set_patch state value
| I_MapRemove {value; region} ->
    pp_loc_node   state "I_MapRemove" region;
    pp_map_remove state value
| I_SetRemove {value; region} ->
    pp_loc_node   state "I_SetRemove" region;
    pp_set_remove state value

and pp_cond_expr state (cond : expr conditional) =
  let () =
    let state = state#pad 3 0 in
    pp_node state "<condition>";
    pp_expr (state#pad 1 0) cond.test in
  let () =
    let state = state#pad 3 1 in
    pp_node state "<true>";
    pp_expr (state#pad 1 0) cond.ifso in
  let () =
    let state = state#pad 3 2 in
    pp_node state "<false>";
    pp_expr (state#pad 1 0) cond.ifnot
  in ()

and pp_cond_instr state (cond: test_clause conditional) =
  let () =
    let state = state#pad 3 0 in
    pp_node state "<condition>";
    pp_expr (state#pad 1 0) cond.test in
  let () =
    let state = state#pad 3 1 in
    pp_node        state "<true>";
    pp_test_clause (state#pad 1 0) cond.ifso in
  let () =
    let state = state#pad 3 2 in
    pp_node        state "<false>";
    pp_test_clause (state#pad 1 0) cond.ifnot
  in ()

and pp_test_clause state = function
  ClauseInstr instr ->
    pp_node        state "ClauseInstr";
    pp_instruction (state#pad 1 0) instr
| ClauseBlock block ->
    pp_node         state "ClauseBlock";
    pp_clause_block (state#pad 1 0) block

and pp_clause_block state = function
  LongBlock {value; region} ->
    pp_loc_node   state "LongBlock" region;
    pp_statements state value.statements
| ShortBlock {value; region} ->
    pp_loc_node   state "ShortBlock" region;
    pp_statements state (fst value.inside)

and pp_case :
  'a.state -> (state -> 'a -> unit) -> 'a case -> unit =
  fun state print case ->
    let clauses = Utils.nsepseq_to_list case.cases.value in
    let clauses = List.map (fun {value; _} -> value) clauses in
    let length  = List.length clauses + 1 in
    let apply len rank =
      let state = state#pad len (rank+1)
      in pp_case_clause state print
    in pp_expr (state#pad length 0) case.expr;
    List.iteri (apply length) clauses

and pp_case_clause :
  'a.state -> (state -> 'a -> unit) -> 'a case_clause -> unit =
  fun state print clause ->
    pp_node state "<clause>";
    pp_pattern (state#pad 2 0) clause.pattern;
    print (state#pad 2 1) clause.rhs

and pp_pattern state = function
  P_Wild region ->
    pp_loc_node state "P_Wild" region
| P_None region ->
    pp_loc_node state "P_None" region
| P_Some {value=_,{value=par; _}; region} ->
    pp_loc_node state "P_Some" region;
    pp_pattern (state#pad 1 0) par.inside
| P_Unit region ->
    pp_loc_node state "P_Unit" region
| P_False region ->
    pp_loc_node state "P_False" region
| P_True region ->
    pp_loc_node state "P_True" region
| P_Ctor {value; region} ->
    pp_loc_node state "P_Ctor" region;
    pp_ctor_app_pattern (state#pad 1 0) value
| P_Var v ->
    pp_node state "P_Var";
    pp_ident (state#pad 1 0) v
| P_Int n ->
    pp_node state "P_Int";
    pp_int state n
| P_Nat n ->
    pp_node state "P_Nat";
    pp_int state n
| P_Bytes b ->
    pp_node state "P_Bytes";
    pp_bytes state b
| P_String s ->
    pp_node state "P_String";
    pp_ident (state#pad 1 0) s
| P_List {value; region} ->
    pp_loc_node state "P_List" region;
    pp_injection (state#pad 1 0) pp_pattern value
| P_Nil region ->
    pp_loc_node state "P_Nil" region
| P_ParCons {value; region} ->
    pp_loc_node state "P_ParCons" region;
    pp_bin_cons (state#pad 1 0) value.inside
| P_Cons {value; region} ->
    let patterns = Utils.nsepseq_to_list value in
    let length   = List.length patterns in
    let apply len rank =
      pp_pattern (state#pad len rank) in
    pp_loc_node state "P_Cons" region;
    List.iteri (apply length) patterns
| P_Tuple {value; region} ->
    pp_loc_node state "P_Tuple" region;
    pp_tuple_pattern (state#pad 1 0) value

and pp_bytes state {value=lexeme,hex; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Hex.show hex)

and pp_int state {value=lexeme,z; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Z.to_string z)

and pp_ctor_app_pattern state (ctor, pat_opt) =
  pp_ident state ctor;
  match pat_opt with
      None -> ()
  | Some p -> pp_tuple_pattern state p.value

and pp_bin_cons state (head, _, tail) =
  pp_pattern (state#pad 2 0) head;
  pp_pattern (state#pad 2 1) tail

and pp_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection -> unit =
  fun state print inj ->
    let elements       = Utils.sepseq_to_list inj.elements in
    let length         = List.length elements in
    let apply len rank = print (state#pad len rank)
    in List.iteri (apply length) elements

and pp_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection -> unit =
  fun state print inj ->
    let ne_elements    = Utils.nsepseq_to_list inj.ne_elements in
    let length         = List.length ne_elements in
    let arity          = if inj.attributes = [] then length else length + 1
    and apply len rank = print (state#pad len rank)
    in List.iteri (apply arity) ne_elements;
       if inj.attributes <> [] then
         let state = state#pad arity (arity-1)
         in pp_attributes state inj.attributes

and pp_tuple_pattern state (node :  (pattern, comma) Utils.nsepseq par) =
  let patterns       = Utils.nsepseq_to_list node.inside in
  let length         = List.length patterns in
  let apply len rank = pp_pattern (state#pad len rank)
  in List.iteri (apply length) patterns

and pp_assignment state (node : assignment) =
  pp_lhs  (state#pad 2 0) node.lhs;
  pp_expr (state#pad 2 1) node.rhs

and pp_lhs state = function
  Path path ->
    pp_node state "Path";
    pp_path (state#pad 1 0) path
| MapPath {value; region} ->
    pp_loc_node   state "MapPath" region;
    pp_map_lookup state value

and pp_path state = function
  Name name ->
    pp_node  state "Name";
    pp_ident (state#pad 1 0) name
| Path {value; region} ->
    pp_loc_node   state "Path" region;
    pp_projection state value

and pp_projection state proj =
  let selections     = Utils.nsepseq_to_list proj.field_path in
  let len            = List.length selections in
  let apply len rank = pp_selection (state#pad len rank) in
  pp_ident (state#pad (1+len) 0) proj.struct_name;
  List.iteri (apply len) selections

and pp_module_path :
  'a.state -> (state -> 'a -> unit ) -> 'a module_path -> unit =
  fun state print node ->
    pp_ident (state#pad 2 0) node.module_name;
    print    (state#pad 2 1) node.field

and pp_update state update =
  pp_path         (state#pad 2 0) update.record;
  pp_ne_injection state pp_field_path_assignment update.updates.value

and pp_selection state = function
  FieldName name ->
    pp_node state "FieldName";
    pp_ident (state#pad 1 0) name
| Component comp ->
    pp_node state "Component";
    pp_int state comp

and pp_map_lookup state lookup =
  pp_path (state#pad 2 0) lookup.path;
  pp_expr (state#pad 2 1) lookup.index.value.inside

and pp_for_int state (node : for_int) =
  let arity =
    match node.step with None -> 3 | Some _ -> 4 in
  let () =
    let state = state#pad arity 0 in
    pp_node  state "<init>";
    pp_ident (state#pad 2 0) node.binder;
    pp_expr  (state#pad 2 1) node.init
    in
  let () =
    let state = state#pad arity 1 in
    pp_node state "<bound>";
    pp_expr (state#pad 1 0) node.bound in
  let () =
    match node.step with
      None -> ()
    | Some (_, expr) ->
        let state = state#pad arity 2 in
        pp_node state "<step>";
        pp_expr (state#pad 1 0) expr in
  let () =
    let state = state#pad arity (arity-1) in
    let statements = node.block.value.statements in
    pp_node state "<statements>";
    pp_statements state statements
  in ()

and pp_iter state (node : iter) =
  let () =
    let state = state#pad 3 0 in
    match node.bind_to with
      None ->
        pp_ident state node.var
    | Some (_, var) ->
        pp_var_binding state (node.var, var) in
  let () =
    let state = state#pad 3 1 in
    pp_node state "<collection>";
    pp_collection (state#pad 2 0) node.collection;
    pp_expr (state#pad 1 0) node.expr in
  let () =
    let state = state#pad 3 2 in
    let statements = node.block.value.statements in
    pp_node state "<statements>";
    pp_statements state statements
  in ()

and pp_collection state = function
  `Map  kwd -> pp_loc_node state "map"  kwd
| `Set  kwd -> pp_loc_node state "set"  kwd
| `List kwd -> pp_loc_node state "list" kwd

and pp_var_binding state (source, image) =
  pp_node  state "<binding>";
  pp_ident (state#pad 2 0) source;
  pp_ident (state#pad 2 1) image

and pp_fun_call state (expr, args) =
  let args           = Utils.nsepseq_to_list args.value.inside in
  let arity          = List.length args in
  let apply len rank = pp_expr (state#pad len rank)
  in pp_expr (state#pad (1+arity) 0) expr;
     List.iteri (apply arity) args

and pp_record_patch state (node : record_patch) =
  pp_path         (state#pad 2 0) node.path;
  pp_ne_injection state pp_field_assignment node.record_inj.value

and pp_field_assignment state (node : field_assignment reg) =
  pp_node  state "<field assignment>";
  pp_ident (state#pad 2 0) node.value.field_name;
  pp_expr  (state#pad 2 1) node.value.field_expr

and pp_field_path_assignment state (node : field_path_assignment reg) =
  pp_node state "<update>";
  pp_path (state#pad 2 0) node.value.field_path;
  pp_expr (state#pad 2 1) node.value.field_expr

and pp_map_patch state (node : map_patch) =
  pp_path         (state#pad 2 0) node.path;
  pp_ne_injection state pp_binding node.map_inj.value

and pp_binding state (node : binding reg) =
  pp_node state "<binding>";
  pp_expr (state#pad 2 0) node.value.source;
  pp_expr (state#pad 2 1) node.value.image

and pp_set_patch state (node : set_patch) =
  pp_path         (state#pad 2 0) node.path;
  pp_ne_injection state pp_expr node.set_inj.value

and pp_map_remove state (node : map_remove) =
  pp_expr (state#pad 2 0) node.key;
  pp_path (state#pad 2 1) node.map

and pp_set_remove state (node : set_remove) =
  pp_expr (state#pad 2 0) node.element;
  pp_path (state#pad 2 1) node.set

and pp_var_decl state (node : var_decl) =
  let arity = if node.var_type = None then 2 else 3
  and rank  = 0 in
  let rank  = pp_ident (state#pad arity rank) node.name; rank+1 in
  let rank  = pp_type_annot (state#pad arity rank) rank node.var_type
  in pp_expr (state#pad arity rank) node.init

and pp_expr state = function
  E_Case  e -> pp_expr_case state e
| E_Cond  e -> pp_expr_conditional state e
| E_Annot e -> pp_annot_expr state e
| E_Or    e -> pp_op2      state "E_Or"    e
| E_And   e -> pp_op2      state "E_And"   e
| E_Not   e -> pp_op1      state "E_Not"   e
| E_Lt    e -> pp_op2 state "E_Lt"    e
| E_Leq   e -> pp_op2 state "E_Leq"   e
| E_Gt    e -> pp_op2 state "E_Gt"    e
| E_Geq   e -> pp_op2 state "E_Geq"   e
| E_Equal e -> pp_op2 state "E_Equal" e
| E_Neq   e -> pp_op2 state "E_Neq"   e
| E_Add e   -> pp_op2 state "E_Add" e
| E_Sub e   -> pp_op2 state "E_Sub" e
| E_Mult e  -> pp_op2 state "E_Mult" e
| E_Div e   -> pp_op2 state "E_Div" e
| E_Mod e   -> pp_op2 state "E_Mod" e
| E_Neg e   -> pp_op1 state "E_Neg" e
| E_Cat e      -> pp_op2 state "E_Cat" e
| E_String e   -> pp_node   state "E_String";
                 pp_string (state#pad 1 0) e
| E_Verbatim e -> pp_node     state "E_Verbatim";
                 pp_verbatim (state#pad 1 0) e
| E_Cons     e -> pp_op2       state "E_Cons" e
| E_List e -> pp_list_comp state e
| E_Nil      e -> pp_loc_node  state "E_Nil" e
| E_None e -> pp_loc_node state "E_None" e
| E_Some e -> pp_some_app state e
| E_Ctor e -> pp_ctor_app state e
| E_Record e -> pp_record state e
| E_Proj e -> pp_proj_expr state e
| E_ModPath e -> let {value; region} = e in
    pp_loc_node      state "E_ModPath" region;
    pp_module_path state pp_expr value
| E_Update {value; region} ->
    pp_loc_node state "E_Update" region;
    pp_update   state value
| E_Set e -> pp_set_injection state e
| E_SetMem e -> pp_set_mem state e
| E_MapLookUp {value; region} ->
    pp_loc_node   state "E_MapLookUp" region;
    pp_map_lookup state value
| E_Map {value; region} ->
    pp_loc_node  state "E_Map" region;
    pp_injection state pp_binding value
| E_BigMap {value; region} ->
    pp_loc_node  state "E_BigMap" region;
    pp_injection state pp_binding value
| E_Var v ->
    pp_node  state "E_Var";
    pp_ident (state#pad 1 0) v
| E_Call {value; region} ->
    pp_loc_node state "E_Call" region;
    pp_fun_call state value
| E_Bytes b ->
    pp_node state "E_Bytes";
    pp_bytes state b
| E_Unit region ->
    pp_loc_node state "E_Unit" region
| E_Tuple e_tuple ->
    pp_node       state "E_Tuple";
    pp_tuple_expr state e_tuple
| E_Par {value; region} ->
    pp_loc_node state "E_Par" region;
    pp_expr     (state#pad 1 0) value.inside
| E_Fun {value; region} ->
    pp_loc_node state "E_Fun" region;
    pp_fun_expr state value;
| E_CodeInj {value; region} ->
    pp_loc_node state "E_CodeInj" region;
    pp_code_inj state value;
| E_Block {value; region} ->
    pp_loc_node   state "E_Block" region;
    pp_block_with state value;
| E_Int e   -> pp_node state "Int";
            pp_int  state e
| E_Nat e   -> pp_node state "Nat";
            pp_int  state e
| E_Mutez e -> pp_node state "Mutez";
            pp_int  state e
| E_False e -> pp_loc_node state "False" e
| E_True  e -> pp_loc_node state "True"  e

and pp_proj_expr state (node : projection reg) =
  pp_loc_node   state "E_Proj" node.region;
  pp_projection state node.value

and pp_record state (node : record reg) =
  pp_loc_node     state "E_Record" node.region;
  pp_ne_injection state pp_field_assignment node.value

and pp_expr_case state (node : expr case reg) =
  pp_loc_node state "E_Case" node.region;
  pp_case     state pp_expr node.value

and pp_expr_conditional state (node : expr conditional reg) =
  pp_loc_node  state "E_Cond<" node.region;
  pp_cond_expr state node.value

and pp_annot_expr state (node : annot_expr par reg) =
  let expr, (_, type_expr) = node.value.inside in
  pp_loc_node  state "E_Annot" node.region;
  pp_expr      (state#pad 2 0) expr;
  pp_type_expr (state#pad 2 1) type_expr

and pp_list_comp state (node : expr injection reg) =
  pp_loc_node state "E_List" node.region;
  if node.value.elements = None then
    pp_node (state#pad 1 0) "[]"
  else pp_injection state pp_expr node.value

and pp_set_mem state (node : set_mem reg) =
  pp_loc_node state "SetMem" node.region;
  pp_expr (state#pad 2 0) node.value.set;
  pp_expr (state#pad 2 1) node.value.element

and pp_set_injection state (node : expr injection reg) =
  pp_loc_node  state "SetInj" node.region;
  pp_injection state pp_expr node.value


and pp_some_app state (node : (kwd_Some * arguments) reg) =
  let _, args = node.value in
  pp_loc_node   state "E_Some" node.region;
  pp_tuple_expr state args

and pp_ctor_app state (node :  (ctor * arguments option) reg) =
  let ctor, args_opt = node.value in
  pp_loc_node state "E_Ctor" node.region;
  let state = state#pad 1 0 in
  pp_ident     state ctor;
  print_option state pp_tuple_expr args_opt

and pp_tuple_expr state (node : (expr, comma) Utils.nsepseq par reg) =
  let exprs          = Utils.nsepseq_to_list node.value.inside in
  let length         = List.length exprs in
  let apply len rank = pp_expr (state#pad len rank)
  in List.iteri (apply length) exprs


and pp_op1 state node {value; region} =
  pp_loc_node state node region;
  pp_expr     (state#pad 1 0) value.arg;

and pp_op2 state node {value; region} =
  pp_loc_node state node region;
  pp_expr     (state#pad 2 0) value.arg1;
  pp_expr     (state#pad 2 1) value.arg2

and pp_type_annot state rank = function
         None -> rank
| Some (_, e) -> pp_type_expr state e; rank+1
