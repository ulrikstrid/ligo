[@@@coverage exclude_file]

open CST

module Region = Simple_utils.Region
open! Region

let sprintf = Printf.sprintf

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
       and the padding for the new node itself (Is it the last child
       of its parent?).
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

(* Printing the tokens with their source regions *)

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

(**)

let print_token state lexeme region =
  let line =
    sprintf "%s: %s\n" (compact state region) lexeme
  in Buffer.add_string state#buffer line

let print_token_opt state lexeme =
  print_option state (fun state -> print_token state lexeme)

let print_variable state {region; value} =
  let line =
    sprintf "%s: Ident %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_ctor state {region; value} =
  let line =
    sprintf "%s: Ctor %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_string state {region; value} =
  let line =
    sprintf "%s: String %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_verbatim state {region; value} =
  let line =
    sprintf "%s: Verbatim %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_bytes state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Bytes (%S, \"0x%s\")\n"
            (compact state region) lexeme
            (Hex.show abstract)
  in Buffer.add_string state#buffer line

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

let print_mutez state {region; value=lex,z} =
  let line =
    sprintf "Mutez %s (%s)" lex (Z.to_string z)
  in print_token state line region

let rec print_tokens state (node : cst) =
  print_nseq  state print_declaration node.decl;
  print_token state "EOF" node.eof

and print_declaration state = function
  TypeDecl    d -> print_type_decl    state d.value
| ConstDecl   d -> print_const_decl   state d.value
| FunDecl     d -> print_fun_decl     state d.value
| ModuleDecl  d -> print_module_decl  state d.value
| ModuleAlias d -> print_module_alias state d.value

and print_const_decl state (node : const_decl) =
  print_attributes state node.attributes;
  print_token      state "const" node.kwd_const;
  print_variable   state node.name;
  print_option     state print_type_annot node.const_type;
  print_token      state "=" node.equal;
  print_expr       state node.init;
  print_token_opt  state ";" node.terminator

and print_type_decl state (node : type_decl) =
  print_token     state "type" node.kwd_type;
  print_variable  state node.name;
  print_token     state "is" node.kwd_is;
  print_type_expr state node.type_expr;
  print_token_opt state ";" node.terminator

and print_module_decl state (node : module_decl) =
  match node.enclosing with
    Brace (lbrace, rbrace) ->
      print_token     state "module" node.kwd_module;
      print_variable  state node.name;
      print_token     state "is" node.kwd_is;
      print_token     state "{" lbrace;
      print_tokens    state node.structure;
      print_token     state  "}" rbrace;
      print_token_opt state ";" node.terminator
  | BeginEnd (kwd_begin, kwd_end) ->
      print_token     state "module" node.kwd_module;
      print_variable  state node.name;
      print_token     state "is" node.kwd_is;
      print_token     state "begin" kwd_begin;
      print_tokens    state node.structure;
      print_token     state "end" kwd_end;
      print_token_opt state ";" node.terminator

and print_module_alias state (node : module_alias) =
  print_token     state "module" node.kwd_module;
  print_variable  state node.alias;
  print_token     state "is" node.kwd_is;
  print_nsepseq   state print_variable "." node.mod_path;
  print_token_opt state ";" node.terminator

and print_par : 'a.state -> (state -> 'a -> unit) -> 'a par -> unit =
  fun state print par ->
    print_token state "(" par.lpar;
    print       state par.inside;
    print_token state ")" par.rpar

and print_braces : 'a.state -> (state -> 'a -> unit) -> 'a braces -> unit =
  fun state print braces ->
    print_token state "{" braces.lbrace;
    print       state braces.inside;
    print_token state "}" braces.rbrace

and print_brackets : 'a.state -> (state -> 'a -> unit) -> 'a brackets -> unit =
  fun state print brackets ->
    print_token state "[" brackets.lbracket;
    print       state brackets.inside;
    print_token state "]" brackets.rbracket

and print_case : 'a.state -> (state -> 'a -> unit) -> 'a case -> unit =
  fun state print node ->
    print_token state "case" node.kwd_case;
    print_expr  state node.expr;
    print_token state "of" node.kwd_of;
    match node.enclosing with
      Brackets (lbracket, rbracket) ->
        print_token     state "[" lbracket;
        print_token_opt state "|" node.lead_vbar;
        print_cases     state print node.cases.value;
        print_token     state "]" rbracket
    | End kwd_end ->
        print_token_opt state "|" node.lead_vbar;
        print_cases     state print node.cases.value;
        print_token     state "end" kwd_end

and print_cases :
  'a.state -> (state -> 'a -> unit) ->
  ('a case_clause reg, vbar) Utils.nsepseq -> unit =
    fun state print node ->
      let print state node = print_case_clause state print node.value
      in print_nsepseq state print "|" node

and print_case_clause :
  'a.state -> (state -> 'a -> unit) -> 'a case_clause -> unit =
  fun state print node ->
    print_pattern state node.pattern;
    print_token   state "->" node.arrow;
    print         state node.rhs

and print_type_expr state = function
  TProd t   -> print_cartesian  state t
| TSum t    -> print_sum_type   state t.value
| TRecord t -> print_TRecord    state t.value
| TApp t    -> print_TApp       state t.value
| TFun t    -> print_TFun       state t.value
| TPar t    -> print_TPar       state t.value
| TVar t    -> print_variable   state t
| TWild t   -> print_wild       state t
| TString t -> print_string     state t
| TInt t    -> print_int        state t
| TModA t   -> print_module_access state print_type_expr t.value

and print_wild state = print_token state "_"

and print_type_annot state (colon, type_expr) =
  print_token     state ":" colon;
  print_type_expr state type_expr;

and print_cartesian state (node : cartesian) =
  print_nsepseq state print_type_expr "*" node.value

and print_of_type_expr state (kwd_of, t_expr) =
  print_token     state "of" kwd_of;
  print_type_expr state t_expr

and print_variant state (node: variant) =
  print_attributes state node.attributes;
  print_ctor       state node.ctor;
  print_option     state print_of_type_expr node.arg

and print_sum_type state (node : sum_type) =
  print_attributes state node.attributes;
  print_token_opt  state "|" node.lead_vbar;
  print_nsepseq    state (strip print_variant) "|" node.variants

and print_TRecord state =
  print_ne_injection state (strip print_field_decl)

and print_TApp state (node : type_ctor * type_tuple) =
  let type_name, type_tuple = node in
  print_variable        state type_name;
  print_type_tuple state type_tuple

and print_TFun state (node : type_expr * arrow * type_expr) =
  let type_expr_a, arrow, type_expr_b = node in
  print_type_expr state type_expr_a;
  print_token     state "->" arrow;
  print_type_expr state type_expr_b

and print_TPar state = print_par state print_type_expr

and print_field_decl state (node : field_decl) =
  print_attributes state node.attributes;
  print_variable        state node.field_name;
  print_token      state ":" node.colon;
  print_type_expr  state node.field_type

and print_type_tuple state (node : type_tuple) =
  let print state = print_nsepseq state print_type_expr ","
  in print_par state print node.value

and print_fun_decl state (node : fun_decl) =
  print_attributes state node.attributes;
  print_token      state "function" node.kwd_function;
  print_variable        state node.fun_name;
  print_parameters state node.param;
  print_option     state print_type_annot node.ret_type;
  print_token      state "is" node.kwd_is;
  print_expr       state node.return;
  print_token_opt  state ";" node.terminator;

and print_fun_expr state (node : fun_expr) =
  print_token      state "function" node.kwd_function;
  print_parameters state node.param;
  print_option     state print_type_annot node.ret_type;
  print_token      state "is" node.kwd_is;
  print_expr       state node.return

and print_code_inj state (node : code_inj) =
  let {value; region} = node.language in
  let header_stop = region#start#shift_bytes 1 in
  let header_reg  = Region.make ~start:region#start ~stop:header_stop in
  print_token  state "[%" header_reg;
  print_string state value;
  print_expr   state node.code;
  print_token  state "]" node.rbracket

and print_block_with state (node : block_with) =
  print_block state node.block;
  print_token state "with" node.kwd_with;
  print_expr  state node.expr;

and print_parameters state (node : parameters) =
  let print state = print_nsepseq state print_param_decl ";"
  in print_par state print node.value

and print_param_decl state = function
  ParamConst p -> print_param_const state p
| ParamVar   p -> print_param_var   state p

and print_param_const state (node : param_const reg) =
  print_token  state "const" node.value.kwd_const;
  print_variable    state node.value.var;
  print_option state print_type_annot node.value.param_type

and print_param_var state (node : param_var reg) =
  print_token  state "var" node.value.kwd_var;
  print_variable    state node.value.var;
  print_option state print_type_annot node.value.param_type

and print_block state (node : block reg) =
  match node.value.enclosing with
    Block (kwd_block, lbrace, rbrace) ->
      print_token      state "block" kwd_block;
      print_token      state "{" lbrace;
      print_statements state node.value.statements;
      print_token_opt  state ";" node.value.terminator;
      print_token      state "}" rbrace
  | BeginEnd (kwd_begin, kwd_end) ->
      print_token      state "begin" kwd_begin;
      print_statements state node.value.statements;
      print_token_opt  state ";" node.value.terminator;
      print_token      state "end" kwd_end

and print_data_decl state = function
  LocalConst       d -> print_const_decl   state d.value
| LocalVar         d -> print_var_decl     state d
| LocalFun         d -> print_fun_decl     state d.value
| LocalType        d -> print_type_decl    state d.value
| LocalModule      d -> print_module_decl  state d.value
| LocalModuleAlias d -> print_module_alias state d.value

and print_var_decl state (node : var_decl reg) =
  print_token     state "var" node.value.kwd_var;
  print_variable       state node.value.name;
  print_option    state print_type_annot node.value.var_type;
  print_token     state ":=" node.value.assign;
  print_expr      state node.value.init;
  print_token_opt state ";" node.value.terminator

and print_attributes state (node : attribute list) =
  let apply {value; region} =
    let attribute_formatted = sprintf "[@%s]" value in
    print_token state attribute_formatted region
  in List.iter apply node

and print_statements state =
  print_nsepseq state print_statement ";"

and print_statement state = function
  Instr instr -> print_instruction state instr
| Data  data  -> print_data_decl   state data

and print_instruction state = function
  Cond        i -> print_cond_instr   state i
| CaseInstr   i -> print_case         state print_test_clause i.value
| Assign      i -> print_assignment   state i
| Loop        i -> print_loop         state i
| ProcCall    i -> print_fun_call     state i
| Skip        i -> print_token        state "skip" i
| RecordPatch i -> print_record_patch state i.value
| MapPatch    i -> print_map_patch    state i.value
| SetPatch    i -> print_set_patch    state i.value
| MapRemove   i -> print_map_remove   state i.value
| SetRemove   i -> print_set_remove   state i.value

and print_ECond state (node : expr conditional reg) =
  print_token     state "if" node.value.kwd_if;
  print_expr      state node.value.test;
  print_token     state "then" node.value.kwd_then;
  print_expr      state node.value.ifso;
  print_token_opt state ";" node.value.terminator;
  print_token     state "else" node.value.kwd_else;
  print_expr      state node.value.ifnot

and print_cond_instr state (node : test_clause conditional reg) =
  print_token       state "if" node.value.kwd_if;
  print_expr        state node.value.test;
  print_token       state "then" node.value.kwd_then;
  print_test_clause state node.value.ifso;
  print_token_opt   state ";" node.value.terminator;
  print_token       state "else" node.value.kwd_else;
  print_test_clause state node.value.ifnot

and print_test_clause state = function
  ClauseInstr instr -> print_instruction  state instr
| ClauseBlock block -> print_clause_block state block

and print_clause_block state = function
  LongBlock block ->
    print_block state block
| ShortBlock block ->
    let print state (stmts, terminator) =
      print_statements state stmts;
      print_token_opt state ";" terminator
    in print_braces state print block.value

and print_assignment state (node: assignment reg) =
  print_lhs   state node.value.lhs;
  print_token state ":=" node.value.assign;
  print_expr  state node.value.rhs

and print_lhs state = function
  Path    path -> print_path       state path
| MapPath path -> print_map_lookup state path.value

and print_loop state = function
  While loop -> print_while_loop state loop
| For   loop -> print_for_loop   state loop

and print_while_loop state (node : while_loop reg) =
  print_token state "while" node.value.kwd_while;
  print_expr  state node.value.cond;
  print_block state node.value.block

and print_for_loop state = function
  ForInt     loop -> print_for_int     state loop
| ForCollect loop -> print_for_collect state loop

and print_step state (kwd_step, expr) =
  print_token state "step" kwd_step;
  print_expr  state expr

and print_for_int state (node : for_int reg) =
  print_token  state "for" node.value.kwd_for;
  print_variable    state node.value.binder;
  print_token  state ":=" node.value.assign;
  print_expr   state node.value.init;
  print_token  state "to" node.value.kwd_to;
  print_expr   state node.value.bound;
  print_option state print_step node.value.step;
  print_block  state node.value.block

and print_for_collect state (node : for_collect reg) =
  print_token      state "for" node.value.kwd_for;
  print_variable        state node.value.var;
  print_option     state print_bind_to node.value.bind_to;
  print_token      state "in" node.value.kwd_in;
  print_collection state node.value.collection;
  print_expr       state node.value.expr;
  print_block      state node.value.block

and print_collection state = function
  Map  kwd_map  -> print_token state "map"  kwd_map
| Set  kwd_set  -> print_token state "set"  kwd_set
| List kwd_list -> print_token state "list" kwd_list

and print_bind_to state (arrow, variable) =
  print_token state "->" arrow;
  print_variable   state variable

and print_expr state = function
  ECase    e -> print_ECase       state e
| ECond    e -> print_ECond       state e
| EAnnot   e -> print_EAnnot      state e
| ELogic   e -> print_logic_expr  state e
| EArith   e -> print_arith_expr  state e
| EString  e -> print_string_expr state e
| EList    e -> print_list_expr   state e
| ESet     e -> print_set_expr    state e
| ECtor    e -> print_ctor_expr   state e
| ERecord  e -> print_record      state e.value
| EProj    e -> print_projection  state e.value
| EModA    e -> print_module_access state print_expr e.value
| EUpdate  e -> print_update_expr state e.value
| EMap     e -> print_map_expr    state e
| EVar     e -> print_variable    state e
| ECall    e -> print_fun_call    state e
| EBytes   e -> print_bytes       state e
| EUnit    e -> print_token       state "Unit" e
| ETuple   e -> print_tuple_expr  state e
| EPar     e -> print_EPar        state e
| EFun     e -> print_fun_expr    state e.value
| ECodeInj e -> print_code_inj    state e.value
| EBlock   e -> print_block_with  state e.value

and print_ECase state (node : expr case reg) =
  print_case state print_expr node.value

and print_EAnnot state (node : annot_expr par reg) =
  let print state (expr, type_annot) =
    print_expr state expr;
    print_type_annot state type_annot
  in print_par state print node.value

and print_map_expr state = function
  MapLookUp e -> print_map_lookup state e.value
| MapInj    e -> print_injection  state (strip print_binding) e.value
| BigMapInj e -> print_injection  state (strip print_binding) e.value

and print_set_expr state = function
  SetInj e -> print_injection state print_expr e.value
| SetMem e -> print_set_membership state e.value

and print_set_membership state (node : set_membership) =
  print_expr  state node.set;
  print_token state "contains" node.kwd_contains;
  print_expr  state node.element

and print_map_lookup state (node : map_lookup) =
  print_path     state node.path;
  print_brackets state print_expr node.index.value

and print_path state = function
  Name var  -> print_variable   state var
| Path path -> print_projection state path.value

and print_logic_expr state = function
  BoolExpr e -> print_bool_expr state e
| CompExpr e -> print_comp_expr state e

and print_un_op state lexeme (node : keyword un_op) =
  print_token state lexeme node.op;
  print_expr  state node.arg

and print_bin_op state lexeme (node : keyword bin_op) =
  print_expr  state node.arg1;
  print_token state lexeme node.op;
  print_expr  state node.arg2

and print_bool_expr state = function
  Or    e -> print_bin_op state "||"    e.value
| And   e -> print_bin_op state "&&"    e.value
| Not   e -> print_un_op  state "not"   e.value
| False e -> print_token  state "False" e
| True  e -> print_token  state "True"  e

and print_comp_expr state = function
  Lt    e -> print_bin_op state "<"   e.value
| Leq   e -> print_bin_op state "<="  e.value
| Gt    e -> print_bin_op state ">"   e.value
| Geq   e -> print_bin_op state ">="  e.value
| Equal e -> print_bin_op state "="   e.value
| Neq   e -> print_bin_op state "=/=" e.value

and print_arith_expr state = function
  Add   e -> print_bin_op state "+"   e.value
| Sub   e -> print_bin_op state "-"   e.value
| Mult  e -> print_bin_op state "*"   e.value
| Div   e -> print_bin_op state "/"   e.value
| Mod   e -> print_bin_op state "mod" e.value
| Neg   e -> print_un_op  state "-"   e.value
| Int   e -> print_int    state e
| Nat   e -> print_nat    state e
| Mutez e -> print_mutez  state e

and print_string_expr state = function
  Cat      e -> print_bin_op   state "^" e.value
| String   e -> print_string   state e
| Verbatim e -> print_verbatim state e

and print_list_expr state = function
  ECons     e -> print_bin_op    state "#" e.value
| EListComp e -> print_injection state print_expr e.value
| ENil      e -> print_nil       state e

and print_ctor_expr state = function
  SomeApp  e -> print_SomeApp  state e
| NoneExpr e -> print_NoneExpr state e
| CtorApp  e -> print_CtorApp  state e

and print_record state =
  print_ne_injection state (strip print_field_assignment)

and print_field_assignment state (node : field_assignment) =
  print_variable   state node.field_name;
  print_token state "=" node.assignment;
  print_expr  state node.field_expr

and print_field_path_assignment state (node : field_path_assignment reg) =
  print_path  state node.value.field_path;
  print_token state "=" node.value.assignment;
  print_expr  state node.value.field_expr

and print_update_expr state (node : update) =
  print_path         state node.record;
  print_token        state "with" node.kwd_with;
  print_ne_injection state print_field_path_assignment node.updates.value

and print_projection state (node : projection) =
  print_variable   state node.struct_name;
  print_token      state "." node.selector;
  print_field_path state node.field_path

and print_module_access :
  'a.state -> (state -> 'a -> unit ) -> 'a module_access -> unit =
  fun state print node ->
    print_variable   state node.module_name;
    print_token state "." node.selector;
    print       state node.field

and print_field_path state =
  print_nsepseq state print_selection "."

and print_selection state = function
  FieldName name -> print_variable state name
| Component int  -> print_int state int

and print_record_patch state (node : record_patch) =
  print_token        state "patch" node.kwd_patch;
  print_path         state node.path;
  print_token        state "with" node.kwd_with;
  print_ne_injection state (strip print_field_assignment)
                           node.record_inj.value

and print_set_patch state (node : set_patch) =
  print_token        state "patch" node.kwd_patch;
  print_path         state node.path;
  print_token        state "with" node.kwd_with;
  print_ne_injection state print_expr node.set_inj.value

and print_map_patch state (node : map_patch) =
  print_token        state "patch" node.kwd_patch;
  print_path         state node.path;
  print_token        state "with" node.kwd_with;
  print_ne_injection state (strip print_binding) node.map_inj.value

and print_map_remove state (node : map_remove) =
  print_token state "remove" node.kwd_remove;
  print_expr  state node.key;
  print_token state "from" node.kwd_from;
  print_token state "map" node.kwd_map;
  print_path  state node.map

and print_set_remove state (node : set_remove) =
  print_token state "remove" node.kwd_remove;
  print_expr  state node.element;
  print_token state "from" node.kwd_from;
  print_token state "set" node.kwd_set;
  print_path  state node.set

and print_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection -> unit =
  fun state print node ->
    print_injection_kwd state node.kind;
    match node.enclosing with
      Brackets (lbracket, rbracket) ->
        print_token     state "[" lbracket;
        print_sepseq    state print ";" node.elements;
        print_token_opt state ";" node.terminator;
        print_token     state "]" rbracket
    | End kwd_end ->
        print_sepseq    state print ";" node.elements;
        print_token_opt state ";" node.terminator;
        print_token     state "end" kwd_end

and print_injection_kwd state = function
  InjSet    kwd_set     -> print_token state "set"     kwd_set
| InjMap    kwd_map     -> print_token state "map"     kwd_map
| InjBigMap kwd_big_map -> print_token state "big_map" kwd_big_map
| InjList   kwd_list    -> print_token state "list"    kwd_list

and print_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection -> unit =
  fun state print node ->
    print_attributes       state node.attributes;
    print_ne_injection_kwd state node.kind;
    match node.enclosing with
      Brackets (lbracket, rbracket) ->
        print_token     state "[" lbracket;
        print_nsepseq   state print ";" node.ne_elements;
        print_token_opt state ";" node.terminator;
        print_token     state "]" rbracket
    | End kwd_end ->
        print_nsepseq   state print ";" node.ne_elements;
        print_token_opt state ";" node.terminator;
        print_token     state "end" kwd_end

and print_ne_injection_kwd state = function
  NEInjSet    kwd_set    -> print_token state "set"    kwd_set
| NEInjMap    kwd_map    -> print_token state "map"    kwd_map
| NEInjRecord kwd_record -> print_token state "record" kwd_record

and print_binding state (node : binding) =
  print_expr  state node.source;
  print_token state "->" node.arrow;
  print_expr  state node.image

and print_tuple_expr state (node : tuple_expr) =
  let print state = print_nsepseq state print_expr ","
  in print_par state print node.value

and print_nil state = print_token state "nil"

and print_NoneExpr state = print_token state "None"

and print_fun_call state (node : fun_call) =
  let expr, arguments = node.value in
  print_expr       state expr;
  print_tuple_expr state arguments

and print_CtorApp state (node : (ctor * arguments option) reg) =
  let ctor, arguments = node.value in
  print_ctor   state ctor;
  print_option state print_tuple_expr arguments

and print_SomeApp state (node : (kwd_Some * arguments) reg) =
  let ctor_Some, arguments = node.value in
  print_token      state "Some" ctor_Some;
  print_tuple_expr state arguments

and print_EPar state (node : expr par reg) =
  print_par state print_expr node.value

and print_pattern state = function
  PVar    p -> print_variable          state p
| PWild   p -> print_token        state "_" p
| PInt    p -> print_int          state p
| PNat    p -> print_nat          state p
| PBytes  p -> print_bytes        state p
| PString p -> print_string       state p
| PList   p -> print_list_pattern state p
| PTuple  p -> print_ptuple       state p
| PCtor   p -> print_ctor_pattern state p

and print_ctor_pattern state = function
  PUnit    p -> print_token     state "Unit"  p
| PFalse   p -> print_token     state "False" p
| PTrue    p -> print_token     state "True"  p
| PNone    p -> print_token     state "None"  p
| PSomeApp p -> print_psome_app state p
| PCtorApp p -> print_pctor_app state p

and print_pctor_app state (node : (ctor * tuple_pattern option) reg) =
  let ctor, arg_opt = node.value in
  print_ctor   state ctor;
  print_option state print_ptuple arg_opt

and print_psome_app state (node : (kwd_Some * pattern par reg) reg) =
  let ctor_Some, patterns = node.value in
  print_token    state "Some" ctor_Some;
  print_patterns state patterns

and print_patterns state (node : pattern par reg) =
  print_par state print_pattern node.value

and print_list_pattern state = function
  PListComp p -> print_injection state print_pattern p.value
| PNil      p -> print_token     state "nil" p
| PParCons  p -> print_par_cons  state p
| PCons     p -> print_nsepseq   state print_pattern "#" p.value

and print_par_cons state (node :  (pattern * cons * pattern) par reg) =
  let print state (head, cons, tail) =
    print_pattern state head;
    print_token   state "#" cons;
    print_pattern state tail;
  in print_par state print node.value

and print_ptuple state (node : tuple_pattern) =
  let print state = print_nsepseq state print_pattern ","
  in print_par state print node.value

(* Conversion to string *)

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

(* Pretty-printing the CST *)

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
  TypeDecl {value; region} ->
    pp_loc_node  state "TypeDecl" region;
    pp_type_decl state value
| ConstDecl {value; region} ->
    pp_loc_node   state "ConstDecl" region;
    pp_const_decl state value
| FunDecl {value; region} ->
    pp_loc_node state "FunDecl" region;
    pp_fun_decl state value
| ModuleDecl {value; region} ->
    pp_loc_node state "ModuleDecl" region;
    pp_mod_decl state value
| ModuleAlias {value; region} ->
    pp_loc_node  state "ModuleAlias" region;
    pp_mod_alias state value

and pp_type_decl state (decl : type_decl) =
  let () = pp_ident (state#pad 2 0) decl.name in
  let () =
    let state = state#pad 2 1 in
    pp_node      state "<type_expr>";
    pp_type_expr (state#pad 1 0) decl.type_expr
  in ()

and pp_mod_decl state (decl : module_decl) =
  let ()    = pp_ident (state#pad 2 0) decl.name in
  let state = state#pad 2 1 in
  let ()    = pp_node state "<structure>" in
  let apply len rank =
    pp_declaration (state#pad len rank) in
  let decls = Utils.nseq_to_list decl.structure.decl in
  List.iteri (List.length decls |> apply) decls

and pp_mod_alias state (node : module_alias) =
  let () = pp_ident (state#pad 2 0) node.alias in
  let () =
    let state          = state#pad 2 1 in
    let mod_path       = Utils.nsepseq_to_list node.mod_path in
    let len            = List.length mod_path in
    let apply len rank = pp_ident (state#pad len rank) in
    pp_node  state "<path>";
    List.iteri (apply len) mod_path
  in ()

and pp_fun_decl state (node : fun_decl) =
  let arity = if node.kwd_recursive = None then 0 else 1 in
  let arity = if node.ret_type = None then arity else arity+1 in
  let arity = if node.attributes = [] then arity else arity+1 in
  let arity = arity + 3
  and rank = 0 in
  let rank =
    match node.kwd_recursive with
        None -> rank
    | Some _ -> let state = state#pad arity rank in
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
  TProd cartesian ->
    pp_loc_node  state "TProd" cartesian.region;
    pp_cartesian state cartesian
| TVar v ->
    pp_node  state "TVar";
    pp_ident (state#pad 1 0) v
| TPar {value; region} ->
    pp_loc_node  state "TPar" region;
    pp_type_expr (state#pad 1 0) value.inside
| TApp {value=name,tuple; region} ->
    pp_loc_node   state "TApp" region;
    pp_ident      (state#pad 1 0) name;
    pp_type_tuple (state#pad 2 1) tuple
| TFun {value; region} ->
    pp_loc_node state "TFun" region;
    let apply len rank =
      pp_type_expr (state#pad len rank) in
    let domain, _, range = value in
    List.iteri (apply 2) [domain; range]
| TSum {value; region} ->
    pp_loc_node state "TSum" region;
    pp_sum_type state value
| TRecord {value; region} ->
    pp_loc_node     state "TRecord" region;
    pp_ne_injection state pp_field_decl value
| TString s ->
    pp_node   state "TString";
    pp_string (state#pad 1 0) s
| TInt s ->
    pp_node state "TInt";
    pp_int  (state#pad 1 0) s
| TWild wild ->
    pp_node     state "TWild";
    pp_loc_node state "TWild" wild
| TModA {value; region} ->
    pp_loc_node      state "TModA" region;
    pp_module_access state pp_type_expr value

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
  Instr instr ->
    pp_node        state "Instr";
    pp_instruction (state#pad 1 0) instr
| Data decl ->
    pp_node state "Data";
    pp_data_decl (state#pad 1 0) decl

and pp_instruction state = function
  Cond {value; region} ->
    pp_loc_node   state "Cond" region;
    pp_cond_instr state value
| CaseInstr {value; region} ->
    pp_loc_node state "CaseInstr" region;
    pp_case     state pp_test_clause value
| Assign {value; region} ->
    pp_loc_node   state "Assign" region;
    pp_assignment state value
| Loop loop ->
    pp_node state "Loop";
    pp_loop (state#pad 1 0) loop
| ProcCall {value; region} ->
    pp_loc_node state "ProcCall" region;
    pp_fun_call state value
| Skip region ->
    pp_loc_node state "Skip" region
| RecordPatch {value; region} ->
    pp_loc_node     state "RecordPatch" region;
    pp_record_patch state value
| MapPatch {value; region} ->
    pp_loc_node  state "MapPatch" region;
    pp_map_patch state value
| SetPatch {value; region} ->
    pp_loc_node  state "SetPatch" region;
    pp_set_patch state value
| MapRemove {value; region} ->
    pp_loc_node   state "MapRemove" region;
    pp_map_remove state value
| SetRemove {value; region} ->
    pp_loc_node   state "SetRemove" region;
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
  PWild region ->
    pp_loc_node state "PWild" region
| PCtor pattern ->
    pp_node state "PCtor";
    pp_ctor_pattern (state#pad 1 0) pattern
| PVar v ->
    pp_node state "PVar";
    pp_ident (state#pad 1 0) v
| PInt n ->
    pp_node state "PInt";
    pp_int state n
| PNat n ->
    pp_node state "PNat";
    pp_int state n
| PBytes b ->
    pp_node state "PBytes";
    pp_bytes state b
| PString s ->
    pp_node state "PString";
    pp_ident (state#pad 1 0) s
| PList plist ->
    pp_node state "PList";
    pp_list_pattern (state#pad 1 0) plist
| PTuple {value; region} ->
    pp_loc_node state "PTuple" region;
    pp_tuple_pattern (state#pad 1 0) value

and pp_bytes state {value=lexeme,hex; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Hex.show hex)

and pp_int state {value=lexeme,z; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Z.to_string z)

and pp_ctor_pattern state = function
  PNone region ->
    pp_loc_node state "PNone" region
| PSomeApp {value=_,{value=par; _}; region} ->
    pp_loc_node state "PSomeApp" region;
    pp_pattern (state#pad 1 0) par.inside
| PUnit region ->
    pp_loc_node state "PUnit" region
| PFalse region ->
    pp_loc_node state "PFalse" region
| PTrue region ->
    pp_loc_node state "PTrue" region
| PCtorApp {value; region} ->
    pp_loc_node state "PCtorApp" region;
    pp_ctor_app_pattern (state#pad 1 0) value

and pp_ctor_app_pattern state (ctor, pat_opt) =
  pp_ident state ctor;
  match pat_opt with
      None -> ()
  | Some p -> pp_tuple_pattern state p.value

and pp_list_pattern state = function
  PListComp {value; region} ->
    pp_loc_node state "PListComp" region;
    pp_injection (state#pad 1 0) pp_pattern value
| PNil region ->
    pp_loc_node state "PNil" region
| PParCons {value; region} ->
    pp_loc_node state "PParCons" region;
    pp_bin_cons (state#pad 1 0) value.inside
| PCons {value; region} ->
    let patterns = Utils.nsepseq_to_list value in
    let length   = List.length patterns in
    let apply len rank =
      pp_pattern (state#pad len rank) in
    pp_loc_node state "PCons" region;
    List.iteri (apply length) patterns

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

and pp_module_access :
  'a.state -> (state -> 'a -> unit ) -> 'a module_access -> unit =
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

and pp_loop state = function
  While {value; _} ->
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
| For for_loop ->
    pp_node state "<for>";
    pp_for_loop (state#pad 1 0) for_loop

and pp_for_loop state = function
  ForInt {value; region} ->
    pp_loc_node state "ForInt" region;
    pp_for_int state value
| ForCollect {value; region} ->
    pp_loc_node state "ForCollect" region;
    pp_for_collect state value

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

and pp_for_collect state (node : for_collect) =
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
  Map  region -> pp_loc_node state "map"  region
| Set  region -> pp_loc_node state "set"  region
| List region -> pp_loc_node state "list" region

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

and pp_data_decl state = function
  LocalConst {value; region} ->
    pp_loc_node state "LocalConst" region;
    pp_const_decl state value
| LocalVar {value; region} ->
    pp_loc_node state "LocalVar" region;
    pp_var_decl state value
| LocalFun {value; region} ->
    pp_loc_node state "LocalFun" region;
    pp_fun_decl state value
| LocalType type_decl ->
    pp_node state "Type";
    pp_type_decl (state#pad 1 0) type_decl.value
| LocalModule module_decl ->
    pp_node state "Module";
    pp_mod_decl (state#pad 1 0) module_decl.value
| LocalModuleAlias module_alias ->
    pp_node state "Module";
    pp_mod_alias (state#pad 1 0) module_alias.value

and pp_var_decl state (node : var_decl) =
  let arity = if node.var_type = None then 2 else 3
  and rank  = 0 in
  let rank  = pp_ident (state#pad arity rank) node.name; rank+1 in
  let rank  = pp_type_annot (state#pad arity rank) rank node.var_type
  in pp_expr (state#pad arity rank) node.init

and pp_expr state = function
  ECase  e -> pp_expr_case state e
| ECond  e -> pp_expr_conditional state e
| EAnnot e -> pp_annot_expr state e
| ELogic e -> pp_logic_expr state e
| EArith e -> pp_arith_expr state e
| EString e -> pp_string_expr state e
| EList e -> pp_list_expr state e
| ESet e -> pp_set_expr state e
| ECtor e -> pp_ctor_expr state e
| ERecord e -> pp_record state e
| EProj e -> pp_proj_expr state e
| EModA e -> let {value; region} = e in
    pp_loc_node      state "EModA" region;
    pp_module_access state pp_expr value
| EUpdate {value; region} ->
    pp_loc_node state "EUpdate" region;
    pp_update   state value
| EMap e_map ->
    pp_node     state "EMap";
    pp_map_expr (state#pad 1 0) e_map
| EVar v ->
    pp_node  state "EVar";
    pp_ident (state#pad 1 0) v
| ECall {value; region} ->
    pp_loc_node state "ECall" region;
    pp_fun_call state value
| EBytes b ->
    pp_node state "EBytes";
    pp_bytes state b
| EUnit region ->
    pp_loc_node state "EUnit" region
| ETuple e_tuple ->
    pp_node       state "ETuple";
    pp_tuple_expr state e_tuple
| EPar {value; region} ->
    pp_loc_node state "EPar" region;
    pp_expr     (state#pad 1 0) value.inside
| EFun {value; region} ->
    pp_loc_node state "EFun" region;
    pp_fun_expr state value;
| ECodeInj {value; region} ->
    pp_loc_node state "ECodeInj" region;
    pp_code_inj state value;
| EBlock {value; region} ->
    pp_loc_node   state "EBlock" region;
    pp_block_with state value;

and pp_proj_expr state (node : projection reg) =
  pp_loc_node   state "EProj" node.region;
  pp_projection state node.value

and pp_record state (node : record reg) =
  pp_loc_node     state "ERecord" node.region;
  pp_ne_injection state pp_field_assignment node.value

and pp_expr_case state (node : expr case reg) =
  pp_loc_node state "ECase" node.region;
  pp_case     state pp_expr node.value

and pp_expr_conditional state (node : expr conditional reg) =
  pp_loc_node  state "ECond" node.region;
  pp_cond_expr state node.value

and pp_annot_expr state (node : annot_expr par reg) =
  let expr, (_, type_expr) = node.value.inside in
  pp_loc_node  state "EAnnot" node.region;
  pp_expr      (state#pad 2 0) expr;
  pp_type_expr (state#pad 2 1) type_expr

and pp_logic_expr state (node : logic_expr) =
  pp_node    state "ELogic";
  pp_e_logic (state#pad 1 0) node

and pp_arith_expr state (node : arith_expr)=
  pp_node state "EArith";
  match node with
    Add e   -> pp_op2 state "Add" e
  | Sub e   -> pp_op2 state "Sub" e
  | Mult e  -> pp_op2 state "Mult" e
  | Div e   -> pp_op2 state "Div" e
  | Mod e   -> pp_op2 state "Mod" e
  | Neg e   -> pp_op1 state "Neg" e
  | Int e   -> pp_node state "Int";
              pp_int  state e
  | Nat e   -> pp_node state "Nat";
              pp_int  state e
  | Mutez e -> pp_node state "Mutez";
              pp_int  state e

and pp_list_expr state (node : list_expr) =
  pp_node state "EList";
  let state = state#pad 1 0 in
  match node with
    ECons     e -> pp_op2       state "ECons" e
  | ENil      e -> pp_loc_node  state "ENil" e
  | EListComp e -> pp_list_comp state e

and pp_list_comp state (node : expr injection reg) =
  pp_loc_node state "EListComp" node.region;
  if node.value.elements = None then
    pp_node (state#pad 1 0) "[]"
  else pp_injection state pp_expr node.value

and pp_set_expr state (node : set_expr) =
  pp_node  state "ESet";
  let state = state#pad 1 0 in
  match node with
    SetInj e -> pp_set_injection state e
  | SetMem e -> pp_set_membership state e

and pp_set_membership state (node : set_membership reg) =
  pp_loc_node state "SetMem" node.region;
  pp_expr (state#pad 2 0) node.value.set;
  pp_expr (state#pad 2 1) node.value.element

and pp_set_injection state (node : expr injection reg) =
  pp_loc_node  state "SetInj" node.region;
  pp_injection state pp_expr node.value

and pp_e_logic state = function
  BoolExpr e ->
    pp_node state "BoolExpr";
    pp_bool_expr (state#pad 1 0) e
| CompExpr e ->
    pp_node state "CompExpr";
    pp_comp_expr (state#pad 1 0) e

and pp_bool_expr state = function
  Or    e -> pp_op2      state "Or"    e
| And   e -> pp_op2      state "And"   e
| Not   e -> pp_op1      state "Not"   e
| False e -> pp_loc_node state "False" e
| True  e -> pp_loc_node state "True"  e

and pp_comp_expr state = function
  Lt    e -> pp_op2 state "Lt"    e
| Leq   e -> pp_op2 state "Leq"   e
| Gt    e -> pp_op2 state "Gt"    e
| Geq   e -> pp_op2 state "Geq"   e
| Equal e -> pp_op2 state "Equal" e
| Neq   e -> pp_op2 state "Neq"   e

and pp_ctor_expr state (node : ctor_expr) =
  pp_node      state "ECtor";
  let state = state#pad 1 0 in
  match node with
    NoneExpr e -> pp_loc_node state "NoneExpr" e
  | SomeApp e -> pp_some_app state e
  | CtorApp e -> pp_ctor_app state e

and pp_some_app state (node : (kwd_Some * arguments) reg) =
  let _, args = node.value in
  pp_loc_node   state "SomeApp" node.region;
  pp_tuple_expr state args

and pp_ctor_app state (node :  (ctor * arguments option) reg) =
  let ctor, args_opt = node.value in
  pp_loc_node state "CtorApp" node.region;
  let state = state#pad 1 0 in
  pp_ident     state ctor;
  print_option state pp_tuple_expr args_opt

and pp_map_expr state = function
  MapLookUp {value; region} ->
    pp_loc_node   state "MapLookUp" region;
    pp_map_lookup state value
| MapInj {value; region}
| BigMapInj {value; region} ->
    pp_loc_node  state "MapInj" region;
    pp_injection state pp_binding value

and pp_tuple_expr state (node : (expr, comma) Utils.nsepseq par reg) =
  let exprs          = Utils.nsepseq_to_list node.value.inside in
  let length         = List.length exprs in
  let apply len rank = pp_expr (state#pad len rank)
  in List.iteri (apply length) exprs

and pp_string_expr state (node : string_expr) =
  pp_node state "EString";
  let state = state#pad 1 0 in
  match node with
    Cat e      -> pp_op2 state "Cat" e
  | String e   -> pp_node   state "String";
                 pp_string (state#pad 1 0) e
  | Verbatim e -> pp_node     state "Verbatim";
                 pp_verbatim (state#pad 1 0) e

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
