(* PRINTING THE CST *)

(* This module produces a textual representation of a subset of the
   Concrete Abstract Tree (CST). It aims at a readable format with the
   most relevant nodes, with source locations. This functionality is
   most useful when testing the parser, for example, checking that a
   particular node corresponding to an operator has the expected
   associativity with the same kind or the expected priority over
   another. *)

[@@@coverage exclude_file]

open CST

module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils
open! Region

let sprintf = Printf.sprintf

let some x = Some x

let (<@) = Utils.(<@)
let swap = Utils.swap
type 'a nseq = 'a Utils.nseq
type ('a, 'sep) nsepseq = ('a, 'sep) Utils.nsepseq

(* STATE *)

(* The printing of the CST makes use of a threaded data structure: the
   _state_. The printing is done to the string buffer bound to the
   field [buffer], which is imperatively updated (see module
   [Stdlib.Buffer].) The method [pad] updates the current padding,
   which is comprised of two components: the padding to reach the new
   node (space before reaching a subtree, then a vertical bar for it)
   and the padding for the new node itself. (Is it the last child of
   its parent?) *)

type state = <
  offsets  : bool;
  mode     : [`Point | `Byte];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

let mk_state ?(buffer=Buffer.create 131) ~offsets mode  =
  object
    method offsets  = offsets;
    method mode     = mode;
    method buffer   = buffer
    val pad_path    = ""
    method pad_path = pad_path
    val pad_node    = ""
    method pad_node = pad_node

    method pad arity rank =
      {< pad_path =
           pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
         pad_node =
           pad_node ^ (if rank = arity-1 then "    " else "|   ") >}
  end

(* The names of the printing functions are all prefixed by
   "print_". The rest of the name is either

     * the name of the type whose value is printed, for example
       [print_declaration], [print_attributes], [print_statements]
       etc.;

     * the name of a token, for example, [print_Int];

TODO: Really?

     * then name of a constructor followed by "_arg", for example,
       "print_E_Ctor_arg" means that the argument of the constructor
       [E_Ctor] is printed, whereas, for example, [print_Int] would
       mean that the constructor itself would be printed, because it
       denotes a token;

     * a generic name, like [print_token] for tokens which are
       keywords or symbols; another example is [print_token_opt] when
       we have an optional keyword. Or higher-order functions like
       [print_injection], [print_option], [print_nsepseq] etc. *)

let compact state (region: Region.t) =
  region#compact ~offsets:state#offsets state#mode

(* Printing the tokens (leaves of the CST) *)

let print_E_String state {value; region} =
  let reg  = compact state region in
  let node = sprintf "%s%S (%s)\n" state#pad_path value reg
  in Buffer.add_string state#buffer node

let print_P_String = print_E_String
let print_T_String = print_E_String

let print_E_Verbatim state {value; region} =
  let reg  = compact state region in
  let node = sprintf "%s{|%s|} (%s)\n" state#pad_path value reg
  in Buffer.add_string state#buffer node

(* Higher-order printers *)

let list_to_option = function
  [] -> None
|  l -> Some l

let print_ident state {value; region} =
  let reg  = compact state region in
  let node = sprintf "%s%s (%s)\n" state#pad_path value reg
  in Buffer.add_string state#buffer node

let print_loc_node state value region = print_ident state {value; region}

let print_node state name =
  let node = sprintf "%s%s\n" state#pad_path name
  in Buffer.add_string state#buffer node

let print_root ?region state label =
  match region with
           None -> print_node state label
  | Some region -> print_loc_node state label region

let print_tree state label print_sub {value; region} =
  print_root state label ~region;
  print_sub  state value

let mk_child print = function
        None -> None
| Some value -> Some (swap print value)

let print_forest state forest =
  let forest = List.filter_map (fun x -> x) forest in
  let arity  = List.length forest in
  let apply rank print = print (state#pad arity rank)
  in List.iteri apply forest

let print_multi state label ?region children =
  print_root   state label ?region;
  print_forest state children

let print_unary state label ?region print_sub node =
  print_multi state label ?region [mk_child print_sub (Some node)]

let print_par state label ?region print_sub (e : 'a par reg) =
  print_unary state label ?region print_sub e.value.inside

let print_brackets state label ?region print_sub (e : 'a brackets reg) =
  print_unary state label ?region print_sub e.value.inside

(* Printing the root of the CST *)

let rec print_cst state (node : cst) =
  print_multi state "<cst>"
  @@ List.map (mk_child print_declaration <@ some)
  @@ Utils.nseq_to_list node.decl

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_declaration state = function
  D_Const    d -> print_D_Const    state d
| D_Fun      d -> print_D_Fun      state d
| D_Module   d -> print_D_Module   state d
| D_ModAlias d -> print_D_ModAlias state d
| D_Type     d -> print_D_Type     state d

and print_type_annot state (node : type_annot) =
  let _, type_expr = node in
  print_unary state "<type>" print_type_expr type_expr

and print_D_Const state (node : const_decl reg) =
  let node = node.value in
  let children = [
    mk_child print_ident      (Some node.name);
    mk_child print_type_annot node.const_type;
    mk_child print_expr       (Some node.init);
    mk_child print_attributes (list_to_option node.attributes)]
  in print_multi state "D_Const" children

and print_parameters state (node : parameters) =
  print_multi state "<parameters>"
  @@ List.map (mk_child print_param_decl <@ some)
  @@ Utils.nsepseq_to_list node.value.inside

and print_ret_type state (node : type_annot) =
  let _, type_expr = node in
  print_unary state "<return type>" print_type_expr type_expr

and print_ret_expr state =
  print_unary state "<return expression>" print_expr

and print_recursive state _ = print_node state "recursive"

and print_D_Fun state (node : fun_decl reg) =
  let node = node.value in
  let children = [
     mk_child print_recursive  node.kwd_recursive;
     mk_child print_ident      (Some node.fun_name);
     mk_child print_parameters (Some node.param);
     mk_child print_ret_type   node.ret_type;
     mk_child print_ret_expr   (Some node.return);
     mk_child print_attributes (list_to_option node.attributes)]
  in print_multi state "D_Fun" children

and print_D_Module state (node : module_decl reg) =
  let node = node.value in
  let children = [
     mk_child print_ident        (Some node.name);
     mk_child print_declarations (Some node.declarations)]
  in print_multi state "D_Module" children

and print_declarations state (node : declaration nseq) =
  print_multi state "<structure>"
  @@ List.map (mk_child print_declaration <@ some)
  @@ Utils.nseq_to_list node

and print_D_ModAlias state (node : module_alias reg) =
  let node = node.value in
  let children = [
    mk_child print_ident    (Some node.alias);
    mk_child print_mod_path (Some node.mod_path)]
  in print_multi state "D_ModAlias" children

and print_mod_path state (node : (module_name, dot) nsepseq) =
  print_multi state "<path>"
  @@ List.map (mk_child print_ident <@ some)
  @@ Utils.nsepseq_to_list node

and print_D_Type state (node : type_decl reg) =
  let node = node.value in
  let print_type_expr state =
    print_unary state "<type>" print_type_expr in
  let children = [
    mk_child print_ident     (Some node.name);
    mk_child print_type_expr (Some node.type_expr)]
  in print_multi state "D_Type" children

and print_type_expr state = function
  T_Ctor    e -> print_T_Ctor    state e
| T_Fun     e -> print_T_Fun     state e
| T_Int     e -> print_multi     state "T_Int" (mk_children_int e)
| T_ModPath e -> print_T_ModPath state e
| T_Par     e -> print_par       state "T_Par" print_type_expr e
| T_Prod    e -> print_T_Prod    state e
| T_Record  e -> print_T_Record  state e
| T_String  e -> print_unary     state "T_String" print_T_String e
| T_Sum     e -> print_T_Sum     state e
| T_Var     e -> print_unary     state "T_Var" print_ident e
| T_Wild    e -> print_unary     state "T_Wild" print_wild e

and print_T_ModPath state (node : type_expr module_path reg) =
  let node = node.value in
  let children = [
    mk_child print_ident     (Some node.module_name);
    mk_child print_type_expr (Some node.field)]
  in print_multi state "T_ModPath" children

and print_T_Record state (node : field_decl reg ne_injection reg) =
  let node = node.value in
  let children =
    (List.map (mk_child print_field_decl <@ some)
     @@ Utils.nsepseq_to_list node.ne_elements)
    @ [mk_child print_attributes (list_to_option node.attributes)]
  in print_multi state "T_Record" children

and print_wild state = print_loc_node state "_"

and print_T_Ctor state (node : (type_ctor * type_tuple) reg) =
  let {value=(name, tuple); region} = node in
  let children = [
    mk_child print_ident      (Some name);
    mk_child print_type_tuple (Some tuple)]
  in print_multi state "T_Ctor" ~region children

and print_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  let {value = (domain, _, codomain); region} = node in
  let children = [
    mk_child print_type_expr (Some domain);
    mk_child print_type_expr (Some codomain)]
  in print_multi state "T_Fun" ~region children

and print_T_Sum state (node : sum_type reg) =
  let node = node.value in
  let children =
    (List.map (fun {value; _} -> mk_child print_variant (Some value))
     @@ Utils.nsepseq_to_list node.variants)
    @ [mk_child print_attributes (list_to_option node.attributes)]
  in print_multi state "T_Sum" children

and print_T_Prod state (node : cartesian) =
  let {value; region} = node in
  print_multi state "T_Prod" ~region
  @@ List.map (mk_child print_type_expr <@ some)
  @@ Utils.nsepseq_to_list value
and print_attributes state =
  print_multi state "<attributes>"
  <@ List.map (mk_child print_ident <@ some)

and print_variant state (node : variant) =
  let {value; region} = node.ctor in
  let children = [
    mk_child print_of_type_expr node.arg;
    mk_child print_attributes   (list_to_option node.attributes)]
  in print_multi state value ~region children

and print_of_type_expr state = print_type_expr state <@ snd

and print_field_decl state (node : field_decl reg) =
  let node = node.value in
  let children = [
    mk_child print_field_type (Some node.field_type);
    mk_child print_attributes (list_to_option node.attributes)]
  in print_ident  state node.field_name;
     print_forest state children

and print_field_type state =
  print_unary state "<type>" print_type_expr

and print_type_tuple
      state (node : (type_expr, comma) nsepseq par reg) =
  let {value; region} = node in
  print_multi state "<arguments>" ~region
  @@ List.map (mk_child print_type_expr <@ some)
  @@ Utils.nsepseq_to_list value.inside

and print_E_Fun state (node : fun_expr reg) =
  let node = node.value in
  let children = [
     mk_child print_parameters (Some node.param);
     mk_child print_ret_type   node.ret_type;
     mk_child print_ret_expr   (Some node.return)]
  in print_multi state "E_Fun" children

and print_E_CodeInj state (node : code_inj reg) =
  let {value; region} = node in
  let children = [
    mk_child print_language (Some value.language.value);
    mk_child print_code     (Some value.code)]
  in print_multi state "E_CodeInj" ~region children

and print_language state =
  print_unary state "<language>" print_E_String

and print_code state = print_unary state "<code>" print_expr

and print_E_Block state (node : block_with reg) =
  let {value; region} = node in
  let children = [
    mk_child print_block      (Some value.block.value);
    mk_child print_block_expr (Some value.expr)]
  in print_multi state "E_Block" ~region children

and print_block state (node : block) =
  print_multi state "<block>"
  @@ List.map (mk_child print_statement <@ some)
  @@ Utils.nsepseq_to_list node.statements

and print_block_expr state =
  print_unary state "<expr>" print_expr

and print_statement state = function
  S_Instr   i -> print_unary state "S_Instr"   print_instruction i
| S_Decl    i -> print_unary state "S_Decl"    print_declaration i
| S_VarDecl i -> print_unary state "S_VarDecl" print_var_decl    i.value

and print_param_decl state = function
  ParamConst d -> print_ParamConst state d
| ParamVar   d -> print_ParamVar   state d

and print_ParamConst state (node : param_const reg) =
  let {value; region} = node in
  let children = [
    mk_child print_ident      (Some value.var);
    mk_child print_type_annot value.param_type]
  in print_multi state "ParamConst" ~region children

and print_ParamVar state (node : param_var reg) =
  let {value; region} = node in
  let children = [
    mk_child print_ident      (Some value.var);
    mk_child print_type_annot value.param_type]
  in print_multi state "ParamVar" ~region children

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_instruction state = function
  I_Assign      e -> print_tree    state "I_Assign" print_assignment e
| I_Call        e -> print_tree    state "I_Call" print_fun_call e
| I_Case        e -> print_I_Case  state e
| I_Cond        e -> print_tree    state "I_Cond" print_cond_instr e
| I_For         e -> print_tree    state "I_For" print_for_int e
| I_Iter        e -> print_tree    state "I_Iter" print_iter e
| I_MapPatch    e -> print_tree    state "I_MapPatch" print_map_patch e
| I_MapRemove   e -> print_tree    state "I_MapRemove" print_map_remove e
| I_RecordPatch e -> print_tree    state "I_RecordPatch" print_record_patch e
| I_Skip        e -> print_root    state ~region:e "I_Skip"
| I_SetPatch    e -> print_tree    state "I_SetPatch" print_set_patch e
| I_SetRemove   e -> print_tree    state "I_SetRemove" print_set_remove e
| I_While       e -> print_I_While state e

and print_I_While state {value; _} =
  print_node state "<while>";
    let () =
      let state = state#pad 2 0 in
      print_unary state "<condition>" print_expr value.cond in
    let () =
      let state = state#pad 2 1 in
      let statements = value.block.value.statements in
      print_node       state "<statements>";
      print_statements state statements
    in ()

and print_statements state statements =
  let statements     = Utils.nsepseq_to_list statements in
  let length         = List.length statements in
  let apply len rank = print_statement (state#pad len rank)
  in List.iteri (apply length) statements

and print_I_Case state =
  let print state = print_case state print_test_clause
  in print_tree state "I_Case" print

and print_cond_expr state (cond : expr conditional) =
  let () =
    let state = state#pad 3 0 in
    print_unary state "<condition>" print_expr cond.test in
  let () =
    let state = state#pad 3 1 in
    print_unary state "<true>" print_expr cond.ifso in
  let () =
    let state = state#pad 3 2 in
    print_unary state "<false>" print_expr cond.ifnot
  in ()

and print_cond_instr state (cond: test_clause conditional) =
  let () =
    let state = state#pad 3 0 in
    print_unary state "<condition>" print_expr cond.test in
  let () =
    let state = state#pad 3 1 in
    print_unary state "<true>" print_test_clause cond.ifso in
  let () =
    let state = state#pad 3 2 in
    print_unary state "<false>" print_test_clause cond.ifnot
  in ()

and print_test_clause state = function
  ClauseInstr instr ->
    print_unary state "ClauseInstr" print_instruction instr
| ClauseBlock block ->
    print_unary state "ClauseBlock" print_clause_block block

and print_clause_block state = function
  LongBlock {value; region} ->
    print_loc_node   state "LongBlock" region;
    print_statements state value.statements
| ShortBlock {value; region} ->
    print_loc_node   state "ShortBlock" region;
    print_statements state (fst value.inside)

and print_case :
  'a.state -> (state -> 'a -> unit) -> 'a case -> unit =
  fun state print case ->
    let clauses = Utils.nsepseq_to_list case.cases.value in
    let clauses = List.map (fun x -> x.value) clauses in
    let length  = List.length clauses + 1 in
    let apply len rank =
      let state = state#pad len (rank+1)
      in print_case_clause state print
    in print_expr (state#pad length 0) case.expr;
    List.iteri (apply length) clauses

and print_case_clause :
  'a.state -> (state -> 'a -> unit) -> 'a case_clause -> unit =
  fun state print clause ->
    print_node    state "<clause>";
    print_pattern (state#pad 2 0) clause.pattern;
    print         (state#pad 2 1) clause.rhs

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_pattern state = function
  P_Bytes b ->
    print_node  state "P_Bytes";
    print_bytes state b
| P_Cons {value; region} ->
    let patterns = Utils.nsepseq_to_list value in
    let length   = List.length patterns in
    let apply len rank =
      print_pattern (state#pad len rank) in
    print_loc_node state "P_Cons" region;
    List.iteri (apply length) patterns
| P_Ctor p ->
    print_unary state "P_Ctor" ~region:p.region print_ctor_app_pattern p
| P_False region ->
    print_loc_node state "P_False" region
| P_Int n ->
    print_node state "P_Int";
    print_int state n
| P_List {value; region} ->
    print_loc_node state "P_List" region;
    print_injection (state#pad 1 0) print_pattern value
| P_Nat n ->
    print_node state "P_Nat";
    print_int state n
| P_Nil region ->
    print_loc_node state "P_Nil" region
| P_None region ->
    print_loc_node state "P_None" region
| P_ParCons p ->
    print_par state "P_ParCons" ~region:p.region print_bin_cons p
| P_Some p -> print_P_Some state p
| P_String p -> print_unary state "P_String" print_P_String p
| P_True region ->
    print_loc_node state "P_True" region
| P_Tuple p ->
    print_unary state "P_Tuple" ~region:p.region print_tuple_pattern p
| P_Unit region ->
    print_loc_node state "P_Unit" region
| P_Var p ->
    print_unary state "P_Var" print_ident p
| P_Wild region ->
    print_loc_node state "P_Wild" region

and print_P_Some state (node : (kwd_Some * pattern par reg) reg) =
  let arg = snd node.value in
  print_par state "P_Some" ~region:node.region print_pattern arg

and print_bytes state {value=lexeme,hex; region} =
  print_loc_node (state#pad 2 0) lexeme region;
  print_node     (state#pad 2 1) (Hex.show hex)

and print_int state node =
  print_forest state @@ mk_children_int node
and mk_children_int {value=lexeme,z; region} =
  [mk_child print_ident (Some {value=lexeme; region});
   mk_child print_node  (Some (Z.to_string z))]

and print_ctor_app_pattern state (node : (ctor * tuple_pattern option) reg) =
  let ctor, pat_opt = node.value in
  print_ident state ctor;
  match pat_opt with
      None -> ()
  | Some p -> print_tuple_pattern state p

and print_bin_cons state (head, _, tail) =
  print_pattern (state#pad 2 0) head;
  print_pattern (state#pad 2 1) tail

and print_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection -> unit =
  fun state print inj ->
    let elements       = Utils.sepseq_to_list inj.elements in
    let length         = List.length elements in
    let apply len rank = print (state#pad len rank)
    in List.iteri (apply length) elements

and print_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection -> unit =
  fun state print inj ->
    let ne_elements    = Utils.nsepseq_to_list inj.ne_elements in
    let length         = List.length ne_elements in
    let arity      = if inj.attributes = [] then length else length + 1
    and apply len rank = print (state#pad len rank)
    in List.iteri (apply arity) ne_elements;
       if inj.attributes <> [] then
         let state = state#pad arity (arity-1)
         in print_attributes state inj.attributes

and print_tuple_pattern
      state (node :  (pattern, comma) nsepseq par reg) =
  let node = node.value in
  let patterns       = Utils.nsepseq_to_list node.inside in
  let length         = List.length patterns in
  let apply len rank = print_pattern (state#pad len rank)
  in List.iteri (apply length) patterns

and print_assignment state (node : assignment) =
  print_lhs  (state#pad 2 0) node.lhs;
  print_expr (state#pad 2 1) node.rhs

and print_lhs state = function
  Path p ->
    print_unary state "Path" print_path p
| MapPath {value; region} ->
    print_unary state "MapPath" ~region print_map_lookup value

and print_path state = function
  Name name ->
    print_unary state "Name" print_ident name
| Path {value; region} ->
    print_unary state "Path" ~region print_projection value

and print_projection state proj =
  let selections     = Utils.nsepseq_to_list proj.field_path in
  let len            = List.length selections in
  let apply len rank = print_selection (state#pad len rank) in
  print_ident (state#pad (1+len) 0) proj.struct_name;
  List.iteri (apply len) selections

and print_module_path :
  'a.state -> (state -> 'a -> unit ) -> 'a module_path -> unit =
  fun state print node ->
    print_forest state [mk_child print_ident (Some node.module_name);
                        mk_child print (Some node.field)]

and print_update state update =
  print_path         (state#pad 2 0) update.record;
  print_ne_injection state print_field_path_assignment update.updates.value

and print_selection state = function
  FieldName name -> print_unary state "FieldName" print_ident name
| Component comp ->
    print_node state "Component";
    print_int state comp

and print_E_MapLookUp state {region; value} =
  print_unary state "E_MapLookUp" ~region print_map_lookup value

and print_map_lookup state (node : map_lookup) =
  let print_path state (path : path) =
    print_unary state "<map>" print_path path
  and print_index state (index : expr brackets reg) =
    print_brackets state "<index>" print_expr index in
  print_forest state [mk_child print_path  (Some node.path);
                      mk_child print_index (Some node.index)]

and print_for_int state (node : for_int) =
  let print_init state =
    print_node   state "<init>";
    print_forest state [mk_child print_ident (Some node.binder);
                        mk_child print_expr  (Some node.init)]
  and print_bound state =
    print_node   state "<bound>";
    print_forest state [mk_child print_expr (Some node.bound)]
  and print_step state (_, expr) =
    print_unary state "<step>" print_expr expr
  and print_block state {statements; _} =
    print_node       state "<statements>";
    print_statements state statements in
  let printers = [Some print_init;
                  Some print_bound;
                  mk_child print_step node.step;
                  Some (swap print_block node.block.value)]
  in print_forest state printers

and print_iter state (node : iter) =
  let () =
    let state = state#pad 3 0 in
    match node.bind_to with
      None ->
        print_ident state node.var
    | Some (_, var) ->
        print_var_binding state (node.var, var) in
  let () =
    let state = state#pad 3 1 in
    print_node       state "<collection>";
    print_collection (state#pad 2 0) node.collection;
    print_expr       (state#pad 2 1) node.expr in
  let () =
    let state = state#pad 3 2 in
    let statements = node.block.value.statements in
    print_node       state "<statements>";
    print_statements state statements
  in ()

and print_collection state = function
  `Map  kwd -> print_loc_node state "map"  kwd
| `Set  kwd -> print_loc_node state "set"  kwd
| `List kwd -> print_loc_node state "list" kwd

and print_var_binding state (source, image) =
  print_node  state "<binding>";
  print_ident (state#pad 2 0) source;
  print_ident (state#pad 2 1) image

and print_fun_call state (expr, args) =
  let args           = Utils.nsepseq_to_list args.value.inside in
  let arity          = List.length args in
  let apply len rank = print_expr (state#pad len rank)
  in print_expr (state#pad (1+arity) 0) expr;
     List.iteri (apply arity) args

and print_record_patch state (node : record_patch) =
  print_path         (state#pad 2 0) node.path;
  print_ne_injection state print_field_assignment node.record_inj.value

and print_field_assignment state (node : field_assignment reg) =
  print_node  state "<field assignment>";
  print_ident (state#pad 2 0) node.value.field_name;
  print_expr  (state#pad 2 1) node.value.field_expr

and print_field_path_assignment state (node : field_path_assignment reg) =
  print_node state "<update>";
  print_path (state#pad 2 0) node.value.field_path;
  print_expr (state#pad 2 1) node.value.field_expr

and print_map_patch state (node : map_patch) =
  print_path         (state#pad 2 0) node.path;
  print_ne_injection state print_binding node.map_inj.value

and print_binding state (node : binding reg) =
  print_node state "<binding>";
  print_expr (state#pad 2 0) node.value.source;
  print_expr (state#pad 2 1) node.value.image

and print_set_patch state (node : set_patch) =
  print_path         (state#pad 2 0) node.path;
  print_ne_injection state print_expr node.set_inj.value

and print_map_remove state (node : map_remove) =
  print_expr (state#pad 2 0) node.key;
  print_path (state#pad 2 1) node.map

and print_set_remove state (node : set_remove) =
  print_expr (state#pad 2 0) node.element;
  print_path (state#pad 2 1) node.set

and print_var_decl state (node : var_decl) =
  let arity = if node.var_type = None then 2 else 3
  and rank  = 0 in
  let rank  = print_ident (state#pad arity rank) node.name; rank+1 in
  let rank  = rank (* TODO : print_type_annot (state#pad arity rank) rank node.var_type*)
  in print_expr (state#pad arity rank) node.init

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_expr state = function
  E_Add e   -> print_op2 state "E_Add" e
| E_And   e -> print_op2      state "E_And"   e
| E_Annot e -> print_annot_expr state e
| E_BigMap {value; region} ->
    print_loc_node  state "E_BigMap" region;
    print_injection state print_binding value
| E_Block e -> print_E_Block state e
| E_Bytes b ->
    print_node state "E_Bytes";
    print_bytes state b
| E_Call {value; region} ->
    print_loc_node state "E_Call" region;
    print_fun_call state value
| E_Case  e -> print_expr_case state e
| E_Cat e      -> print_op2 state "E_Cat" e
| E_CodeInj e -> print_E_CodeInj state e
| E_Equal e -> print_op2 state "E_Equal" e
| E_Cond  e -> print_expr_conditional state e
| E_Cons     e -> print_op2       state "E_Cons" e
| E_Ctor e -> print_E_Ctor state e
| E_Div e   -> print_op2 state "E_Div" e
| E_False e -> print_loc_node state "False" e
| E_Fun e -> print_E_Fun state e
| E_Geq   e -> print_op2 state "E_Geq"   e
| E_Gt    e -> print_op2 state "E_Gt"    e
| E_Int e   -> print_node state "Int";
            print_int  state e
| E_Leq   e -> print_op2 state "E_Leq"   e
| E_List e -> print_list_comp state e
| E_Lt    e -> print_op2 state "E_Lt"    e
| E_Map {value; region} ->
    print_loc_node  state "E_Map" region;
    print_injection state print_binding value
| E_MapLookUp e -> print_E_MapLookUp state e
| E_Mod e   -> print_op2 state "E_Mod" e
| E_ModPath e -> print_E_ModPath state e
| E_Mult e  -> print_op2 state "E_Mult" e
| E_Mutez e -> print_node state "Mutez";
            print_int  state e
| E_Nat e   -> print_node state "Nat";
            print_int  state e
| E_Neg e   -> print_op1 state "E_Neg" e
| E_Nil      e -> print_loc_node  state "E_Nil" e
| E_Neq   e -> print_op2 state "E_Neq"   e
| E_None e -> print_loc_node state "E_None" e
| E_Not   e -> print_op1      state "E_Not"   e
| E_Or    e -> print_op2      state "E_Or"    e
| E_Par e -> print_par state "E_Par" ~region:e.region print_expr e
| E_Proj e -> print_proj_expr state e
| E_Record e -> print_record state e
| E_Set e -> print_set_injection state e
| E_SetMem e -> print_set_mem state e
| E_Some e -> print_some_app state e
| E_String e   -> print_unary state "E_String" print_E_String e
| E_Sub e   -> print_op2 state "E_Sub" e
| E_True  e -> print_loc_node state "True"  e
| E_Tuple e_tuple ->
    print_node       state "E_Tuple";
    print_tuple_expr state e_tuple
| E_Unit region ->
    print_loc_node state "E_Unit" region
| E_Update {value; region} ->
    print_loc_node state "E_Update" region;
    print_update   state value
| E_Var e -> print_unary state "E_Var" print_ident e
| E_Verbatim e -> print_unary state "E_Verbatim" print_E_Verbatim e

and print_E_ModPath state =
     print_tree state "E_ModPath"
  @@ swap print_module_path print_expr
and print_proj_expr state (node : projection reg) =
  print_loc_node   state "E_Proj" node.region;
  print_projection state node.value

and print_record state (node : record reg) =
  print_loc_node     state "E_Record" node.region;
  print_ne_injection state print_field_assignment node.value

and print_expr_case state (node : expr case reg) =
  print_loc_node state "E_Case" node.region;
  print_case     state print_expr node.value

and print_expr_conditional state (node : expr conditional reg) =
  print_loc_node  state "E_Cond<" node.region;
  print_cond_expr state node.value

and print_annot_expr state (node : annot_expr par reg) =
  let expr, (_, type_expr) = node.value.inside in
  print_loc_node  state "E_Annot" node.region;
  print_expr      (state#pad 2 0) expr;
  print_type_expr (state#pad 2 1) type_expr

and print_list_comp state (node : expr injection reg) =
  print_loc_node state "E_List" node.region;
  if node.value.elements = None then
    print_node (state#pad 1 0) "[]"
  else print_injection state print_expr node.value

and print_set_mem state (node : set_mem reg) =
  print_loc_node state "SetMem" node.region;
  print_expr (state#pad 2 0) node.value.set;
  print_expr (state#pad 2 1) node.value.element

and print_set_injection state (node : expr injection reg) =
  print_loc_node  state "SetInj" node.region;
  print_injection state print_expr node.value

and print_some_app state (node : (kwd_Some * arguments) reg) =
  let _, args = node.value in
  print_loc_node   state "E_Some" node.region;
  print_tuple_expr state args

and print_E_Ctor state (node :  (ctor * arguments option) reg) =
  let ctor, args_opt = node.value in
  print_loc_node state "E_Ctor" node.region;
  let state = state#pad 1 0 in
  print_ident     state ctor;
  ignore args_opt (* TODO: remove. see below *)
(* TODO   print_option state print_tuple_expr args_opt*)

and print_tuple_expr state (node : (expr, comma) nsepseq par reg) =
  let exprs          = Utils.nsepseq_to_list node.value.inside in
  let length         = List.length exprs in
  let apply len rank = print_expr (state#pad len rank)
  in List.iteri (apply length) exprs

and print_op1 state label {value; region} =
  print_unary state label ~region print_expr value.arg

and print_op2 state label {value; region} =
  print_loc_node state label region;
  print_expr     (state#pad 2 0) value.arg1;
  print_expr     (state#pad 2 1) value.arg2

(*
and print_type_annot state rank = function
         None -> rank
| Some (_, e) -> print_type_expr state e; rank+1
 *)

(* Printing tokens (client-slide) *)

type ('src, 'dst) printer = state -> 'src -> 'dst

let print_to_buffer state cst = print_cst state cst; state#buffer
let print_to_string state cst = print_to_buffer state cst |> Buffer.contents

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
