[@@@warning "-42"]
[@@@warning "-27"]
[@@@warning "-26"]

module CST = Cst.Pascaligo
open CST
module Region = Simple_utils.Region
open! Region
open! PPrint

let pp_par : ('a -> document) -> 'a par reg -> document =
  fun printer {value; _} ->
    string "(" ^^ nest 1 (printer value.inside ^^ string ")")

let pp_brackets : ('a -> document) -> 'a brackets reg -> document =
  fun printer {value; _} ->
    string "[" ^^ nest 1 (printer value.inside ^^ string "]")

let pp_braces : ('a -> document) -> 'a braces reg -> document =
  fun printer {value; _} ->
    string "{" ^^ nest 1 (printer value.inside ^^ string "}")

let pp_option : ('a -> document) -> 'a option -> document =
  fun printer -> function
    None -> empty
  | Some opt -> printer opt

let rec print ast =
  let app decl = group (pp_declaration decl) in
  let decl = Utils.nseq_to_list ast.decl in
  separate_map (hardline ^^ hardline) app decl

and pp_declaration = function
  TypeDecl  d -> pp_type_decl  d
| ConstDecl d -> pp_const_decl d
| FunDecl   d -> pp_fun_decl   d
| AttrDecl  d -> pp_attr_decl  d

and pp_attr_decl decl = pp_ne_injection pp_string decl

and pp_const_decl {value; _} =
  let {name; const_type; init; attributes; _} = value in
  let attr  = match attributes with
                None -> empty
              | Some a -> hardline ^^ pp_attr_decl a in
  let start = string ("const " ^ name.value) in
  let start =
    match const_type with
      None -> start
    | Some (_, e) ->
        group (start ^/^ nest 2 (string ": " ^^ pp_type_expr e)) in
  start
  ^^ group (break 1 ^^ nest 2 (string "= " ^^ pp_expr init))
  ^^ attr

(* Type declarations *)

and pp_type_decl decl =
  let {name; type_expr; _} = decl.value in
  string "type " ^^ string name.value ^^ string " is"
  ^^ group (nest 2 (break 1 ^^ pp_type_expr type_expr))

and pp_type_expr = function
  TProd t   -> pp_cartesian t
| TSum t    -> pp_variants t
| TRecord t -> pp_fields t
| TApp t    -> pp_type_app t
| TFun t    -> pp_fun_type t
| TPar t    -> pp_type_par t
| TVar t    -> pp_ident t
| TString s -> pp_string s

and pp_cartesian {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_type_expr e)
  | e::items ->
      group (break 1 ^^ pp_type_expr e ^^ string " *") ^^ app items
  in pp_type_expr head ^^ string " *" ^^ app (List.map snd tail)

and pp_variants {value; _} =
  let head, tail = value in
  let head = pp_variant head in
  let head = if tail = [] then head
             else ifflat head (string "  " ^^ head) in
  let rest = List.map snd tail in
  let app variant = break 1 ^^ string "| " ^^ pp_variant variant
  in head ^^ concat_map app rest

and pp_variant {value; _} =
  let {constr; arg} = value in
  match arg with
    None -> pp_ident constr
  | Some (_, e) ->
      prefix 4 1 (pp_ident constr ^^ string " of") (pp_type_expr e)

and pp_fields fields = pp_ne_injection pp_field_decl fields

and pp_field_decl {value; _} =
  let {field_name; field_type; _} = value in
  let name   = pp_ident field_name in
  let t_expr = pp_type_expr field_type
  in prefix 2 1 (name ^^ string " :") t_expr

and pp_fun_type {value; _} =
  let lhs, _, rhs = value in
  group (pp_type_expr lhs ^^ string " ->" ^/^ pp_type_expr rhs)

and pp_type_par t = pp_par pp_type_expr t

and pp_type_app {value = ctor, tuple; _} =
  prefix 2 1 (pp_type_constr ctor) (pp_type_tuple tuple)

and pp_type_constr ctor = string ctor.value

and pp_type_tuple {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_type_expr e)
  | e::items ->
      group (break 1 ^^ pp_type_expr e ^^ string ",") ^^ app items in
  let components =
    if tail = []
    then pp_type_expr head
    else pp_type_expr head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

(* Function and procedure declarations *)

and pp_fun_expr {value; _} =
  let {param; ret_type; return; _} : fun_expr = value in
  let start      = string "function" in
  let parameters = pp_par pp_parameters param in
  let t_annot    =
    match ret_type with
      None -> empty
    | Some (_, e) ->
        group (break 1 ^^ nest 2 (string ": " ^^ pp_type_expr e)) in
  group (start ^^ nest 2 (break 1 ^^ parameters))
  ^^ t_annot
  ^^ string " is" ^^ group (nest 4 (break 1 ^^ pp_expr return))

and pp_fun_decl {value; _} =
  let {kwd_recursive; fun_name; param; ret_type;
       block_with; return; attributes; _} = value in
  let start =
    match kwd_recursive with
        None -> string "function"
    | Some _ -> string "recursive" ^/^ string "function" in
  let start = start ^^ group (break 1 ^^ nest 2 (pp_ident fun_name))
  and parameters = pp_par pp_parameters param
  and t_annot_is =
    match ret_type with
      None -> string " is"
    | Some (_, e) ->
        let ret_type = pp_type_expr e in
        group (nest 2 (break 1 ^^ string ": " ^^ nest 2 ret_type
                       ^^ string " is"))
  and body =
    let expr = pp_expr return in
    match block_with with
            None -> group (nest 2 (break 1 ^^ expr))
    | Some (b,_) -> hardline ^^ pp_block b ^^ string " with"
                   ^^ group (nest 4 (break 1 ^^ expr))
  and attr =
    match attributes with
      None -> empty
    | Some a -> hardline ^^ pp_attr_decl a in
  prefix 2 1 start parameters
  ^^ t_annot_is
  ^^ body
  ^^ attr

and pp_parameters p = pp_nsepseq ";" pp_param_decl p

and pp_param_decl = function
  ParamConst c -> pp_param_const c
| ParamVar   v -> pp_param_var v

and pp_param_const {value; _} =
  let {var; param_type; _} : param_const = value in
  let name = string ("const " ^ var.value) in
  match param_type with
    None -> name
  | Some (_, e) ->
      prefix 2 1 (name ^^ string " :") (pp_type_expr e)

and pp_param_var {value; _} =
  let {var; param_type; _} : param_var = value in
  let name   = string ("var " ^ var.value) in
  match param_type with
    None -> name
  | Some (_, e) ->
      prefix 2 1 (name ^^ string " :") (pp_type_expr e)

and pp_block {value; _} =
  string "block {"
  ^^ nest 2 (hardline ^^ pp_statements value.statements)
  ^^ hardline ^^ string "}"

and pp_statements s = pp_nsepseq ";" pp_statement s

and pp_statement = function
  Instr s -> pp_instruction s
| Data  s -> pp_data_decl   s
| Attr  s -> pp_attr_decl   s

and pp_data_decl = function
  LocalConst d -> pp_const_decl d
| LocalVar   d -> pp_var_decl   d
| LocalFun   d -> pp_fun_decl   d

and pp_var_decl {value; _} =
  let {name; var_type; init; _} = value in
  let start = string ("var " ^ name.value) in
  let start =
    match var_type with
      None -> start
    | Some (_, e) ->
        group (start ^/^ nest 2 (string ": " ^^ pp_type_expr e)) in
  start
  ^^ group (break 1 ^^ nest 2 (string ":= " ^^ pp_expr init))

and pp_instruction = function
  Cond        i -> group (pp_conditional i)
| CaseInstr   i -> pp_case pp_if_clause i
| Assign      i -> pp_assignment i
| Loop        i -> pp_loop i
| ProcCall    i -> pp_fun_call i
| Skip        _ -> string "skip"
| RecordPatch i -> pp_record_patch i
| MapPatch    i -> pp_map_patch i
| SetPatch    i -> pp_set_patch i
| MapRemove   i -> pp_map_remove i
| SetRemove   i -> pp_set_remove i

and pp_set_remove {value; _} =
  let {element; set; _} : set_remove = value in
  string "remove" ^^ group (nest 2 (break 1 ^^ pp_expr element))
  ^^ group (break 1 ^^ prefix 2 1 (string "from set") (pp_path set))

and pp_map_remove {value; _} =
  let {key; map; _} = value in
  string "remove" ^^ group (nest 2 (break 1 ^^ pp_expr key))
  ^^ group (break 1 ^^ prefix 2 1 (string "from map") (pp_path map))

and pp_set_patch {value; _} =
  let {path; set_inj; _} = value in
  let inj = pp_ne_injection pp_expr set_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ pp_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and pp_map_patch {value; _} =
  let {path; map_inj; _} = value in
  let inj = pp_ne_injection pp_binding map_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ pp_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and pp_binding {value; _} =
  let {source; image; _} = value in
  pp_expr source
  ^^ string " ->" ^^ group (nest 2 (break 1 ^^ pp_expr image))

and pp_record_patch {value; _} =
  let {path; record_inj; _} = value in
  let inj = pp_record record_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ pp_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and pp_cond_expr {value; _} =
  let {test; ifso; ifnot; _} : cond_expr = value in
  let test  = string "if "  ^^ group (nest 3 (pp_expr test))
  and ifso  = string "then" ^^ group (nest 2 (break 1 ^^ pp_expr ifso))
  and ifnot = string "else" ^^ group (nest 2 (break 1 ^^ pp_expr ifnot))
  in test ^/^ ifso ^/^ ifnot

and pp_conditional {value; _} =
  let {test; ifso; ifnot; _} : conditional = value in
  let test  = string "if "  ^^ group (nest 3 (pp_expr test))
  and ifso  = match ifso with
                ClauseInstr _ | ClauseBlock LongBlock _ ->
                  string "then"
                  ^^ group (nest 2 (break 1 ^^ pp_if_clause ifso))
              | ClauseBlock ShortBlock _ ->
                  string "then {"
                  ^^ group (nest 2 (hardline ^^ pp_if_clause ifso))
                  ^^ hardline ^^ string "}"
  and ifnot = match ifnot with
                ClauseInstr _ | ClauseBlock LongBlock _ ->
                  string "else"
                  ^^ group (nest 2 (break 1 ^^ pp_if_clause ifnot))
              | ClauseBlock ShortBlock _ ->
                  string "else {"
                  ^^ group (nest 2 (hardline ^^ pp_if_clause ifnot))
                  ^^ hardline ^^ string "}"
  in test ^/^ ifso ^/^ ifnot

and pp_if_clause = function
  ClauseInstr i -> pp_instruction i
| ClauseBlock b -> pp_clause_block b

and pp_clause_block = function
  LongBlock b  -> pp_block b
| ShortBlock b -> Utils.(pp_statements <@ fst) b.value.inside

and pp_set_membership {value; _} =
  let {set; element; _} : set_membership = value in
  group (pp_expr set ^/^ string "contains" ^/^ pp_expr element)

and pp_case : 'a.('a -> document) -> 'a case Region.reg -> document =
  fun printer {value; _} ->
    let {expr; cases; _} = value in
    group (string "case " ^^ nest 5 (pp_expr expr) ^/^ string "of [")
    ^^ hardline ^^ pp_cases printer cases
    ^^ hardline ^^ string "]"

and pp_cases :
  'a.('a -> document) ->
    ('a case_clause reg, vbar) Utils.nsepseq Region.reg ->
    document =
  fun printer {value; _} ->
    let head, tail = value in
    let head       = pp_case_clause printer head in
    let head       = blank 2 ^^ head in
    let rest       = List.map snd tail in
    let app clause = break 1 ^^ string "| " ^^ pp_case_clause printer clause
    in  head ^^ concat_map app rest

and pp_case_clause :
  'a.('a -> document) -> 'a case_clause Region.reg -> document =
  fun printer {value; _} ->
    let {pattern; rhs; _} = value in
    pp_pattern pattern ^^ prefix 4 1 (string " ->") (printer rhs)

and pp_assignment {value; _} =
  let {lhs; rhs; _} = value in
  prefix 2 1 (pp_lhs lhs ^^ string " :=") (pp_expr rhs)

and pp_lhs : lhs -> document = function
  Path p    -> pp_path p
| MapPath p -> pp_map_lookup p

and pp_loop = function
  While l -> pp_while_loop l
| For f   -> pp_for_loop f

and pp_while_loop {value; _} =
  let {cond; block; _} = value in
  prefix 2 1 (string "while") (pp_expr cond) ^^ hardline ^^ pp_block block

and pp_for_loop = function
  ForInt l     -> pp_for_int l
| ForCollect l -> pp_for_collect l

and pp_for_int {value; _} =
  let {binder; init; bound; step; block; _} = value in
  let step =
    match step with
      None -> empty
    | Some (_, e) -> prefix 2 1 (string " step") (pp_expr e) in
  prefix 2 1 (string "for") (prefix 2 1 (pp_ident binder ^^ string " :=") (pp_expr init))
  ^^ prefix 2 1 (string " to") (pp_expr bound)
  ^^ step ^^ hardline ^^ pp_block block

and pp_for_collect {value; _} =
  let {var; bind_to; collection; expr; block; _} = value in
  let binding =
    match bind_to with
      None -> pp_ident var
    | Some (_, dest) -> pp_ident var ^^ string " -> " ^^ pp_ident dest in
  prefix 2 1 (string "for") binding
  ^^ prefix 2 1 (string " in") (pp_collection collection ^/^ pp_expr expr)
  ^^ hardline ^^ pp_block block

and pp_collection = function
  Map  _ -> string "map"
| Set  _ -> string "set"
| List _ -> string "list"

(* Expressions *)

and pp_expr = function
  ECase       e -> pp_case pp_expr e
| ECond       e -> group (pp_cond_expr e)
| EAnnot      e -> pp_annot_expr e
| ELogic      e -> group (pp_logic_expr e)
| EArith      e -> group (pp_arith_expr e)
| EString     e -> pp_string_expr e
| EList       e -> group (pp_list_expr e)
| ESet        e -> pp_set_expr e
| EConstr     e -> pp_constr_expr e
| ERecord     e -> pp_record e
| EProj       e -> pp_projection e
| EUpdate     e -> pp_update e
| EMap        e -> pp_map_expr e
| EVar        e -> pp_ident e
| ECall       e -> pp_fun_call e
| EBytes      e -> pp_bytes e
| EUnit       _ -> string "Unit"
| ETuple      e -> pp_tuple_expr e
| EPar        e -> pp_par pp_expr e
| EFun        e -> pp_fun_expr e
| ECodeInj    e -> pp_code_inj e

and pp_annot_expr {value; _} =
  let expr, _, type_expr = value.inside in
  group (string "(" ^^ nest 1 (pp_expr expr ^/^ string ": "
                               ^^ pp_type_expr type_expr ^^ string ")"))

and pp_set_expr = function
  SetInj inj -> pp_injection pp_expr inj
| SetMem mem -> pp_set_membership mem

and pp_map_expr = function
  MapLookUp fetch -> pp_map_lookup fetch
| MapInj inj      -> pp_injection pp_binding inj
| BigMapInj inj   -> pp_injection pp_binding inj

and pp_map_lookup {value; _} =
  prefix 2 1 (pp_path value.path) (pp_brackets pp_expr value.index)

and pp_path = function
  Name v -> pp_ident v
| Path p -> pp_projection p

and pp_logic_expr = function
  BoolExpr e -> pp_bool_expr e
| CompExpr e -> pp_comp_expr e

and pp_bool_expr = function
  Or   e  -> pp_bin_op "or" e
| And  e  -> pp_bin_op "and" e
| Not  e  -> pp_un_op "not" e
| True  _ -> string "True"
| False _ -> string "False"

and pp_bin_op op {value; _} =
  let {arg1; arg2; _} = value
  and length = String.length op + 1 in
  pp_expr arg1 ^/^ string (op ^ " ") ^^ nest length (pp_expr arg2)

and pp_un_op op {value; _} =
  string (op ^ " ") ^^ pp_expr value.arg

and pp_comp_expr = function
  Lt    e -> pp_bin_op "<"  e
| Leq   e -> pp_bin_op "<=" e
| Gt    e -> pp_bin_op ">"  e
| Geq   e -> pp_bin_op ">=" e
| Equal e -> pp_bin_op "="  e
| Neq   e -> pp_bin_op "=/=" e

and pp_arith_expr = function
  Add   e -> pp_bin_op "+" e
| Sub   e -> pp_bin_op "-" e
| Mult  e -> pp_bin_op "*" e
| Div   e -> pp_bin_op "/" e
| Mod   e -> pp_bin_op "mod" e
| Neg   e -> string "-" ^^ pp_expr e.value.arg
| Int   e -> pp_int e
| Nat   e -> pp_nat e
| Mutez e -> pp_mutez e

and pp_mutez {value; _} =
  Z.to_string (snd value) ^ "mutez" |> string

and pp_string_expr = function
  Cat      e -> pp_bin_op "^" e
| String   e -> pp_string e
| Verbatim e -> pp_verbatim e

and pp_ident {value; _} = string value

and pp_string s = string "\"" ^^ pp_ident s ^^ string "\""

and pp_verbatim s = string "{|" ^^ pp_ident s ^^ string "|}"

and pp_list_expr = function
  ECons     e -> pp_bin_op "#" e
| EListComp e -> pp_injection pp_expr e
| ENil      _ -> string "nil"

and pp_constr_expr = function
  SomeApp   a -> pp_some_app a
| NoneExpr  _ -> string "None"
| ConstrApp a -> pp_constr_app a

and pp_some_app {value; _} =
  prefix 4 1 (string "Some") (pp_arguments (snd value))

and pp_constr_app {value; _} =
  let constr, args = value in
  let constr = string constr.value in
  match args with
          None -> constr
  | Some tuple -> prefix 2 1 constr (pp_tuple_expr tuple)


and pp_field_assign {value; _} =
  let {field_name; field_expr; _} = value in
  prefix 2 1 (pp_ident field_name ^^ string " =") (pp_expr field_expr)

and pp_record ne_inj = group (pp_ne_injection pp_field_assign ne_inj)

and pp_projection {value; _} =
  let {struct_name; field_path; _} = value in
  let fields = Utils.nsepseq_to_list field_path
  and sep    = string "." ^^ break 0 in
  let fields = separate_map sep pp_selection fields in
  group (pp_ident struct_name ^^ string "." ^^ break 0 ^^ fields)

and pp_update {value; _} =
  let {record; updates; _} = value in
  let updates = group (pp_ne_injection pp_field_path_assign updates)
  and record  = pp_path record in
  record ^^ string " with" ^^ nest 2 (break 1 ^^ updates)

and pp_code_inj {value; _} =
  let {language; code; _} = value in
  let language = pp_string language.value
  and code     = pp_expr code in
  string "[%" ^^ language ^/^ code ^^ string "]"

and pp_field_path_assign {value; _} =
  let {field_path; field_expr; _} = value in
  let path = pp_path field_path in
  prefix 2 1 (path ^^ string " =") (pp_expr field_expr)

and pp_selection = function
  FieldName v   -> string v.value
| Component cmp -> cmp.value |> snd |> Z.to_string |> string

and pp_tuple_expr {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_expr e)
  | e::items ->
      group (break 1 ^^ pp_expr e ^^ string ",") ^^ app items in
  let components =
    if tail = []
    then pp_expr head
    else pp_expr head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

and pp_fun_call {value; _} =
  let lambda, arguments = value in
  let arguments = pp_tuple_expr arguments in
  group (pp_expr lambda ^^ nest 2 (break 1 ^^ arguments))

and pp_arguments v = pp_tuple_expr v

(* Injections *)

and pp_injection :
  'a.('a -> document) -> 'a injection reg -> document =
  fun printer {value; _} ->
    let {kind; elements; _} = value in
    let sep      = string ";" ^^ break 1 in
    let elements = Utils.sepseq_to_list elements in
    let elements = separate_map sep printer elements in
    let kwd      = pp_injection_kwd kind in
    group (string (kwd ^ " [")
           ^^ nest 2 (break 0 ^^ elements) ^^ break 0 ^^ string "]")

and pp_injection_kwd = function
  InjSet    _ -> "set"
| InjMap    _ -> "map"
| InjBigMap _ -> "big_map"
| InjList   _ -> "list"

and pp_ne_injection :
  'a.('a -> document) -> 'a ne_injection reg -> document =
  fun printer {value; _} ->
    let {kind; ne_elements; _} = value in
    let elements = pp_nsepseq ";" printer ne_elements in
    let kwd      = pp_ne_injection_kwd kind in
    group (string (kwd ^ " [")
           ^^ group (nest 2 (break 0 ^^ elements ))
           ^^ break 0 ^^ string "]")

and pp_ne_injection_kwd = function
  NEInjAttr   _ -> "attributes"
| NEInjSet    _ -> "set"
| NEInjMap    _ -> "map"
| NEInjRecord _ -> "record"

and pp_nsepseq :
  'a.string -> ('a -> document) -> ('a, t) Utils.nsepseq -> document =
  fun sep printer elements ->
    let elems = Utils.nsepseq_to_list elements
    and sep   = string sep ^^ break 1
    in separate_map sep printer elems

(* Patterns *)

and pp_pattern = function
  PConstr p -> pp_constr_pattern p
| PVar    v -> pp_ident v
| PWild   _ -> string "_"
| PInt    i -> pp_int i
| PNat    n -> pp_nat n
| PBytes  b -> pp_bytes b
| PString s -> pp_string s
| PList   l -> pp_list_pattern l
| PTuple  t -> pp_tuple_pattern t

and pp_int {value; _} =
  string (Z.to_string (snd value))

and pp_nat {value; _} =
  string (Z.to_string (snd value) ^ "n")

and pp_bytes {value; _} =
  string ("0x" ^ Hex.show (snd value))

and pp_constr_pattern = function
  PUnit      _ -> string "Unit"
| PFalse     _ -> string "False"
| PTrue      _ -> string "True"
| PNone      _ -> string "None"
| PSomeApp   a -> pp_psome a
| PConstrApp a -> pp_pconstr_app a

and pp_psome {value=_, p; _} =
  prefix 4 1 (string "Some") (pp_par pp_pattern p)

and pp_pconstr_app {value; _} =
  match value with
    constr, None -> pp_ident constr
  | constr, Some ptuple ->
      prefix 4 1 (pp_ident constr) (pp_tuple_pattern ptuple)

and pp_tuple_pattern {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_pattern e)
  | e::items ->
      group (break 1 ^^ pp_pattern e ^^ string ",") ^^ app items in
  let components =
    if   tail = []
    then pp_pattern head
    else pp_pattern head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

and pp_list_pattern = function
  PListComp cmp -> pp_list_comp cmp
| PNil _        -> string "nil"
| PParCons p    -> pp_ppar_cons p
| PCons p       -> nest 4 (pp_nsepseq " #" pp_pattern p.value)

and pp_list_comp e = pp_injection pp_pattern e

and pp_ppar_cons {value; _} =
  let patt1, _, patt2 = value.inside in
  let comp = prefix 2 1 (pp_pattern patt1 ^^ string " ::") (pp_pattern patt2)
  in string "(" ^^ nest 1 (comp ^^ string ")")