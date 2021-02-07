(* A pretty printer for PascaLIGO *)

[@@@warning "-42-27-26"]

module CST = Cst.Pascaligo
open CST
module Region = Simple_utils.Region
open! Region
open! PPrint

let pp_par : ('a -> document) -> 'a par reg -> document =
  fun print {value; _} ->
    string "(" ^^ nest 1 (print value.inside ^^ string ")")

let pp_brackets : ('a -> document) -> 'a brackets reg -> document =
  fun print {value; _} ->
    string "[" ^^ nest 1 (print value.inside ^^ string "]")

(*
let pp_braces : ('a -> document) -> 'a braces reg -> document =
  fun print {value; _} ->
    string "{" ^^ nest 1 (print value.inside ^^ string "}")
 *)

let rec print ast =
  pp_declarations ast.decl

and pp_declarations decl =
  let app decl = group (pp_declaration decl) in
  let decl = Utils.nseq_to_list decl in
  separate_map (hardline ^^ hardline) app decl

and pp_declaration = function
  D_Type    d -> pp_type_decl    d
| D_Const   d -> pp_const_decl   d
| D_Fun     d -> pp_fun_decl     d
| D_Module  d -> pp_module_decl  d
| D_ModAlias d -> pp_module_alias d

and pp_const_decl {value; _} =
  let {name; const_type; init; attributes; _} = value in
  let start = string ("const " ^ name.value) in
  let start = if attributes = [] then start
              else pp_attributes attributes ^/^ start in
  let start =
    match const_type with
      None -> start
    | Some (_, e) ->
        group (start ^/^ nest 2 (string ": " ^^ pp_type_expr e)) in
  start
  ^^ group (break 1 ^^ nest 2 (string "= " ^^ pp_expr init))

(* Type declarations *)

and pp_type_decl decl =
  let {name; type_expr; _} = decl.value in
  string "type " ^^ pp_ident name ^^ string " is"
  ^^ group (nest 2 (break 1 ^^ pp_type_expr type_expr))

and pp_module_decl decl =
  let {name; declarations; enclosing; _} = decl.value in
  string "module " ^^ pp_ident name ^^ string " is {"
  ^^ group (nest 2 (break 1 ^^ pp_declarations declarations))
  ^^ string "}"

and pp_module_alias decl =
  let {alias; mod_path; _} = decl.value in
  string "module " ^^ string alias.value
  ^^ group (nest 2 (break 1 ^^ pp_nsepseq "." pp_ident mod_path))

and pp_type_expr = function
  T_Ctor    t -> pp_type_app t
| T_Fun     t -> pp_fun_type t
| T_Int     t -> pp_int t
| T_ModPath t -> pp_module_path pp_type_expr t
| T_Par     t -> pp_type_par t
| T_Prod    t -> pp_cartesian t
| T_Record  t -> pp_record_type t
| T_String  t -> pp_string t
| T_Sum     t -> pp_sum_type t
| T_Var     t -> pp_ident t
| T_Wild    _ -> string "_"

and pp_sum_type {value; _} =
  let {variants; attributes; _} = value in
  let head, tail = variants in
  let head = pp_variant head in
  let padding_flat =
    if attributes = [] then empty else string "| " in
  let padding_non_flat =
    if attributes = [] then blank 2 else string "| " in
  let head =
    if tail = [] then head
    else ifflat (padding_flat ^^ head) (padding_non_flat ^^ head) in
  let rest = List.map snd tail in
  let app variant =
    group (break 1 ^^ string "| " ^^ pp_variant variant) in
  let whole = head ^^ concat_map app rest in
  if attributes = [] then whole
  else group (pp_attributes attributes ^/^ whole)

and pp_cartesian {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_type_expr e)
  | e::items ->
      group (break 1 ^^ pp_type_expr e ^^ string " *") ^^ app items
  in pp_type_expr head ^^ string " *" ^^ app (List.map snd tail)

and pp_variant {value; _} =
  let {ctor; arg; attributes=attr} = value in
  let pre = if attr = [] then pp_ident ctor
            else group (pp_attributes attr ^/^ pp_ident ctor) in
  match arg with
    None -> pre
  | Some (_,e) -> prefix 4 1 (pre ^^ string " of") (pp_type_expr e)

and pp_attributes = function
    [] -> empty
| attr ->
   let make s = string "[@" ^^ string s.value ^^ string "]"
   in separate_map (break 0) make attr

and pp_record_type fields = group (pp_ne_injection pp_field_decl fields)

and pp_field_decl {value; _} =
  let {field_name; field_type; attributes; _} = value in
  let attr = pp_attributes attributes in
  let name = if attributes = [] then pp_ident field_name
             else attr ^/^ pp_ident field_name in
  let t_expr = pp_type_expr field_type
  in prefix 2 1 (group (name ^^ string " :")) t_expr

and pp_fun_type {value; _} =
  let lhs, _, rhs = value in
  group (pp_type_expr lhs ^^ string " ->" ^/^ pp_type_expr rhs)

and pp_type_par t = pp_par pp_type_expr t

and pp_type_app {value = ctor, tuple; _} =
  prefix 2 1 (pp_type_ctor ctor) (pp_type_tuple tuple)

and pp_type_ctor ctor = string ctor.value

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

and pp_recursive = function
  None   -> string "function"
| Some _ -> string "recursive" ^/^ string "function"

and pp_fun_decl {value; _} =
  let {kwd_recursive; fun_name; param; ret_type;
       return; attributes; _} = value in
  let start = pp_recursive kwd_recursive in
  let start = if attributes = [] then start
              else pp_attributes attributes ^/^ start in
  let start = group (start ^^ group (break 1 ^^ nest 2 (pp_ident fun_name)))
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
    match return with
      E_Block _ -> group (break 1 ^^ expr)
    |         _ -> group (nest 2 (break 1 ^^ expr))
in prefix 2 1 start parameters
   ^^ t_annot_is
   ^^ body

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
  S_Instr s -> pp_instruction s
| S_Decl d -> pp_declaration d
| S_VarDecl d -> pp_var_decl d

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
  I_Assign      i -> pp_assignment i
| I_Call        i -> pp_fun_call i
| I_Case        i -> pp_case pp_if_clause i
| I_Cond        i -> group (pp_cond_instr i)
| I_For         i -> pp_for_int i
| I_Iter        i -> pp_for_collect i
| I_MapPatch    i -> pp_map_patch i
| I_MapRemove   i -> pp_map_remove i
| I_RecordPatch i -> pp_record_patch i
| I_Skip        _ -> string "skip"
| I_SetPatch    i -> pp_set_patch i
| I_SetRemove   i -> pp_set_remove i
| I_While       i -> pp_while_loop i

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
  let rhs = group (nest 2 (break 1 ^^ pp_expr image))
  in pp_expr source ^^ string " ->" ^^ rhs

and pp_record_patch {value; _} =
  let {path; record_inj; _} = value in
  let inj = pp_record record_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ pp_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and pp_cond_expr {value; _} =
  let {test; ifso; ifnot; _} : expr conditional = value in
  let test  = string "if "  ^^ group (nest 3 (pp_expr test))
  and ifso  = string "then" ^^ group (nest 2 (break 1 ^^ pp_expr ifso))
  and ifnot = string "else" ^^ group (nest 2 (break 1 ^^ pp_expr ifnot))
  in test ^/^ ifso ^/^ ifnot

and pp_cond_instr {value; _} =
  let {test; ifso; ifnot; _} : test_clause conditional = value in
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

and pp_set_mem {value; _} =
  let {set; element; _} : set_mem = value in
  group (pp_expr set ^/^ string "contains" ^/^ pp_expr element)

and pp_case : 'a.('a -> document) -> 'a case Region.reg -> document =
  fun print {value; _} ->
    let {expr; cases; _} = value in
    group (string "case " ^^ nest 5 (pp_expr expr) ^/^ string "of [")
    ^^ hardline ^^ pp_cases print cases
    ^^ hardline ^^ string "]"

and pp_cases :
  'a.('a -> document) ->
    ('a case_clause reg, vbar) Utils.nsepseq Region.reg ->
    document =
  fun print {value; _} ->
    let head, tail = value in
    let head       = pp_case_clause print head in
    let head       = blank 2 ^^ head in
    let rest       = List.map snd tail in
    let app clause = break 1 ^^ string "| " ^^ pp_case_clause print clause
    in  head ^^ concat_map app rest

and pp_case_clause :
  'a.('a -> document) -> 'a case_clause Region.reg -> document =
  fun print {value; _} ->
    let {pattern; rhs; _} = value in
    pp_pattern pattern ^^ prefix 4 1 (string " ->") (print rhs)

and pp_assignment {value; _} =
  let {lhs; rhs; _} = value in
  prefix 2 1 (pp_lhs lhs ^^ string " :=") (pp_expr rhs)

and pp_lhs : lhs -> document = function
  Path p    -> pp_path p
| MapPath p -> pp_map_lookup p


and pp_while_loop {value; _} =
  let {cond; block; _} = value in
  prefix 2 1 (string "while") (pp_expr cond) ^^ hardline ^^ pp_block block

and pp_for_int {value; _} =
  let {binder; init; bound; step; block; _} = value in
  let step =
    match step with
      None -> empty
    | Some (_, e) -> prefix 2 1 (string " step") (pp_expr e) in
  let init = prefix 2 1 (pp_ident binder ^^ string " :=") (pp_expr init) in
  prefix 2 1 (string "for") init
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
  `Map  _ -> string "map"
| `Set  _ -> string "set"
| `List _ -> string "list"

(* Expressions *)

and pp_expr = function
  E_Annot   e -> pp_annot_expr e
| E_Int   e -> pp_int e
| E_Nat   e -> pp_nat e
| E_Mutez e -> pp_mutez e
| E_Block   e -> pp_block_with e
| E_Bytes   e -> pp_bytes e
| E_Call    e -> pp_fun_call e
| E_Case    e -> pp_case pp_expr e
| E_CodeInj e -> pp_code_inj e
| E_Cond    e -> group (pp_cond_expr e)
| E_Cons    e -> pp_bin_op "#" e
| E_Ctor    e -> pp_ctor_app e
| E_Fun     e -> pp_fun_expr e
| E_List    e -> group (pp_injection pp_expr e)
| E_Or   e  -> pp_bin_op "or" e
| E_And  e  -> pp_bin_op "and" e
| E_Not  e  -> pp_un_op "not" e
| E_True  _ -> string "True"
| E_False _ -> string "False"
| E_Lt    e -> pp_bin_op "<"  e
| E_Leq   e -> pp_bin_op "<=" e
| E_Gt    e -> pp_bin_op ">"  e
| E_Geq   e -> pp_bin_op ">=" e
| E_Equal e -> pp_bin_op "="  e
| E_Neq   e -> pp_bin_op "=/=" e
| E_Set inj -> pp_injection pp_expr inj
| E_SetMem mem -> pp_set_mem mem
| E_MapLookUp fetch -> pp_map_lookup fetch
| E_Map inj      -> pp_injection pp_binding inj
| E_BigMap inj   -> pp_injection pp_binding inj
| E_ModPath e -> pp_module_path pp_expr e
| E_Nil      _ -> string "nil"
| E_None  _ -> string "None"
| E_Par     e -> pp_par pp_expr e
| E_Proj    e -> pp_projection e
| E_Record  e -> pp_record e
| E_Some   e -> pp_some_app e
| E_Cat      e -> pp_bin_op "^" e
| E_String   e -> pp_string e
| E_Verbatim e -> pp_verbatim e
| E_Tuple   e -> pp_tuple_expr e
| E_Unit    _ -> string "Unit"
| E_Update  e -> pp_update e
| E_Var     e -> pp_ident e
| E_Add   e -> pp_bin_op "+" e
| E_Sub   e -> pp_bin_op "-" e
| E_Mult  e -> pp_bin_op "*" e
| E_Div   e -> pp_bin_op "/" e
| E_Mod   e -> pp_bin_op "mod" e
| E_Neg   e -> string "-" ^^ pp_expr e.value.arg

and pp_block_with {value; _} =
  let {block; kwd_with; expr} = value in
  let expr = pp_expr value.expr in
  group (pp_block block ^^ string " with"
         ^^ group (nest 4 (break 1 ^^ expr)))

and pp_annot_expr {value; _} =
  let expr, (_, type_expr) = value.inside in
  group (string "("
         ^^ nest 1 (pp_expr expr ^/^ string ": "
                    ^^ pp_type_expr type_expr ^^ string ")"))

and pp_map_lookup {value; _} =
  prefix 2 1 (pp_path value.path) (pp_brackets pp_expr value.index)

and pp_path = function
  Name v -> pp_ident v
| Path p -> pp_projection p


and pp_bin_op op {value; _} =
  let {arg1; arg2; _} = value
  and length = String.length op + 1 in
  group (pp_expr arg1 ^/^ string (op ^ " ")
         ^^ nest length (pp_expr arg2))

and pp_un_op op {value; _} =
  group (string (op ^ " ") ^^ pp_expr value.arg)


and pp_mutez {value; _} =
  Z.to_string (snd value) ^ "mutez" |> string

and pp_ident {value; _} = string value

and pp_string s = string "\"" ^^ pp_ident s ^^ string "\""

and pp_verbatim s = string "{|" ^^ pp_ident s ^^ string "|}"


and pp_some_app {value; _} =
  prefix 4 1 (string "Some") (pp_arguments (snd value))

and pp_ctor_app {value; _} =
  let ctor, args = value in
  let ctor = string ctor.value in
  match args with
          None -> ctor
  | Some tuple -> prefix 2 1 ctor (pp_tuple_expr tuple)

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

and pp_module_path :
  'a.('a -> document) -> 'a module_path reg -> document =
  fun print {value; _} ->
    let {module_name; field; _} = value in
    group (pp_ident module_name ^^ string "." ^^ break 0 ^^ print field)

and pp_update {value; _} =
  let {record; updates; _} = value in
  let updates = group (pp_ne_injection pp_field_path_assign updates)
  and record  = pp_path record in
  record ^^ string " with" ^^ nest 2 (break 1 ^^ updates)

and pp_code_inj {value; _} =
  let {language; code; _} = value in
  let language = string language.value.value
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
    if   tail = []
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
  fun print {value; _} ->
    let {kind; elements; _} = value in
    let sep      = string ";" ^^ break 1 in
    let elements = Utils.sepseq_to_list elements in
    let elements = separate_map sep print elements in
    let kwd      = pp_injection_kwd kind in
    group (string (kwd ^ " [")
           ^^ nest 2 (break 0 ^^ elements) ^^ break 0 ^^ string "]")

and pp_injection_kwd = function
  `Set    _ -> "set"
| `Map    _ -> "map"
| `BigMap _ -> "big_map"
| `List   _ -> "list"

and pp_ne_injection :
  'a.('a -> document) -> 'a ne_injection reg -> document =
  fun print {value; _} ->
    let {kind; ne_elements; attributes; _} = value in
    let elements = pp_nsepseq ";" print ne_elements in
    let kwd      = pp_ne_injection_kwd kind in
    let inj      = group (string (kwd ^ " [")
                          ^^ group (nest 2 (break 0 ^^ elements ))
                          ^^ break 0 ^^ string "]") in
    let inj      = if attributes = [] then inj
                   else group (pp_attributes attributes ^/^ inj)
    in inj

and pp_ne_injection_kwd = function
  `Set    _ -> "set"
| `Map    _ -> "map"
| `Record _ -> "record"

and pp_nsepseq :
  'a.string -> ('a -> document) -> ('a, t) Utils.nsepseq -> document =
  fun sep print elements ->
    let elems = Utils.nsepseq_to_list elements
    and sep   = string sep ^^ break 1
    in separate_map sep print elems

(* Patterns *)

and pp_pattern = function
  P_Bytes   p -> pp_bytes p
| P_Cons    p -> nest 4 (pp_nsepseq " #" pp_pattern p.value)
| P_Ctor    p -> pp_pctor_app p
| P_False   _ -> string "False"
| P_Int     p -> pp_int p
| P_List    p -> pp_injection pp_pattern p
| P_Nat     p -> pp_nat p
| P_Nil     _ -> string "nil"
| P_None    _ -> string "None"
| P_ParCons p -> pp_ppar_cons p
| P_Some    p -> pp_psome p
| P_String  p -> pp_string p
| P_True    _ -> string "True"
| P_Tuple   p -> pp_tuple_pattern p
| P_Unit    _ -> string "Unit"
| P_Var     p -> pp_ident p
| P_Wild    _ -> string "_"

and pp_int {value; _} =
  string (Z.to_string (snd value))

and pp_nat {value; _} =
  string (Z.to_string (snd value) ^ "n")

and pp_bytes {value; _} =
  string ("0x" ^ Hex.show (snd value))

and pp_psome {value=_, p; _} =
  prefix 4 1 (string "Some") (pp_par pp_pattern p)

and pp_pctor_app {value; _} =
  match value with
    ctor, None -> pp_ident ctor
  | ctor, Some ptuple ->
      prefix 4 1 (pp_ident ctor) (pp_tuple_pattern ptuple)

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



and pp_ppar_cons {value; _} =
  let patt1, _, patt2 = value.inside in
  let comp = prefix 2 1 (pp_pattern patt1 ^^ string " ::") (pp_pattern patt2)
  in string "(" ^^ nest 1 (comp ^^ string ")")

let print_type_expr = pp_type_expr
let print_pattern   = pp_pattern
let print_expr      = pp_expr

type cst        = Cst.Pascaligo.t
type expr       = Cst.Pascaligo.expr
type type_expr  = Cst.Pascaligo.type_expr
type pattern    = Cst.Pascaligo.pattern
