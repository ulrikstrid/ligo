(* A pretty printer for PascaLIGO *)

[@@@warning "-42-27-26"]

module CST = Cst.Pascaligo
open CST
module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils
open! Region
open! PPrint

(* UTILITY PRINTERS *)

(* Compound structures *)

let print_par : ('a -> document) -> 'a par reg -> document =
  fun print {value; _} ->
    string "(" ^^ nest 1 (print value.inside ^^ string ")")

let print_brackets : ('a -> document) -> 'a brackets reg -> document =
  fun print {value; _} ->
    string "[" ^^ nest 1 (print value.inside ^^ string "]")

(* Guideline: When destructuring a value [v] of type [Region.t], use
   the following order: [let {value; region} = v in ...]

   The following functions are used by others to make strings,
   vertbatim strings, integers and bytes. See for example functions
   [print_T_String], [print_E_Nat], [print_P_Int] etc.

   Note that, contrary to the following utility functions,
   higher-order printers, like [print_call], [print_case],
   [print_conditional] etc. are defined when they are first needed.*)

(* Strings *)

let print_ident {value; _} = string value

let print_string s = string "\"" ^^ print_ident s ^^ string "\""

let print_verbatim s = string "{|" ^^ print_ident s ^^ string "|}"

(* Integers and natural numbers *)

let print_int {value; _} =
  string (Z.to_string (snd value))

let print_nat {value; _} =
  string (Z.to_string (snd value) ^ "n")

(* Bytes *)

let print_bytes {value; _} =
  string ("0x" ^ Hex.show (snd value))

(* Mutez *)

let print_mutez {value; _} =
  Z.to_string (snd value) ^ "mutez" |> string


(* PRINTING THE CST  *)

let rec print (ast : cst) = print_declarations ast.decl

(* DECLARATIONS *)

and print_declarations (decl : declaration Utils.nseq) =
  let app decl = group (print_declaration decl) in
  let decl = Utils.nseq_to_list decl in
  separate_map (hardline ^^ hardline) app decl

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_D_Const] comes before [print_D_Fun]. *)

and print_declaration = function
  D_Const     d -> print_D_Const    d
| D_Directive _ -> empty
| D_Fun       d -> print_D_Fun      d
| D_Module    d -> print_D_Module   d
| D_ModAlias  d -> print_D_ModAlias d
| D_Type      d -> print_D_Type     d

(* Constant declarations *)

and print_D_Const (node : const_decl reg) =
  let {value; _} = node in
  let {name; const_type; init; attributes; _} = value in
  let start = string ("const " ^ name.value) in
  let start = if attributes = [] then start
              else print_attributes attributes ^/^ start in
  let start =
    match const_type with
      None -> start
    | Some (_, e) ->
        group (start ^/^ nest 2 (string ": " ^^ print_type_expr e)) in
  start
  ^^ group (break 1 ^^ nest 2 (string "= " ^^ print_expr init))

(* Function and procedure declarations *)

and print_D_Fun (node : fun_decl reg) =
  let {kwd_recursive; fun_name; param; ret_type;
       return; attributes; _} = node.value in
  let start = print_recursive kwd_recursive in
  let start = if attributes = [] then start
              else print_attributes attributes ^/^ start in
  let start = group (start ^^ group (break 1 ^^ nest 2 (print_ident fun_name)))
  and parameters = print_par print_parameters param
  and t_annot_is =
    match ret_type with
      None -> string " is"
    | Some (_, e) ->
        let ret_type = print_type_expr e in
        group (nest 2 (break 1 ^^ string ": " ^^ nest 2 ret_type
                       ^^ string " is"))
  and body =
    let expr = print_expr return in
    match return with
      E_Block _ -> group (break 1 ^^ expr)
    |         _ -> group (nest 2 (break 1 ^^ expr))
in prefix 2 1 start parameters
   ^^ t_annot_is
   ^^ body

and print_recursive = function
  None   -> string "function"
| Some _ -> string "recursive" ^/^ string "function"

and print_attributes = function
    [] -> empty
| attr -> let make s = string "[@" ^^ string s.value ^^ string "]"
         in separate_map (break 0) make attr

and print_parameters p = print_nsepseq ";" print_param_decl p

and print_param_decl = function
  ParamConst c -> print_param_const c
| ParamVar   v -> print_param_var   v

and print_param_const (node : param_const reg) =
  let {var; param_type; _} : param_const = node.value in
  let name = string ("const " ^ var.value) in
  match param_type with
    None -> name
  | Some (_, e) ->
      prefix 2 1 (name ^^ string " :") (print_type_expr e)

and print_param_var (node : param_var reg) =
  let {var; param_type; _} : param_var = node.value in
  let name   = string ("var " ^ var.value) in
  match param_type with
    None -> name
  | Some (_, e) ->
      prefix 2 1 (name ^^ string " :") (print_type_expr e)

and print_block {value; _} =
  string "block {"
  ^^ nest 2 (hardline ^^ print_statements value.statements)
  ^^ hardline ^^ string "}"

(* Module declaration (by extension) *)

and print_D_Module (node : module_decl reg) =
  let {name; declarations; enclosing; _} = node.value in
  string "module " ^^ print_ident name ^^ string " is {"
  ^^ group (nest 2 (break 1 ^^ print_declarations declarations))
  ^^ string "}"

(* Module aliasing *)

and print_D_ModAlias (node : module_alias reg) =
  let {alias; mod_path; _} = node.value in
  string "module " ^^ string alias.value
  ^^ group (nest 2 (break 1 ^^ print_nsepseq "." print_ident mod_path))

(* Type declarations *)

and print_D_Type (node : type_decl reg) =
  let {name; type_expr; _} = node.value in
  string "type " ^^ print_ident name ^^ string " is"
  ^^ group (nest 2 (break 1 ^^ print_type_expr type_expr))

(* TYPE EXPRESSIONS *)

and print_type_expr = function
  T_Ctor    t -> print_type_app t
| T_Fun     t -> print_fun_type t
| T_Int     t -> print_int t
| T_ModPath t -> print_module_path print_string t
| T_Par     t -> print_type_par t
| T_Prod    t -> print_cartesian t
| T_Record  t -> print_record_type t
| T_String  t -> print_string t
| T_Sum     t -> print_sum_type t
| T_Var     t -> print_ident t
| T_Wild    _ -> string "_"

and print_sum_type {value; _} =
  let {variants; attributes; _} = value in
  let head, tail = variants in
  let head = print_variant head in
  let padding_flat =
    if attributes = [] then empty else string "| " in
  let padding_non_flat =
    if attributes = [] then blank 2 else string "| " in
  let head =
    if tail = [] then head
    else ifflat (padding_flat ^^ head) (padding_non_flat ^^ head) in
  let rest = List.map snd tail in
  let app variant =
    group (break 1 ^^ string "| " ^^ print_variant variant) in
  let whole = head ^^ concat_map app rest in
  if attributes = [] then whole
  else group (print_attributes attributes ^/^ whole)

and print_cartesian {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_type_expr e)
  | e::items ->
      group (break 1 ^^ print_type_expr e ^^ string " *") ^^ app items
  in print_type_expr head ^^ string " *" ^^ app (List.map snd tail)

and print_variant {value; _} =
  let {ctor; arg; attributes=attr} = value in
  let pre = if attr = [] then print_ident ctor
            else group (print_attributes attr ^/^ print_ident ctor) in
  match arg with
    None -> pre
  | Some (_,e) -> prefix 4 1 (pre ^^ string " of") (print_type_expr e)

and print_record_type fields = group (print_ne_injection print_field_decl fields)

and print_field_decl {value; _} =
  let {field_name; field_type; attributes; _} = value in
  let attr = print_attributes attributes in
  let name = if attributes = [] then print_ident field_name
             else attr ^/^ print_ident field_name in
  let t_expr = print_type_expr (snd field_type)
  in prefix 2 1 (group (name ^^ string " :")) t_expr

and print_fun_type {value; _} =
  let lhs, _, rhs = value in
  group (print_type_expr lhs ^^ string " ->" ^/^ print_type_expr rhs)

and print_type_par t = print_par print_type_expr t

and print_type_app {value = ctor, tuple; _} =
  prefix 2 1 (print_type_ctor ctor) (print_type_tuple tuple)

and print_type_ctor ctor = string ctor.value

and print_type_tuple {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_type_expr e)
  | e::items ->
      group (break 1 ^^ print_type_expr e ^^ string ",") ^^ app items in
  let components =
    if tail = []
    then print_type_expr head
    else print_type_expr head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

and print_statements s = print_nsepseq ";" print_statement s

and print_statement = function
  S_Instr s -> print_instruction s
| S_Decl d -> print_declaration d
| S_VarDecl d -> print_var_decl d

and print_var_decl {value; _} =
  let {name; var_type; init; _} = value in
  let start = string ("var " ^ name.value) in
  let start =
    match var_type with
      None -> start
    | Some (_, e) ->
        group (start ^/^ nest 2 (string ": " ^^ print_type_expr e)) in
  start
  ^^ group (break 1 ^^ nest 2 (string ":= " ^^ print_expr init))

and print_instruction = function
  I_Assign      i -> print_assignment i
| I_Call        i -> print_fun_call i
| I_Case        i -> print_case print_if_clause i
| I_Cond        i -> group (print_cond_instr i)
| I_For         i -> print_for_int i
| I_ForIn       i -> print_for_in i
| I_MapPatch    i -> print_map_patch i
| I_MapRemove   i -> print_map_remove i
| I_RecordPatch i -> print_record_patch i
| I_Skip        _ -> string "skip"
| I_SetPatch    i -> print_set_patch i
| I_SetRemove   i -> print_set_remove i
| I_While       i -> print_while_loop i

and print_set_remove {value; _} =
  let {element; set; _} : set_remove = value in
  string "remove" ^^ group (nest 2 (break 1 ^^ print_expr element))
  ^^ group (break 1 ^^ prefix 2 1 (string "from set") (print_path set))

and print_map_remove {value; _} =
  let {key; map; _} = value in
  string "remove" ^^ group (nest 2 (break 1 ^^ print_expr key))
  ^^ group (break 1 ^^ prefix 2 1 (string "from map") (print_path map))

and print_set_patch {value; _} =
  let {path; set_inj; _} = value in
  let inj = print_ne_injection print_expr set_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ print_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and print_map_patch {value; _} =
  let {path; map_inj; _} = value in
  let inj = print_ne_injection print_binding map_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ print_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and print_binding {value; _} =
  let {source; image; _} = value in
  let rhs = group (nest 2 (break 1 ^^ print_expr image))
  in print_expr source ^^ string " ->" ^^ rhs

and print_record_patch {value; _} =
  let {path; record_inj; _} = value in
  let inj = print_record record_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ print_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and print_cond_expr {value; _} =
  let {test; ifso; ifnot; _} : expr conditional = value in
  let test  = string "if "  ^^ group (nest 3 (print_expr test))
  and ifso  = string "then" ^^ group (nest 2 (break 1 ^^ print_expr ifso))
  and ifnot = string "else" ^^ group (nest 2 (break 1 ^^ print_expr ifnot))
  in test ^/^ ifso ^/^ ifnot

and print_cond_instr (node : test_clause conditional reg) =
  let {value; _} = node in
  let {test; ifso; ifnot; _} : test_clause conditional = value in
  let test  = string "if "  ^^ group (nest 3 (print_expr test))
  and ifso  = match ifso with
                ClauseInstr _ ->
                  string "then"
                  ^^ group (nest 2 (break 1 ^^ print_if_clause ifso))
              | ClauseBlock _ ->
                  string "then {"
                  ^^ group (nest 2 (hardline ^^ print_if_clause ifso))
                  ^^ hardline ^^ string "}"
  and ifnot = match ifnot with
                ClauseInstr _ ->
                  string "else"
                  ^^ group (nest 2 (break 1 ^^ print_if_clause ifnot))
              | ClauseBlock _ ->
                  string "else {"
                  ^^ group (nest 2 (hardline ^^ print_if_clause ifnot))
                  ^^ hardline ^^ string "}"
  in test ^/^ ifso ^/^ ifnot

and print_if_clause = function
  ClauseInstr i -> print_instruction i
| ClauseBlock b -> print_block b

and print_set_mem {value; _} =
  let {set; element; _} : set_mem = value in
  group (print_expr set ^/^ string "contains" ^/^ print_expr element)

and print_case : 'a.('a -> document) -> 'a case Region.reg -> document =
  fun print {value; _} ->
    let {expr; cases; _} = value in
    group (string "case " ^^ nest 5 (print_expr expr) ^/^ string "of [")
    ^^ hardline ^^ print_cases print cases
    ^^ hardline ^^ string "]"

and print_cases :
  'a.('a -> document) ->
    ('a case_clause reg, vbar) Utils.nsepseq Region.reg ->
    document =
  fun print {value; _} ->
    let head, tail = value in
    let head       = print_case_clause print head in
    let head       = blank 2 ^^ head in
    let rest       = List.map snd tail in
    let app clause = break 1 ^^ string "| " ^^ print_case_clause print clause
    in  head ^^ concat_map app rest

and print_case_clause :
  'a.('a -> document) -> 'a case_clause Region.reg -> document =
  fun print {value; _} ->
    let {pattern; rhs; _} = value in
    print_pattern pattern ^^ prefix 4 1 (string " ->") (print rhs)

and print_assignment {value; _} =
  let {lhs; rhs; _} = value in
  prefix 2 1 (print_lhs lhs ^^ string " :=") (print_expr rhs)

and print_lhs : lhs -> document = function
  Path p    -> print_path p
| MapPath p -> print_map_lookup p


and print_while_loop {value; _} =
  let {cond; block; _} = value in
  prefix 2 1 (string "while") (print_expr cond) ^^ hardline ^^ print_block block

and print_for_int {value; _} =
  let {binder; init; bound; step; block; _} = value in
  let step =
    match step with
      None -> empty
    | Some (_, e) -> prefix 2 1 (string " step") (print_expr e) in
  let init = prefix 2 1 (print_ident binder ^^ string " :=") (print_expr init) in
  prefix 2 1 (string "for") init
  ^^ prefix 2 1 (string " to") (print_expr bound)
  ^^ step ^^ hardline ^^ print_block block

and print_for_in {value; _} =
  let {var; bind_to; collection; expr; block; _} = value in
  let binding =
    match bind_to with
      None -> print_ident var
    | Some (_, dest) -> print_ident var ^^ string " -> " ^^ print_ident dest in
  prefix 2 1 (string "for") binding
  ^^ prefix 2 1 (string " in") (print_collection collection ^/^ print_expr expr)
  ^^ hardline ^^ print_block block

and print_collection = function
  `Map  _ -> string "map"
| `Set  _ -> string "set"
| `List _ -> string "list"

(* Expressions *)

and print_expr = function
  E_Annot   e -> print_annot_expr e
| E_Int   e -> print_int e
| E_Nat   e -> print_nat e
| E_Mutez e -> print_mutez e
| E_Block   e -> print_block_with e
| E_Bytes   e -> print_bytes e
| E_Call    e -> print_fun_call e
| E_Case    e -> print_case print_expr e
| E_CodeInj e -> print_code_inj e
| E_Cond    e -> group (print_cond_expr e)
| E_Cons    e -> print_bin_op "#" e
| E_Ctor    e -> print_ctor_app e
| E_Fun     e -> print_fun_expr e
| E_List    e -> group (print_injection print_expr e)
| E_Or   e  -> print_bin_op "or" e
| E_And  e  -> print_bin_op "and" e
| E_Not  e  -> print_un_op "not" e
| E_True  _ -> string "True"
| E_False _ -> string "False"
| E_Lt    e -> print_bin_op "<"  e
| E_Leq   e -> print_bin_op "<=" e
| E_Gt    e -> print_bin_op ">"  e
| E_Geq   e -> print_bin_op ">=" e
| E_Equal e -> print_bin_op "="  e
| E_Neq   e -> print_bin_op "=/=" e
| E_Set inj -> print_injection print_expr inj
| E_SetMem mem -> print_set_mem mem
| E_MapLookUp fetch -> print_map_lookup fetch
| E_Map inj      -> print_injection print_binding inj
| E_BigMap inj   -> print_injection print_binding inj
| E_ModPath e -> print_module_path print_expr e
| E_Nil      _ -> string "nil"
| E_None  _ -> string "None"
| E_Par     e -> print_par print_expr e
| E_Proj    e -> print_projection e
| E_Record  e -> print_record e
| E_Some   e -> print_some_app e
| E_Cat      e -> print_bin_op "^" e
| E_String   e -> print_string e
| E_Verbatim e -> print_verbatim e
| E_Tuple   e -> print_tuple_expr e
| E_Unit    _ -> string "Unit"
| E_Update  e -> print_update e
| E_Var     e -> print_ident e
| E_Add   e -> print_bin_op "+" e
| E_Sub   e -> print_bin_op "-" e
| E_Mult  e -> print_bin_op "*" e
| E_Div   e -> print_bin_op "/" e
| E_Mod   e -> print_bin_op "mod" e
| E_Neg   e -> string "-" ^^ print_expr e.value.arg

and print_fun_expr {value; _} =
  let {param; ret_type; return; _} : fun_expr = value in
  let start      = string "function" in
  let parameters = print_par print_parameters param in
  let t_annot    =
    match ret_type with
      None -> empty
    | Some (_, e) ->
        group (break 1 ^^ nest 2 (string ": " ^^ print_type_expr e)) in
  group (start ^^ nest 2 (break 1 ^^ parameters))
  ^^ t_annot
  ^^ string " is" ^^ group (nest 4 (break 1 ^^ print_expr return))

and print_block_with {value; _} =
  let {block; kwd_with; expr} = value in
  let expr = print_expr value.expr in
  group (print_block block ^^ string " with"
         ^^ group (nest 4 (break 1 ^^ expr)))

and print_annot_expr {value; _} =
  let expr, (_, type_expr) = value.inside in
  group (string "("
         ^^ nest 1 (print_expr expr ^/^ string ": "
                    ^^ print_type_expr type_expr ^^ string ")"))

and print_map_lookup {value; _} =
  prefix 2 1 (print_path value.path) (print_brackets print_expr value.index)

and print_path = function
  Name p -> print_ident p
| Path p -> print_projection p

and print_bin_op op {value; _} =
  let {arg1; arg2; _} = value
  and length = String.length op + 1 in
  group (print_expr arg1 ^/^ string (op ^ " ")
         ^^ nest length (print_expr arg2))

and print_un_op op {value; _} =
  group (string (op ^ " ") ^^ print_expr value.arg)

and print_some_app {value; _} =
  prefix 4 1 (string "Some") (print_arguments (snd value))

and print_ctor_app {value; _} =
  let ctor, args = value in
  let ctor = string ctor.value in
  match args with
          None -> ctor
  | Some tuple -> prefix 2 1 ctor (print_tuple_expr tuple)

and print_field_assign {value; _} =
  let {field_name; field_expr; _} = value in
  prefix 2 1 (print_ident field_name ^^ string " =") (print_expr field_expr)

and print_record ne_inj = group (print_ne_injection print_field_assign ne_inj)

and print_projection {value; _} =
  let {struct_name; field_path; _} = value in
  let fields = Utils.nsepseq_to_list field_path
  and sep    = string "." ^^ break 0 in
  let fields = separate_map sep print_selection fields in
  group (print_ident struct_name ^^ string "." ^^ break 0 ^^ fields)

and print_module_path :
  'a.('a -> document) -> 'a module_path reg -> document =
  fun print {value; _} ->
  let {module_path; field; _} = value in
  let path = print_nsepseq "." print_ident module_path
  in group (path ^^ string "." ^^ break 0 ^^ print field)

and print_update {value; _} =
  let {record; updates; _} = value in
  let updates = group (print_ne_injection print_field_path_assign updates)
  and record  = print_path record in
  record ^^ string " with" ^^ nest 2 (break 1 ^^ updates)

and print_code_inj {value; _} =
  let {language; code; _} = value in
  let language = string language.value.value
  and code     = print_expr code in
  string "[%" ^^ language ^/^ code ^^ string "]"

and print_field_path_assign {value; _} =
  let {field_path; field_expr; _} = value in
  let path = print_path field_path in
  prefix 2 1 (path ^^ string " =") (print_expr field_expr)

and print_selection = function
  FieldName v   -> string v.value
| Component cmp -> cmp.value |> snd |> Z.to_string |> string

and print_tuple_expr {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_expr e)
  | e::items ->
      group (break 1 ^^ print_expr e ^^ string ",") ^^ app items in
  let components =
    if   tail = []
    then print_expr head
    else print_expr head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

and print_fun_call {value; _} =
  let lambda, arguments = value in
  let arguments = print_tuple_expr arguments in
  group (print_expr lambda ^^ nest 2 (break 1 ^^ arguments))

and print_arguments v = print_tuple_expr v

(* Injections *)

and print_injection :
  'a.('a -> document) -> 'a injection reg -> document =
  fun print {value; _} ->
    let {kind; elements; _} = value in
    let sep      = string ";" ^^ break 1 in
    let elements = Utils.sepseq_to_list elements in
    let elements = separate_map sep print elements in
    let kwd      = print_injection_kwd kind in
    group (string (kwd ^ " [")
           ^^ nest 2 (break 0 ^^ elements) ^^ break 0 ^^ string "]")

and print_injection_kwd = function
  `Set    _ -> "set"
| `Map    _ -> "map"
| `BigMap _ -> "big_map"
| `List   _ -> "list"

and print_ne_injection :
  'a.('a -> document) -> 'a ne_injection reg -> document =
  fun print {value; _} ->
    let {kind; ne_elements; attributes; _} = value in
    let elements = print_nsepseq ";" print ne_elements in
    let kwd      = print_ne_injection_kwd kind in
    let inj      = group (string (kwd ^ " [")
                          ^^ group (nest 2 (break 0 ^^ elements ))
                          ^^ break 0 ^^ string "]") in
    let inj      = if attributes = [] then inj
                   else group (print_attributes attributes ^/^ inj)
    in inj

and print_ne_injection_kwd = function
  `Set    _ -> "set"
| `Map    _ -> "map"
| `Record _ -> "record"

and print_nsepseq :
  'a.string -> ('a -> document) -> ('a, t) Utils.nsepseq -> document =
  fun sep print elements ->
    let elems = Utils.nsepseq_to_list elements
    and sep   = string sep ^^ break 1
    in separate_map sep print elems

(* Patterns *)

and print_pattern = function
  P_Bytes   p -> print_bytes p
| P_Cons    p -> nest 4 (print_nsepseq " #" print_pattern p.value)
| P_Ctor    p -> print_pctor_app p
| P_False   _ -> string "False"
| P_Int     p -> print_int p
| P_List    p -> print_injection print_pattern p
| P_Nat     p -> print_nat p
| P_Nil     _ -> string "nil"
| P_None    _ -> string "None"
| P_Par     p -> print_par print_pattern p
| P_Some    p -> print_psome p
| P_String  p -> print_string p
| P_True    _ -> string "True"
| P_Tuple   p -> print_tuple_pattern p
| P_Unit    _ -> string "Unit"
| P_Var     p -> print_ident p

and print_psome {value=_, p; _} =
  prefix 4 1 (string "Some") (print_par print_pattern p)

and print_pctor_app {value; _} =
  match value with
    ctor, None -> print_ident ctor
  | ctor, Some ptuple ->
      prefix 4 1 (print_ident ctor) (print_tuple_pattern ptuple)

and print_tuple_pattern {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_pattern e)
  | e::items ->
      group (break 1 ^^ print_pattern e ^^ string ",") ^^ app items in
  let components =
    if   tail = []
    then print_pattern head
    else print_pattern head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

type cst        = Cst.Pascaligo.t
type expr       = Cst.Pascaligo.expr
type type_expr  = Cst.Pascaligo.type_expr
type pattern    = Cst.Pascaligo.pattern
