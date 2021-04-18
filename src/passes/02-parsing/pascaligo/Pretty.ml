(* A pretty printer for PascaLIGO *)

[@@@warning "-42"]
[@@@coverage exclude_file]

(* Dependencies (order matters) *)

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
   higher-order printers, like [print_case], [print_conditional]
   etc. are defined when they are first needed.*)

(* Strings *)

let print_ident   id = string id.value

let print_string   s = string "\"" ^^ print_ident s ^^ string "\""

let print_verbatim s = string "{|" ^^ print_ident s ^^ string "|}"

(* Integers and natural numbers *)

let print_int i = string (Z.to_string (snd i.value))

let print_nat n = string (Z.to_string (snd n.value) ^ "n")

(* Bytes *)

let print_bytes b = string ("0x" ^ Hex.show (snd b.value))

(* Mutez *)

let print_mutez m = Z.to_string (snd m.value) ^ "mutez" |> string


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
  let {name; const_type; init; attributes; _} = node.value in
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
  let start = start ^^ group (break 1 ^^ nest 2 (print_ident fun_name))
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
in prefix 2 1 (group start) parameters ^^ t_annot_is ^^ body

and print_recursive = function
  None   -> string "function"
| Some _ -> string "recursive" ^/^ string "function"

and print_attributes = function
    [] -> empty
| attr -> let make s = string "[@" ^^ string s.value ^^ string "]"
         in separate_map (break 0) make attr

and print_parameters p = print_nsepseq ";" print_param_decl p

and print_nsepseq :
  'a.string -> ('a -> document) -> ('a, t) Utils.nsepseq -> document =
  fun sep print elements ->
    let elems = Utils.nsepseq_to_list elements
    and sep   = string sep ^^ break 1
    in separate_map sep print elems

and print_param_decl = function
  ParamConst c -> print_ParamConst c
| ParamVar   v -> print_ParamVar   v

and print_ParamConst (node : param_const reg) =
  let {var; param_type; _} : param_const = node.value in
  let name = string ("const " ^ var.value) in
  match param_type with
    None -> name
  | Some (_, e) ->
      prefix 2 1 (name ^^ string " :") (print_type_expr e)

and print_ParamVar (node : param_var reg) =
  let {var; param_type; _} : param_var = node.value in
  let name = string ("var " ^ var.value) in
  match param_type with
    None -> name
  | Some (_, e) ->
      prefix 2 1 (name ^^ string " :") (print_type_expr e)

(* Module declaration (by extension) *)

and print_D_Module (node : module_decl reg) =
  let {name; declarations; _} = node.value in
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

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_T_Ctor] comes before [print_T_Fun]. *)

and print_type_expr = function
  T_Ctor    t -> print_T_Ctor    t
| T_Fun     t -> print_T_Fun     t
| T_Int     t -> print_T_Int     t
| T_ModPath t -> print_T_ModPath t
| T_Par     t -> print_T_Par     t
| T_Prod    t -> print_T_Prod    t
| T_Record  t -> print_T_Record  t
| T_String  t -> print_T_String  t
| T_Sum     t -> print_T_Sum     t
| T_Var     t -> print_T_Var     t
| T_Wild    t -> print_T_Wild    t

(* General constructors *)

and print_T_Ctor (node : (type_ctor * type_tuple) reg) =
  let {value = (ctor, tuple); _} = node in
  prefix 2 1 (string ctor.value) (print_type_tuple tuple)

and print_type_tuple (node : type_tuple) =
  let head, tail = node.value.inside in
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

(* Functional types *)

and print_T_Fun (node : (type_expr * arrow * type_expr) reg) =
  let domain, _, codomain = node.value in
  group (print_type_expr domain ^^ string " ->"
         ^/^ print_type_expr codomain)

(* The integer type *)

and print_T_Int = print_int

(* Module paths *)

and print_T_ModPath (node : type_name module_path reg) =
  print_module_path print_string node

and print_module_path :
  'a.('a -> document) -> 'a module_path reg -> document =
  fun print {value; _} ->
  let {module_path; field; _} = value in
  let path = print_nsepseq "." print_ident module_path
  in group (path ^^ string "." ^^ break 0 ^^ print field)

(* Parenthesised type expressions *)

and print_T_Par (node : type_expr par reg) =
  print_par print_type_expr node

(* Product types *)

and print_T_Prod (node: cartesian) =
  let head, tail = node.value in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_type_expr e)
  | e::items ->
      group (break 1 ^^ print_type_expr e ^^ string " *")
      ^^ app items
  in print_type_expr head
     ^^ string " *" ^^ app (List.map snd tail)

(* Record types *)

and print_T_Record (node : field_decl reg ne_injection reg) =
  group (print_ne_injection print_field_decl node)

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

and print_field_decl (node : field_decl reg) =
  let {field_name; field_type; attributes; _} = node.value in
  let attr = print_attributes attributes in
  let name = if attributes = [] then print_ident field_name
             else attr ^/^ print_ident field_name in
  let t_expr = print_type_expr (snd field_type)
  in prefix 2 1 (group (name ^^ string " :")) t_expr

(* The string type *)

and print_T_String node = print_string node

(* Sum types *)

and print_T_Sum (node : sum_type reg) =
  let {variants; attributes; _} = node.value in
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

and print_variant (node : variant reg) =
  let {ctor; arg; attributes=attr} = node.value in
  let pre =
    if attr = [] then print_ident ctor
    else group (print_attributes attr ^/^ print_ident ctor) in
  match arg with
    None -> pre
  | Some (_, e) ->
      prefix 4 1 (pre ^^ string " of") (print_type_expr e)

(* A type variable *)

and print_T_Var node = print_ident node

(* A catch-all variable (a.k.a. joker) *)

and print_T_Wild _ = string "_"


(* STATEMENTS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_S_Instr] comes before [print_S_Decl]. *)

and print_statement = function
  S_Instr   i -> print_S_Instr   i
| S_Decl    i -> print_S_Decl    i
| S_VarDecl i -> print_S_VarDecl i

and print_statements s = print_nsepseq ";" print_statement s

and print_S_Instr node = print_instruction node

and print_S_Decl node = print_declaration node

and print_S_VarDecl {value; _} =
  let {name; var_type; init; _} = value in
  let start = string ("var " ^ name.value) in
  let start =
    match var_type with
      None -> start
    | Some (_, e) ->
        group (start ^/^ nest 2 (string ": " ^^ print_type_expr e)) in
  start
  ^^ group (break 1 ^^ nest 2 (string ":= " ^^ print_expr init))

(* INSTRUCTIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_I_Assign] comes before [print_I_Call]. *)

and print_instruction = function
  I_Assign      i -> print_I_Assign      i
| I_Call        i -> print_I_Call        i
| I_Case        i -> print_I_Case        i
| I_Cond        i -> print_I_Cond        i
| I_For         i -> print_I_For         i
| I_ForIn       i -> print_I_ForIn       i
| I_MapPatch    i -> print_I_MapPatch    i
| I_MapRemove   i -> print_I_MapRemove   i
| I_RecordPatch i -> print_I_RecordPatch i
| I_Skip        i -> print_I_Skip        i
| I_SetPatch    i -> print_I_SetPatch    i
| I_SetRemove   i -> print_I_SetRemove   i
| I_While       i -> print_I_While       i

(* Assignments *)

and print_I_Assign (node : assignment reg) =
  let {lhs; rhs; _} = node.value in
  prefix 2 1 (print_lhs lhs ^^ string " :=") (print_expr rhs)

and print_lhs : lhs -> document = function
  Path p    -> print_path       p
| MapPath p -> print_map_lookup p

and print_path : path -> document = function
  Name p -> print_ident      p
| Path p -> print_projection p

and print_projection {value; _} =
  let {struct_name; field_path; _} = value in
  let fields = Utils.nsepseq_to_list field_path
  and sep    = string "." ^^ break 0 in
  let fields = separate_map sep print_selection fields in
  group (print_ident struct_name ^^ string "." ^^ break 0 ^^ fields)

and print_selection = function
  FieldName v   -> string v.value
| Component cmp -> cmp.value |> snd |> Z.to_string |> string

and print_map_lookup (node : map_lookup reg) =
  prefix 2 1 (print_path node.value.path)
             (print_brackets print_expr node.value.index)

(* Procedure calls *)

and print_I_Call node = print_call node

and print_call (node : call) =
  let lambda, arguments = node.value in
  let arguments = print_arguments arguments in
  group (print_expr lambda ^^ nest 2 (break 1 ^^ arguments))

and print_arguments node = print_tuple_expr node

and print_tuple_expr (node : tuple_expr) =
  let head, tail = node.value.inside in
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

(* Case instructions *)

and print_I_Case node = print_case print_if_clause node

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

(* Conditional instructions *)

and print_I_Cond (node : test_clause conditional reg) =
  let {test; ifso; ifnot; _} = node.value in
  let test = string "if "  ^^ group (nest 3 (print_expr test))
  and ifso =
    match ifso with
      ClauseInstr _ ->
        string "then"
        ^^ group (nest 2 (break 1 ^^ print_if_clause ifso))
    | ClauseBlock _ ->
        string "then {"
        ^^ group (nest 2 (hardline ^^ print_if_clause ifso))
        ^^ hardline ^^ string "}"
  and ifnot =
    match ifnot with
      ClauseInstr _ ->
        string "else"
        ^^ group (nest 2 (break 1 ^^ print_if_clause ifnot))
    | ClauseBlock _ ->
        string "else {"
        ^^ group (nest 2 (hardline ^^ print_if_clause ifnot))
        ^^ hardline ^^ string "}"
  in group (test ^/^ ifso ^/^ ifnot)

and print_if_clause = function
  ClauseInstr i -> print_instruction i
| ClauseBlock b -> print_block b

and print_block (node : block reg) =
  string "block {"
  ^^ nest 2 (hardline ^^ print_statements node.value.statements)
  ^^ hardline ^^ string "}"

(* Bounded iterations on integer intervals (a.k.a. "for loops") *)

and print_I_For (node : for_int reg) =
  let {binder; init; bound; step; block; _} = node.value in
  let step =
    match step with
      None -> empty
    | Some (_, e) -> prefix 2 1 (string " step") (print_expr e) in
  let init =
    prefix 2 1 (print_ident binder ^^ string " :=") (print_expr init) in
  prefix 2 1 (string "for") init
  ^^ prefix 2 1 (string " to") (print_expr bound)
  ^^ step ^^ hardline ^^ print_block block

(* Iterations over collections (maps, sets and lists) *)

and print_I_ForIn (node : for_in reg) =
  let {var; bind_to; collection; expr; block; _} = node.value in
  let binding =
    match bind_to with
      None -> print_ident var
    | Some (_, dest) ->
        print_ident var ^^ string " -> " ^^ print_ident dest in
  prefix 2 1 (string "for") binding
  ^^ prefix 2 1 (string " in")
                (print_collection collection ^/^ print_expr expr)
  ^^ hardline ^^ print_block block

and print_collection = function
  `Map  _ -> string "map"
| `Set  _ -> string "set"
| `List _ -> string "list"

(* Map patches *)

and print_I_MapPatch (node : map_patch reg) =
  let {path; map_inj; _} = node.value in
  let inj = print_ne_injection print_binding map_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ print_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and print_binding {value; _} =
  let {source; image; _} = value in
  let rhs = group (nest 2 (break 1 ^^ print_expr image))
  in print_expr source ^^ string " ->" ^^ rhs

(* Removal of entries in a map *)

and print_I_MapRemove (node : map_remove reg) =
  let {key; map; _} = node.value in
  string "remove" ^^ group (nest 2 (break 1 ^^ print_expr key))
  ^^ group (break 1 ^^ prefix 2 1 (string "from map") (print_path map))

(* Patching records *)

and print_I_RecordPatch (node : record_patch reg) =
  let {path; record_inj; _} = node.value in
  let inj = print_record record_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ print_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and print_record (node : record reg) =
  group (print_ne_injection print_field_assignment node)

and print_field_assignment (node : field_assignment reg) =
  let {field_name; field_expr; _} = node.value in
  prefix 2 1 (print_ident field_name ^^ string " =")
             (print_expr field_expr)

(* Skipping (non-operation) *)

and print_I_Skip _ = string "skip"

(* Patching sets *)

and print_I_SetPatch (node : set_patch reg) =
  let {path; set_inj; _} = node.value in
  let inj = print_ne_injection print_expr set_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ print_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

(* Removal from sets *)

and print_I_SetRemove (node : set_remove reg) =
  let {element; set; _} : set_remove = node.value in
  string "remove"
  ^^ group (nest 2 (break 1 ^^ print_expr element))
  ^^ group (break 1 ^^ prefix 2 1 (string "from set") (print_path set))

(* While loops *)

and print_I_While  (node : while_loop reg) =
  let {cond; block; _} = node.value in
  prefix 2 1 (string "while") (print_expr cond) ^^ hardline
  ^^ print_block block

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_I_Bytes] comes before [print_P_Cons]. *)

and print_pattern = function
  P_Bytes  p -> print_P_Bytes  p
| P_Cons   p -> print_P_Cons   p
| P_Ctor   p -> print_P_Ctor   p
| P_False  p -> print_P_False  p
| P_Int    p -> print_P_Int    p
| P_List   p -> print_P_List   p
| P_Nat    p -> print_P_Nat    p
| P_Nil    p -> print_P_Nil    p
| P_None   p -> print_P_None   p
| P_Par    p -> print_P_Par    p
| P_Some   p -> print_P_Some   p
| P_String p -> print_P_String p
| P_True   p -> print_P_True   p
| P_Tuple  p -> print_P_Tuple  p
| P_Unit   p -> print_P_Unit   p
| P_Var    p -> print_P_Var    p

(* Bytes as literals in patterns *)

and print_P_Bytes node = print_bytes node

(* A series of cons operators in patterns *)

and print_P_Cons (node : (pattern, cons) Utils.nsepseq reg) =
  nest 4 (print_nsepseq " #" print_pattern node.value)

(* A constructor application (or constant constructor) in patterns *)

and print_P_Ctor (node : (ctor * tuple_pattern option) reg) =
  match node.value with
    ctor, None -> print_ident ctor
  | ctor, Some ptuple ->
      prefix 4 1 (print_ident ctor) (print_tuple_pattern ptuple)

and print_tuple_pattern (node : tuple_pattern) =
  let head, tail = node.value.inside in
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

(* The Boolean untruth as a pattern *)

and print_P_False _ = string "False"

(* Integers in patterns *)

and print_P_Int p = print_int p

(* Patterns of lists by extension *)

and print_P_List (node : pattern injection reg) =
  print_injection print_pattern node

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

(* Natural numbers in patterns *)

and print_P_Nat p = print_nat p

(* The pattern for the empty list *)

and print_P_Nil _ = string "nil"

(* The pattern for the predefined constructor [None] *)

and print_P_None _ = string "None"

(* Parenthesised patterns *)

and print_P_Par (node : pattern par reg) =
  print_par print_pattern node

(* The pattern for the application of the predefined constructor
   [Some] *)

and print_P_Some (node : (kwd_Some * pattern par reg) reg) =
  let p = snd node.value in
  prefix 4 1 (string "Some") (print_par print_pattern p)

(* String literals as patterns *)

and print_P_String node = print_string node

(* The Boolean for truth in patterns *)

and print_P_True _ = string "True"

(* The pattern matching a tuple *)

and print_P_Tuple node = print_tuple_pattern node

(* The pattern matching the unique value of the type "unit". *)

and print_P_Unit _ = string "Unit"

(* A pattern variable *)

and print_P_Var node = print_ident node


(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_E_Add] comes before [print_E_And]. *)

and print_expr = function
  E_Add       e -> print_E_Add       e
| E_And       e -> print_E_And       e
| E_Annot     e -> print_E_Annot     e
| E_BigMap    e -> print_E_BigMap    e
| E_Block     e -> print_E_Block     e
| E_Bytes     e -> print_E_Bytes     e
| E_Call      e -> print_E_Call      e
| E_Case      e -> print_E_Case      e
| E_Cat       e -> print_E_Cat       e
| E_CodeInj   e -> print_E_CodeInj   e
| E_Equal     e -> print_E_Equal     e
| E_Cond      e -> print_E_Cond      e
| E_Cons      e -> print_E_Cons      e
| E_Ctor      e -> print_E_Ctor      e
| E_Div       e -> print_E_Div       e
| E_False     e -> print_E_False     e
| E_Fun       e -> print_E_Fun       e
| E_Geq       e -> print_E_Geq       e
| E_Gt        e -> print_E_Gt        e
| E_Int       e -> print_E_Int       e
| E_Leq       e -> print_E_Leq       e
| E_List      e -> print_E_List      e
| E_Lt        e -> print_E_Lt        e
| E_Map       e -> print_E_Map       e
| E_MapLookup e -> print_E_MapLookup e
| E_Mod       e -> print_E_Mod       e
| E_ModPath   e -> print_E_ModPath   e
| E_Mult      e -> print_E_Mult      e
| E_Mutez     e -> print_E_Mutez     e
| E_Nat       e -> print_E_Nat       e
| E_Neg       e -> print_E_Neg       e
| E_Nil       e -> print_E_Nil       e
| E_Neq       e -> print_E_Neq       e
| E_None      e -> print_E_None      e
| E_Not       e -> print_E_Not       e
| E_Or        e -> print_E_Or        e
| E_Par       e -> print_E_Par       e
| E_Proj      e -> print_E_Proj      e
| E_Record    e -> print_E_Record    e
| E_Set       e -> print_E_Set       e
| E_SetMem    e -> print_E_SetMem    e
| E_Some      e -> print_E_Some      e
| E_String    e -> print_E_String    e
| E_Sub       e -> print_E_Sub       e
| E_True      e -> print_E_True      e
| E_Tuple     e -> print_E_Tuple     e
| E_Unit      e -> print_E_Unit      e
| E_Update    e -> print_E_Update    e
| E_Var       e -> print_E_Var       e
| E_Verbatim  e -> print_E_Verbatim  e

(* Arithmetic addition *)

and print_E_Add node = print_op2 "+" node

and print_op2 op (node : Region.t bin_op reg) =
  let {arg1; arg2; _} = node.value
  and length = String.length op + 1 in
  group (print_expr arg1 ^/^ string (op ^ " ")
         ^^ nest length (print_expr arg2))

(* Boolean conjunction *)

and print_E_And node = print_op2 "and" node

(* Expressions annotated with a type *)

and print_E_Annot (node : annot_expr par reg) =
  let expr, (_, type_expr) = node.value.inside in
  group (string "("
         ^^ nest 1 (print_expr expr ^/^ string ": "
                    ^^ print_type_expr type_expr ^^ string ")"))

(* Big maps defined intensionally *)

and print_E_BigMap (node : binding reg injection reg) =
  print_injection print_binding node

(* Block expressions *)

and print_E_Block (node : block_with reg) =
  let {block; expr; _} : block_with = node.value in
  group (print_block block ^^ string " with"
         ^^ group (nest 4 (break 1 ^^ print_expr expr)))

(* Bytes as expressions *)

and print_E_Bytes node = print_bytes node

(* Function calls *)

and print_E_Call node = print_call node

(* Case expressions *)

and print_E_Case node = print_case print_expr node

(* String catenation *)

and print_E_Cat node = print_op2 "^" node

(* Code Injection *)

and print_E_CodeInj (node : code_inj reg) =
  let {language; code; _} = node.value in
  let language = string language.value.value
  and code     = print_expr code in
  string "[%" ^^ language ^/^ code ^^ string "]"

(* Equality *)

and print_E_Equal node = print_op2 "=" node

(* Conditional expressions *)

and print_E_Cond (node : expr conditional reg) =
  let {test; ifso; ifnot; _} : expr conditional = node.value in
  let test  = string "if "  ^^ group (nest 3 (print_expr test))
  and ifso  = string "then" ^^ group (nest 2 (break 1 ^^ print_expr ifso))
  and ifnot = string "else" ^^ group (nest 2 (break 1 ^^ print_expr ifnot))
  in group (test ^/^ ifso ^/^ ifnot)

(* Consing (that is, pushing an item on top of a stack/list *)

and print_E_Cons node = print_op2 "#" node

(* Constructor application (or constant constructor) as expressions *)

and print_E_Ctor (node : (ctor * arguments option) reg) =
  let ctor, args = node.value in
  let ctor = string ctor.value in
  match args with
          None -> ctor
  | Some tuple -> prefix 2 1 ctor (print_tuple_expr tuple)

(* The Euclidean quotient *)

and print_E_Div node = print_op2 "/" node

(* The Boolean untruth *)

and print_E_False _ = string "False"

(* Functional expressions *)

and print_E_Fun (node : fun_expr reg) =
  let {param; ret_type; return; _} : fun_expr = node.value in
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

(* Greater or Equal *)

and print_E_Geq node = print_op2 ">=" node

(* Greater Than *)

and print_E_Gt node = print_op2 ">" node

(* Integer literals as expressions *)

and print_E_Int node = print_int node

(* Lower or Equal *)

and print_E_Leq node = print_op2 "<=" node

(* Lists of expressions defined intensionally *)

and print_E_List (node : expr injection reg) =
  group (print_injection print_expr node)

(* Lower Than *)

and print_E_Lt node = print_op2 "<" node

(* Map expressions defined intensionally (that is, by a series of
   bindings from keys to values. *)

and print_E_Map (node : binding reg injection reg) =
  print_injection print_binding node

(* Map lookup as an expression (denoting the key or a failure) *)

and print_E_MapLookup node = print_map_lookup node

(* Euclidean reminder ("modulo") *)

and print_E_Mod node = print_op2 "mod" node

(* Module path as an expression *)

and print_E_ModPath (node : expr module_path reg) =
  print_module_path print_expr node

(* Arithmetic multiplication *)

and print_E_Mult node = print_op2 "*" node

(* Literal mutez as expressions *)

and print_E_Mutez node = print_mutez node

(* Natural numbers as expressions *)

and print_E_Nat node = print_nat node

(* Arithmetic negation *)

and print_E_Neg (node : minus un_op reg) =
  string "-" ^^ print_expr node.value.arg

(* The empty list as a value *)

and print_E_Nil _ = string "nil"

(* Not Equal *)

and print_E_Neq node = print_op2 "=/=" node

(* The predefined constant constructor [None] as an expression *)

and print_E_None _ = string "None"

(* Boolean negation *)

and print_E_Not node = print_un_op "not" node

and print_un_op op (node : Region.t un_op reg) =
  group (string (op ^ " ") ^^ print_expr node.value.arg)

(* Boolean disjunction *)

and print_E_Or node = print_op2 "or" node

(* Parenthesised expression *)

and print_E_Par (node : expr par reg) = print_par print_expr node

(* Projections *)

and print_E_Proj (node : projection reg) = print_projection node

(* Record expression defined intensionally (that is, by listing all
   the field assignments) *)

and print_E_Record (node : record reg) = print_record node

(* Set expression defined intensionally (that is, by listing all the
   elements) *)

and print_E_Set (node : expr injection reg) =
  print_injection print_expr node

(* Set membership *)

and print_E_SetMem (node : set_mem reg) =
  let {set; element; _} : set_mem = node.value in
  group (print_expr set ^/^ string "contains" ^/^ print_expr element)

(* Application of the predefined constructor [Some] *)

and print_E_Some (node : (kwd_Some * arguments) reg) =
  prefix 4 1 (string "Some") (print_arguments (snd node.value))

(* String literals as expressions *)

and print_E_String node = print_string node

(* Arithmetic subtraction *)

and print_E_Sub node = print_op2 "-" node

(* Boolean truth as an expression *)

and print_E_True _ = string "True"

(* Tuples of expressions *)

and print_E_Tuple node = print_tuple_expr node

(* The unique value of the type "unit" *)

and print_E_Unit _ = string "Unit"

(* Functional updates of record expressions *)

and print_E_Update (node : update reg) =
  let {record; updates; _} = node.value in
  let updates =
    print_ne_injection print_field_path_assignment updates
  and record = print_path record in
  record ^^ string " with" ^^ nest 2 (break 1 ^^ group updates)

and print_field_path_assignment (node : field_path_assignment reg) =
  let {field_path; field_expr; _} = node.value in
  let path = print_path field_path in
  prefix 2 1 (path ^^ string " =") (print_expr field_expr)

(* Expression variables *)

and print_E_Var node = print_ident node

(* Verbatim strings as expressions *)

and print_E_Verbatim node = print_verbatim node


(* EXPORT *)

type cst        = Cst.Pascaligo.t
type expr       = Cst.Pascaligo.expr
type type_expr  = Cst.Pascaligo.type_expr
type pattern    = Cst.Pascaligo.pattern
