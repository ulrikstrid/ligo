open Types

type json = Yojson.Safe.t

let type_constant = function
  | TC_unit                      -> `List [ `String "TC_unit"; `Null]
  | TC_string                    -> `List [ `String "TC_string"; `Null]
  | TC_bytes                     -> `List [ `String "TC_bytes"; `Null]
  | TC_nat                       -> `List [ `String "TC_nat"; `Null]
  | TC_int                       -> `List [ `String "TC_int"; `Null]
  | TC_mutez                     -> `List [ `String "TC_mutez"; `Null]
  | TC_operation                 -> `List [ `String "TC_operation"; `Null]
  | TC_address                   -> `List [ `String "TC_address"; `Null]
  | TC_key                       -> `List [ `String "TC_key"; `Null]
  | TC_key_hash                  -> `List [ `String "TC_key_hash"; `Null]
  | TC_chain_id                  -> `List [ `String "TC_chain_id"; `Null]
  | TC_signature                 -> `List [ `String "TC_signature"; `Null]
  | TC_timestamp                 -> `List [ `String "TC_timestamp"; `Null]
  | TC_contract                  -> `List [ `String "TC_contract"; `Null]
  | TC_option                    -> `List [ `String "TC_option"; `Null]
  | TC_list                      -> `List [ `String "TC_list"; `Null]
  | TC_set                       -> `List [ `String "TC_set"; `Null]
  | TC_map                       -> `List [ `String "TC_map"; `Null]
  | TC_big_map                   -> `List [ `String "TC_big_map"; `Null]
  | TC_map_or_big_map            -> `List [ `String "TC_map_or_big_map"; `Null]
  | TC_michelson_pair            -> `List [ `String "TC_michelson_pair"; `Null]
  | TC_michelson_or              -> `List [ `String "TC_michelson_or"; `Null]
  | TC_michelson_pair_right_comb -> `List [ `String "TC_michelson_pair_right_comb"; `Null]
  | TC_michelson_pair_left_comb  -> `List [ `String "TC_michelson_pair_left_comb"; `Null]
  | TC_michelson_or_right_comb   -> `List [ `String "TC_michelson_or_right_comb"; `Null]
  | TC_michelson_or_left_comb    -> `List [ `String "TC_michelson_or_left_comb"; `Null]

let constant' = function
  | C_INT                -> `List [`String "C_INT"; `Null ]
  | C_UNIT               -> `List [`String "C_UNIT"; `Null ]
  | C_NIL                -> `List [`String "C_NIL"; `Null ]
  | C_NOW                -> `List [`String "C_NOW"; `Null ]
  | C_IS_NAT             -> `List [`String "C_IS_NAT"; `Null ]
  | C_SOME               -> `List [`String "C_SOME"; `Null ]
  | C_NONE               -> `List [`String "C_NONE"; `Null ]
  | C_ASSERTION          -> `List [`String "C_ASSERTION"; `Null ]
  | C_ASSERT_SOME        -> `List [`String "C_ASSERT_SOME"; `Null ]
  | C_ASSERT_INFERRED    -> `List [`String "C_ASSERT_INFERRED"; `Null ]
  | C_FAILWITH           -> `List [`String "C_FAILWITH"; `Null ]
  | C_UPDATE             -> `List [`String "C_UPDATE"; `Null ]
  (* Loops *)
  | C_ITER               -> `List [`String "C_ITER"; `Null ]
  | C_FOLD_WHILE         -> `List [`String "C_FOLD_WHILE"; `Null ]
  | C_FOLD_CONTINUE      -> `List [`String "C_FOLD_CONTINUE"; `Null ]
  | C_FOLD_STOP          -> `List [`String "C_FOLD_STOP"; `Null ]
  | C_LOOP_LEFT          -> `List [`String "C_LOOP_LEFT"; `Null ]
  | C_LOOP_CONTINUE      -> `List [`String "C_LOOP_CONTINUE"; `Null ]
  | C_LOOP_STOP          -> `List [`String "C_LOOP_STOP"; `Null ]
  | C_FOLD               -> `List [`String "C_FOLD"; `Null ]
  (* MATH *)
  | C_NEG                -> `List [`String "C_NEG"; `Null ]
  | C_ABS                -> `List [`String "C_ABS"; `Null ]
  | C_ADD                -> `List [`String "C_ADD"; `Null ]
  | C_SUB                -> `List [`String "C_SUB"; `Null ]
  | C_MUL                -> `List [`String "C_MUL"; `Null ]
  | C_EDIV               -> `List [`String "C_EDIV"; `Null ]
  | C_DIV                -> `List [`String "C_DIV"; `Null ]
  | C_MOD                -> `List [`String "C_MOD"; `Null ]
  (* LOGIC *)
  | C_NOT                -> `List [`String "C_NOT"; `Null ]
  | C_AND                -> `List [`String "C_AND"; `Null ]
  | C_OR                 -> `List [`String "C_OR"; `Null ]
  | C_XOR                -> `List [`String "C_XOR"; `Null ]
  | C_LSL                -> `List [`String "C_LSL"; `Null ]
  | C_LSR                -> `List [`String "C_LSR"; `Null ]
  (* COMPARATOR *)
  | C_EQ                 -> `List [`String "C_EQ"; `Null ]
  | C_NEQ                -> `List [`String "C_NEQ"; `Null ]
  | C_LT                 -> `List [`String "C_LT"; `Null ]
  | C_GT                 -> `List [`String "C_GT"; `Null ]
  | C_LE                 -> `List [`String "C_LE"; `Null ]
  | C_GE                 -> `List [`String "C_GE"; `Null ]
  (* Bytes/ String *)
  | C_SIZE               -> `List [`String "C_SIZE"; `Null ]
  | C_CONCAT             -> `List [`String "C_CONCAT"; `Null ]
  | C_SLICE              -> `List [`String "C_SLICE"; `Null ]
  | C_BYTES_PACK         -> `List [`String "C_BYTES_PACK"; `Null ]
  | C_BYTES_UNPACK       -> `List [`String "C_BYTES_UNPACK"; `Null ]
  | C_CONS               -> `List [`String "C_CONS"; `Null ]
  (* Pair *)
  | C_PAIR               -> `List [`String "C_PAIR"; `Null ]
  | C_CAR                -> `List [`String "C_CAR"; `Null ]
  | C_CDR                -> `List [`String "C_CDR"; `Null ]
  | C_LEFT               -> `List [`String "C_LEFT"; `Null ]
  | C_RIGHT              -> `List [`String "C_RIGHT"; `Null ]
  (* Set *)
  | C_SET_EMPTY          -> `List [`String "C_SET_EMPTY"; `Null ]
  | C_SET_LITERAL        -> `List [`String "C_SET_LITERAL"; `Null ]
  | C_SET_ADD            -> `List [`String "C_SET_ADD"; `Null ]
  | C_SET_REMOVE         -> `List [`String "C_SET_REMOVE"; `Null ]
  | C_SET_ITER           -> `List [`String "C_SET_ITER"; `Null ]
  | C_SET_FOLD           -> `List [`String "C_SET_FOLD"; `Null ]
  | C_SET_MEM            -> `List [`String "C_SET_MEM"; `Null ]
  (* List *)
  | C_LIST_EMPTY         -> `List [`String "C_LIST_EMPTY"; `Null ]
  | C_LIST_LITERAL       -> `List [`String "C_LIST_LITERAL"; `Null ]
  | C_LIST_ITER          -> `List [`String "C_LIST_ITER"; `Null ]
  | C_LIST_MAP           -> `List [`String "C_LIST_MAP"; `Null ]
  | C_LIST_FOLD          -> `List [`String "C_LIST_FOLD"; `Null ]
  (* Maps *)
  | C_MAP                -> `List [`String "C_MAP"; `Null ]
  | C_MAP_EMPTY          -> `List [`String "C_MAP_EMPTY"; `Null ]
  | C_MAP_LITERAL        -> `List [`String "C_MAP_LITERAL"; `Null ]
  | C_MAP_GET            -> `List [`String "C_MAP_GET"; `Null ]
  | C_MAP_GET_FORCE      -> `List [`String "C_MAP_GET_FORCE"; `Null ]
  | C_MAP_ADD            -> `List [`String "C_MAP_ADD"; `Null ]
  | C_MAP_REMOVE         -> `List [`String "C_MAP_REMOVE"; `Null ]
  | C_MAP_UPDATE         -> `List [`String "C_MAP_UPDATE"; `Null ]
  | C_MAP_ITER           -> `List [`String "C_MAP_ITER"; `Null ]
  | C_MAP_MAP            -> `List [`String "C_MAP_MAP"; `Null ]
  | C_MAP_FOLD           -> `List [`String "C_MAP_FOLD"; `Null ]
  | C_MAP_MEM            -> `List [`String "C_MAP_MEM"; `Null ]
  | C_MAP_FIND           -> `List [`String "C_MAP_FIND"; `Null ]
  | C_MAP_FIND_OPT       -> `List [`String "C_MAP_FIND_OPT"; `Null ]
  (* Big Maps *)
  | C_BIG_MAP            -> `List [`String "C_BIG_MAP"; `Null ]
  | C_BIG_MAP_EMPTY      -> `List [`String "C_BIG_MAP_EMPTY"; `Null ]
  | C_BIG_MAP_LITERAL    -> `List [`String "C_BIG_MAP_LITERAL"; `Null ]
  (* Crypto *)
  | C_SHA256             -> `List [`String "C_SHA256"; `Null ]
  | C_SHA512             -> `List [`String "C_SHA512"; `Null ]
  | C_BLAKE2b            -> `List [`String "C_BLAKE2b"; `Null ]
  | C_HASH               -> `List [`String "C_HASH"; `Null ]
  | C_HASH_KEY           -> `List [`String "C_HASH_KEY"; `Null ]
  | C_CHECK_SIGNATURE    -> `List [`String "C_CHECK_SIGNATURE"; `Null ]
  | C_CHAIN_ID           -> `List [`String "C_CHAIN_ID"; `Null ]
  (* Blockchain *)
  | C_CALL                     -> `List [`String "C_CALL"; `Null ]
  | C_CONTRACT                 -> `List [`String "C_CONTRACT"; `Null ]
  | C_CONTRACT_OPT             -> `List [`String "C_CONTRACT_OPT"; `Null ]
  | C_CONTRACT_ENTRYPOINT      -> `List [`String "C_CONTRACT_ENTRYPOINT"; `Null ]
  | C_CONTRACT_ENTRYPOINT_OPT  -> `List [`String "C_CONTRACT_ENTRYPOINT_OPT"; `Null ]
  | C_AMOUNT                   -> `List [`String "C_AMOUNT"; `Null ]
  | C_BALANCE                  -> `List [`String "C_BALANCE"; `Null ]
  | C_SOURCE                   -> `List [`String "C_SOURCE"; `Null ]
  | C_SENDER                   -> `List [`String "C_SENDER"; `Null ]
  | C_ADDRESS                  -> `List [`String "C_ADDRESS"; `Null ]
  | C_SELF                     -> `List [`String "C_SELF"; `Null ]
  | C_SELF_ADDRESS             -> `List [`String "C_SELF_ADDRESS"; `Null ]
  | C_IMPLICIT_ACCOUNT         -> `List [`String "C_IMPLICIT_ACCOUNT"; `Null ]
  | C_SET_DELEGATE             -> `List [`String "C_SET_DELEGATE"; `Null ]
  | C_CREATE_CONTRACT          -> `List [`String "C_CREATE_CONTRACT"; `Null ]
  | C_CONVERT_TO_LEFT_COMB     -> `List [`String "C_CONVERT_TO_LEFT_COMB"; `Null ]
  | C_CONVERT_TO_RIGHT_COMB    -> `List [`String "C_CONVERT_TO_RIGHT_COMB"; `Null ]
  | C_CONVERT_FROM_LEFT_COMB   -> `List [`String "C_CONVERT_FROM_LEFT_COMB"; `Null ]
  | C_CONVERT_FROM_RIGHT_COMB  -> `List [`String "C_CONVERT_FROM_RIGHT_COMB"; `Null ]

let literal = function
  | Literal_unit        -> `List [`String "Literal_unit"; `Null ]
  | Literal_int       l -> `List [`String "Literal_unit"; z_to_yojson l ]
  | Literal_nat       l -> `List [`String "Literal_unit"; z_to_yojson l ]
  | Literal_timestamp l -> `List [`String "Literal_unit"; z_to_yojson l ]
  | Literal_mutez     l -> `List [`String "Literal_unit"; z_to_yojson l ]
  | Literal_string    l -> `List [`String "Literal_unit"; Ligo_string.to_yojson l ]
  | Literal_bytes     l -> `List [`String "Literal_unit"; bytes_to_yojson l ]
  | Literal_address   l -> `List [`String "Literal_unit"; `String l ]
  | Literal_signature l -> `List [`String "Literal_unit"; `String l ]
  | Literal_key       l -> `List [`String "Literal_unit"; `String l ]
  | Literal_key_hash  l -> `List [`String "Literal_unit"; `String l ]
  | Literal_chain_id  l -> `List [`String "Literal_unit"; `String l ]
  | Literal_operation l -> `List [`String "Literal_unit"; bytes_to_yojson l ]

let label = label_to_yojson
let option f o =
    match o with
    | None   -> `List [ `String "None" ; `Null ]
    | Some v -> `List [ `String "Some" ; f v ]

let list f lst = `List (List.map f lst)
let label_map f lmap =
  let lst = List.sort (fun (Label a, _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
  let lst' = List.fold_left
    (fun acc (Label k, v) -> (k , f v)::acc)
    [] lst
  in
  `Assoc lst'


let rec type_expression {type_content=tc;type_meta;location} =
  `Assoc [
    ("type_content", type_content tc);
    ("type_meta", option Ast_core.Yojson.type_expression type_meta);
    ("location", Location.to_yojson location);
  ]

and type_content = function
  | T_sum      t -> `List [ `String "t_sum"; label_map row_element t]
  | T_record   t -> `List [ `String "t_record"; label_map row_element t]
  | T_arrow    t -> `List [ `String "t_arrow"; arrow t]
  | T_variable t -> `List [ `String "t_variable"; type_variable_to_yojson t]
  | T_constant t -> `List [ `String "t_constant"; type_operator t]
  | T_wildcard   -> `List [ `String "t_wildcard"; `Null]

and row_element {associated_type; michelson_annotation; decl_pos} =
  `Assoc [
    ("associated_type", type_expression associated_type);
    ("michelson_annotation", option (fun s -> `String s) michelson_annotation);
    ("decl_pos", `Int decl_pos);
  ]

and arrow {type1;type2} =
  `Assoc [
    ("type1", type_expression type1);
    ("type2", type_expression type2);
  ]

and type_operator {type_constant=tc; arguments} =
  `Assoc [
    ("type_constant", type_constant tc);
    ("arguments", list type_expression arguments)
  ]

let rec expression {expression_content=ec;location;type_expression=te} =
  `Assoc [
    ("expression_content", expression_content ec);
    ("location", Location.to_yojson location);
    ("type_expression", type_expression te)
  ]

and expression_content = function 
  (* Base *)
  | E_literal     e -> `List [ `String "E_literal"; literal e ]
  | E_constant    e -> `List [ `String "E_constant"; constant e ]
  | E_variable    e -> `List [ `String "E_variable"; expression_variable_to_yojson e ]
  | E_application e -> `List [ `String "E_application"; application e ]
  | E_lambda      e -> `List [ `String "E_lambda"; lambda e ]
  | E_recursive   e -> `List [ `String "E_recursive"; recursive e ]
  | E_let_in      e -> `List [ `String "E_let_in"; let_in e ]
  | E_raw_code    e -> `List [ `String "E_raw_code"; raw_code e ]
  (* Variant *)
  | E_constructor     e -> `List [ `String "E_constructor"; constructor e ]
  | E_matching        e -> `List [ `String "E_matching"; matching e ]
  (* Record *)
  | E_record          e -> `List [ `String "E_record"; record e ]
  | E_record_accessor e -> `List [ `String "E_record_accessor"; record_accessor e ]
  | E_record_update   e -> `List [ `String "E_record_update"; record_update e ]

and constant {cons_name;arguments} =
  `Assoc [
    ("cons_name", constant' cons_name);
    ("arguments", list expression arguments);
  ]

and application {lamb;args} =
  `Assoc [
    ("lamb", expression lamb);
    ("args", expression args);
  ]

and lambda {binder;result} =
  `Assoc [
    ("binder", expression_variable_to_yojson binder);
    ("result", expression result);
  ]

and recursive {fun_name;fun_type;lambda=l} =
  `Assoc [
    ("fun_name", expression_variable_to_yojson fun_name);
    ("fun_type", type_expression fun_type);
    ("lambda", lambda l)
  ]

and let_in {let_binder;rhs;let_result;inline} =
  `Assoc [
    ("let_binder", expression_variable_to_yojson let_binder);
    ("rhs", expression rhs);
    ("let_result", expression let_result);
    ("inline", `Bool inline);
  ]

and raw_code {language;code} =
  `Assoc [
    ("language", `String language);
    ("code", expression code);
  ]

and constructor {constructor;element} =
  `Assoc [
    ("constructor", label constructor);
    ("element", expression element);
  ]

and matching {matchee; cases} =
  `Assoc [
    ("matchee", expression matchee);
    ("cases", matching_expr cases);
  ]

and record r = label_map expression r

and record_accessor {record; path} =
  `Assoc [
    ("record", expression record);
    ("path", label path);
  ]

and record_update {record; path; update} =
  `Assoc [
    ("record", expression record);
    ("path", label path);
    ("update", expression update);
  ]

and matching_expr = function
  | Match_list    m -> `List [ `String "Match_list";    matching_content_list    m ]
  | Match_option  m -> `List [ `String "Match_option";  matching_content_option  m ]
  | Match_variant m -> `List [ `String "Match_variant"; matching_content_variant m ]

and matching_content_list {match_nil;match_cons} =
  `Assoc [
    ("match_nil", expression match_nil);
    ("match_cons", matching_content_cons match_cons);
  ]

and matching_content_cons {hd;tl;body;tv} =
  `Assoc [
    ("hd", expression_variable_to_yojson hd);
    ("tl", expression_variable_to_yojson tl);
    ("body", expression body);
    ("tv", type_expression tv);
  ]

and matching_content_option {match_none;match_some} =
  `Assoc [
    ("match_none", expression match_none);
    ("match_some", matching_content_some match_some);
  ]

and matching_content_some {opt;body;tv} =
  `Assoc [
    ("opt", expression_variable_to_yojson opt);
    ("body", expression body);
    ("tv", type_expression tv);
  ]

and matching_content_variant {cases;tv} =
  `Assoc [
    ("cases", list matching_content_case cases);
    ("tv", type_expression tv);
  ]

and matching_content_case {constructor; pattern; body} =
  `Assoc [
    ("constructor", label constructor);
    ("pattern", expression_variable_to_yojson pattern);
    ("body", expression body);
  ]

let declaration_type {type_binder;type_expr} =
  `Assoc [
    ("type_binder", type_variable_to_yojson type_binder);
    ("type_expr", type_expression type_expr);
  ]

let declaration_constant {binder;inline;expr} =
  `Assoc [
    ("binder",expression_variable_to_yojson binder);
    ("expr", expression expr);
    ("attribute", `Bool inline);
  ]
let declaration = function 
  | Declaration_type     dt -> `List [ `String "Declaration_type"; declaration_type dt]
  | Declaration_constant dc -> `List [ `String "Declaration_constant"; declaration_constant dc]

let program = list (Location.wrap_to_yojson declaration)


(* Environment *)

let environment_element_definition_declaration {expression=e; free_variables} = 
  `Assoc [
    ("expression", expression e);
    ("free_variables", list expression_variable_to_yojson free_variables);
  ]

let environment_element_definition = function
  | ED_binder  -> `List [ `String "ED_binder"; `Null]
  | ED_declaration ed -> `List [ `String "ED_declaration"; environment_element_definition_declaration ed]

let rec environment_element {type_value;source_environment;definition} =
  `Assoc [
    ("type_value", type_expression type_value);
    ("source_environment", environment source_environment);
    ("definition", environment_element_definition definition);
  ]

and environment_binding {expr_var;env_elt} =
  `Assoc [
    ("expr_var", expression_variable_to_yojson expr_var);
    ("env_elt", environment_element env_elt);
  ]
and expression_environment e = list environment_binding e

and type_environment_binding {type_variable;type_} =
  `Assoc [
    ("type_variable", type_variable_to_yojson type_variable);
    ("type_", type_expression type_);
  ]
and type_environment e = list type_environment_binding e

and environment {expression_environment=ee;type_environment=te} =
  `Assoc [
    ("expression_environment", expression_environment ee);
    ("type_environment", type_environment te);
  ]

(* Solver types *)

let constant_tag : constant_tag -> json = function
  | C_arrow     -> `List [`String "C_arrow"; `Null]
  | C_option    -> `List [`String "C_option"; `Null]
  | C_map       -> `List [`String "C_map"; `Null]
  | C_big_map   -> `List [`String "C_big_map"; `Null]
  | C_list      -> `List [`String "C_list"; `Null]
  | C_set       -> `List [`String "C_set"; `Null]
  | C_unit      -> `List [`String "C_unit"; `Null]
  | C_string    -> `List [`String "C_string"; `Null]
  | C_nat       -> `List [`String "C_nat"; `Null]
  | C_mutez     -> `List [`String "C_mutez"; `Null]
  | C_timestamp -> `List [`String "C_timestamp"; `Null]
  | C_int       -> `List [`String "C_int"; `Null]
  | C_address   -> `List [`String "C_address"; `Null]
  | C_bytes     -> `List [`String "C_bytes"; `Null]
  | C_key_hash  -> `List [`String "C_key_hash"; `Null]
  | C_key       -> `List [`String "C_key"; `Null]
  | C_signature -> `List [`String "C_signature"; `Null]
  | C_operation -> `List [`String "C_operation"; `Null]
  | C_contract  -> `List [`String "C_contract"; `Null]
  | C_chain_id  -> `List [`String "C_chain_id"; `Null]

let row_tag = function
  | C_record  -> `List [`String "C_record"; `Null]
  | C_variant -> `List [`String "C_variant"; `Null]

let rec type_value {tsrc;t} =
  `Assoc [
    ("tsrc", `String tsrc);
    ("t", type_value_ t);
  ]

and type_value_ = function
  | P_forall    p -> `List [`String "P_"; p_forall p ]
  | P_variable  p -> `List [`String "P_"; type_variable_to_yojson p ]
  | P_constant  p -> `List [`String "P_"; p_constant p ]
  | P_apply     p -> `List [`String "P_"; p_apply p ]
  | P_row       p -> `List [`String "P_"; p_row p ]


and typeclass tc = list (list type_value) tc
and c_equation {aval;bval} =
  `Assoc [
    ("aval", type_value aval);
    ("bval", type_value bval);
  ]

and c_typeclass {tc_args; typeclass=tc} =
  `Assoc [
    ("tc_args", list type_value tc_args);
    ("typeclass", typeclass tc)
  ]

and c_access_label {c_access_label_tval; accessor; c_access_label_tvar} =
  `Assoc [
    ("c_access_label_tval", type_value c_access_label_tval);
    ("accessor", label accessor);
    ("c_acces_label_tvar", type_variable_to_yojson c_access_label_tvar);
  ]

and type_constraint_ = function
  | C_equation     c -> `List [`String "C_equation"; c_equation c]
  | C_typeclass    c -> `List [`String "C_typeclass"; c_typeclass c]
  | C_access_label c -> `List [`String "C_access_label"; c_access_label c]

and type_constraint {reason;c} =
  `Assoc [
    ("reason", `String reason);
    ("c", type_constraint_ c);
  ]
and p_constraints p = list type_constraint p
and p_forall {binder; constraints;body} =
  `Assoc [
    ("binder", type_variable_to_yojson binder);
    ("constraints", p_constraints constraints);
    ("body", type_value body);
  ]

and p_constant {p_ctor_tag; p_ctor_args} =
  `Assoc [
    ("p_ctor_tag", constant_tag p_ctor_tag);
    ("p_ctor_args", list type_value p_ctor_args);
  ]

and p_apply {tf;targ} =
  `Assoc [
    ("tf", type_value tf);
    ("targ", type_value targ);
  ]

and p_row {p_row_tag;p_row_args} =
  `Assoc [
    ("p_row_tag", row_tag p_row_tag);
    ("p_row_args", label_map type_value p_row_args);
  ]

let c_constructor_simpl {reason_constr_simpl;tv;c_tag;tv_list} =
  `Assoc [
    ("reason_constr_simpl", `String reason_constr_simpl);
    ("tv", type_variable_to_yojson tv);
    ("c_tag", constant_tag c_tag);
    ("tv_list", list type_variable_to_yojson tv_list)
  ]

let c_alias {reason_alias_simpl; a; b} =
  `Assoc [
    ("reason_alias_simpl", `String reason_alias_simpl);
    ("a", type_variable_to_yojson a);
    ("b", type_variable_to_yojson b);
  ]

let c_poly_simpl {reason_poly_simpl; tv; forall} =
  `Assoc [
    ("reason_poly_simpl", `String reason_poly_simpl);
    ("tv", type_variable_to_yojson tv);
    ("forall", p_forall forall)
  ]

let c_typeclass_simpl {reason_typeclass_simpl;tc;args} =
  `Assoc [
    ("reason_typeclass_simpl", `String reason_typeclass_simpl);
    ("tc", typeclass tc);
    ("args", list type_variable_to_yojson args)
  ]

let c_row_simpl {reason_row_simpl; tv;r_tag;tv_map} =
  `Assoc [
    ("reason_row_simpl", `String reason_row_simpl);
    ("tv", type_variable_to_yojson tv);
    ("r_tag", row_tag r_tag);
    ("tv_map", label_map type_variable_to_yojson tv_map)
  ]
let type_constraint_simpl = function
  | SC_Constructor c -> `List [`String "SC_constructor"; c_constructor_simpl c] 
  | SC_Alias       c -> `List [`String "SC_alias"; c_alias c]
  | SC_Poly        c -> `List [`String "SC_Poly"; c_poly_simpl c]
  | SC_Typeclass   c -> `List [`String "SC_Typclass"; c_typeclass_simpl c]
  | SC_Row         c -> `List [`String "SC_Row"; c_row_simpl c]


let poly_unionfind f p =  
  let lst = (UnionFind.Poly2.partitions p) in
  let lst' = List.map (fun l -> f (UnionFind.Poly2.repr (List.hd l) p )) lst in
  `Assoc ["UnionFind", `List lst']

let unionfind = poly_unionfind type_variable_to_yojson

let typeVariableMap f tvmap =
  let lst = List.sort (fun (a, _) (b, _) -> Var.compare a b) (RedBlackTrees.PolyMap.bindings tvmap) in
  let aux (k, v) =
    `Assoc [ Format.asprintf "%a" Var.pp k , f v ] in
  let lst' = List.map aux lst in
  `Assoc ["typeVariableMap",  `List lst']

let constraints {constructor; poly; tc; row} =
  `Assoc [
    ("constructor", list c_constructor_simpl constructor);
    ("poly", list c_poly_simpl poly);
    ("tc", list c_typeclass_simpl tc);
    ("row", list c_row_simpl row);
  ]
let structured_dbs {all_constraints;aliases;assignments;grouped_by_variable;cycle_detection_toposort=_} =
  `Assoc [
    ("all_constrants", list type_constraint_simpl all_constraints);
    ("aliases", unionfind aliases);
    ("assignments", typeVariableMap c_constructor_simpl assignments);
    ("grouped_by_variable", typeVariableMap constraints grouped_by_variable);
    ("cycl_detection_toposort", `Null)
  ]