open Ast
open Stage_common.To_yojson
type json = Yojson.Safe.t
type 'a json_printer = 'a -> json

let constant' = Stage_common.To_yojson.constant'

let label = label_to_yojson
let option f o =
    match o with
    | None   -> `List [ `String "None" ; `Null ]
    | Some v -> `List [ `String "Some" ; f v ]

let pair f g (x, y) = `Tuple [ f x ; g y ]
let list f lst = `List (List.map f lst)
let label_map f lmap =
  let lst = List.sort (fun (Label a, _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
  let lst' = List.fold_left
    (fun acc (Label k, v) -> (k , f v)::acc)
    [] lst
  in
  `Assoc lst'

let layout = function
  | L_comb -> `List [ `String "L_comb"; `Null ]
  | L_tree -> `List [ `String "L_tree"; `Null ]


let rec type_expression {type_content=tc;type_meta;location;orig_var} =
  `Assoc [
    ("type_content", type_content tc);
    ("type_meta", option Ast_core.Yojson.type_expression type_meta);
    ("location", Location.to_yojson location);
    ("orig_var", option type_variable_to_yojson orig_var);
  ]

and type_content = function
  | T_variable        t -> `List [ `String "t_variable"; type_variable_to_yojson t]
  | T_sum             t -> `List [ `String "t_sum"; rows t]
  | T_record          t -> `List [ `String "t_record"; rows t]
  | T_arrow           t -> `List [ `String "t_arrow"; arrow t]
  | T_constant        t -> `List [ `String "t_constant"; type_injection t]
  | T_module_accessor t -> `List [ `String "t_module_accessor"; module_access type_expression t]
  | T_singleton       t -> `List [ `String "t_singleton" ; literal t ]

and type_injection {language;injection;parameters} =
  `Assoc [
    ("language", `String language);
    ("injection", `String (Ligo_string.extract injection));
    ("parameters", list type_expression parameters)
  ]

and rows {content; layout = l } =
  `Assoc [
    ("content", label_map row_element content);
    ("layout", layout l);
  ]
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

let rec expression {expression_content=ec;location;type_expression=te} =
  `Assoc [
    ("expression_content", expression_content ec);
    ("location", Location.to_yojson location);
    ("type_expression", type_expression te)
  ]

and expression_content = function
  (* Base *)
  | E_literal     e -> `List [ `String "E_literal"; Stage_common.To_yojson.literal e ]
  | E_constant    e -> `List [ `String "E_constant"; constant e ]
  | E_variable    e -> `List [ `String "E_variable"; expression_variable_to_yojson e ]
  | E_application e -> `List [ `String "E_application"; application e ]
  | E_lambda      e -> `List [ `String "E_lambda"; lambda e ]
  | E_recursive   e -> `List [ `String "E_recursive"; recursive e ]
  | E_let_in      e -> `List [ `String "E_let_in"; let_in e ]
  | E_type_in     e -> `List [ `String "E_type_in"; type_in   expression type_expression e ]
  | E_mod_in      e -> `List [ `String "E_mod_in"; mod_in e ]
  | E_mod_alias   e -> `List [ `String "E_mod_alias"; mod_alias expression e ]
  | E_raw_code    e -> `List [ `String "E_raw_code"; raw_code e ]
  (* Variant *)
  | E_constructor     e -> `List [ `String "E_constructor"; constructor e ]
  | E_matching        e -> `List [ `String "E_matching"; matching e ]
  (* Record *)
  | E_record          e -> `List [ `String "E_record"; record e ]
  | E_record_accessor e -> `List [ `String "E_record_accessor"; record_accessor e ]
  | E_record_update   e -> `List [ `String "E_record_update"; record_update e ]
  | E_module_accessor e -> `List [ `String "E_module_accessor"; module_access expression e]

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

and mod_in {module_binder;rhs;let_result} =
  `Assoc [
    ("module_binder", module_variable_to_yojson module_binder);
    ("rhs", module_fully_typed rhs);
    ("let_result", expression let_result);
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
  | Match_record m -> `List [ `String "Match_record"; matching_content_record m ]

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

and matching_content_record {fields; body; tv} =
  `Assoc [
    ("fields", label_map (pair expression_variable_to_yojson type_expression) fields);
    ("body", expression body);
    ("record_type", type_expression tv);
  ]

and declaration_type {type_binder;type_expr} =
  `Assoc [
    ("type_binder", type_variable_to_yojson type_binder);
    ("type_expr", type_expression type_expr);
  ]

and declaration_constant {name; binder;inline;expr} =
  `Assoc [
    ("name", option' string name);
    ("binder",expression_variable_to_yojson binder);
    ("expr", expression expr);
    ("attribute", `Bool inline);
  ]

and declaration_module {module_binder;module_} =
  `Assoc [
    ("module_binder",module_variable_to_yojson module_binder);
    ("module_", module_fully_typed module_);
  ]

and module_alias ({alias ; binders} : module_alias) =
  `Assoc [
    ("alais"  , module_variable_to_yojson alias);
    ("binders", list module_variable_to_yojson @@ List.Ne.to_list binders);
  ]
and declaration = function
  | Declaration_type     dt -> `List [ `String "Declaration_type";     declaration_type dt]
  | Declaration_constant dc -> `List [ `String "Declaration_constant"; declaration_constant dc]
  | Declaration_module   dm -> `List [ `String "Declaration_module";   declaration_module dm]
  | Module_alias         ma -> `List [ `String "Module_alias";         module_alias ma]

and module_fully_typed (Module_Fully_Typed p) = list (Location.wrap_to_yojson declaration) p
let module_with_unification_vars (Module_With_Unification_Vars p) = list (Location.wrap_to_yojson declaration) p


(* Environment *)

let environment_element_definition_declaration {expression=e; free_variables} =
  `Assoc [
    ("expression", expression e);
    ("free_variables", list expression_variable_to_yojson free_variables);
  ]

let environment_element_definition = function
  | ED_binder  -> `List [ `String "ED_binder"; `Null]
  | ED_declaration ed -> `List [ `String "ED_declaration"; environment_element_definition_declaration ed]

let rec environment_element {type_value;definition} =
  `Assoc [
    ("type_value", type_expression type_value);
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

and module_environment_binding {module_variable; module_} =
  `Assoc [
    ("module_name", module_variable_to_yojson module_variable);
    ("module_", environment module_)
  ]

and module_environment e = list module_environment_binding e

and environment {expression_environment=ee;type_environment=te;module_environment=me} =
  `Assoc [
    ("expression_environment", expression_environment ee);
    ("type_environment", type_environment te);
    ("module_environment", module_environment me)
  ]

(* Solver types *)

let constant_tag : constant_tag -> json = function
  | C_arrow        -> `List [`String "C_arrow"; `Null]
  | C_option       -> `List [`String "C_option"; `Null]
  | C_map          -> `List [`String "C_map"; `Null]
  | C_big_map      -> `List [`String "C_big_map"; `Null]
  | C_list         -> `List [`String "C_list"; `Null]
  | C_set          -> `List [`String "C_set"; `Null]
  | C_unit         -> `List [`String "C_unit"; `Null]
  | C_string       -> `List [`String "C_string"; `Null]
  | C_nat          -> `List [`String "C_nat"; `Null]
  | C_mutez        -> `List [`String "C_mutez"; `Null]
  | C_timestamp    -> `List [`String "C_timestamp"; `Null]
  | C_int          -> `List [`String "C_int"; `Null]
  | C_address      -> `List [`String "C_address"; `Null]
  | C_bytes        -> `List [`String "C_bytes"; `Null]
  | C_key_hash     -> `List [`String "C_key_hash"; `Null]
  | C_key          -> `List [`String "C_key"; `Null]
  | C_signature    -> `List [`String "C_signature"; `Null]
  | C_operation    -> `List [`String "C_operation"; `Null]
  | C_contract     -> `List [`String "C_contract"; `Null]
  | C_chain_id     -> `List [`String "C_chain_id"; `Null]
  | C_bls12_381_g1 -> `List [`String "C_bls12_381_g1"; `Null]
  | C_bls12_381_g2 -> `List [`String "C_bls12_381_g2"; `Null]
  | C_bls12_381_fr -> `List [`String "C_bls12_381_fr"; `Null]

let row_tag = function
  | C_record  -> `List [`String "C_record"; `Null]
  | C_variant -> `List [`String "C_variant"; `Null]

let rec type_value t = Location.wrap_to_yojson type_value_ t

and type_value_ = function
  | P_forall     p -> `List [`String "P_"; p_forall p ]
  | P_variable   p -> `List [`String "P_"; type_variable_to_yojson p ]
  | P_constant   p -> `List [`String "P_"; p_constant p ]
  | P_apply      p -> `List [`String "P_"; p_apply p ]
  | P_row        p -> `List [`String "P_"; p_row p ]
  | P_abs        p -> `List [`String "P_"; p_abs p ]
  | P_constraint p -> `List [`String "P_"; p_constraint p ]


and typeclass tc = list (list type_value) tc
and c_equation {aval;bval} =
  `Assoc [
    ("aval", type_value aval);
    ("bval", type_value bval);
  ]

and constraint_identifier (ConstraintIdentifier.T ci : constraint_identifier) : json =
  `Assoc [
    "ConstraintIdentifier", `String (Format.asprintf "%Li" ci);
  ]

and c_typeclass {tc_bound; tc_constraints; tc_args; typeclass=tc;original_id} =
  `Assoc [
    ("tc_bound", list type_variable_to_yojson tc_bound);
    ("tc_constraints", list type_constraint tc_constraints);
    ("tc_args", list type_value tc_args);
    ("original_id", match original_id with Some x -> constraint_identifier x | None -> `String "none");
    ("typeclass", typeclass tc)
  ]

and c_access_label {c_access_label_record_type; accessor; c_access_label_tvar} =
  `Assoc [
    ("c_access_label_record_type", type_value c_access_label_record_type);
    ("accessor", label accessor);
    ("c_acces_label_tvar", type_variable_to_yojson c_access_label_tvar);
  ]

and c_apply ({f; arg} : c_apply) =
  `Assoc [
    ("f", type_variable_to_yojson f);
    ("arg", type_variable_to_yojson arg);
  ]

and type_constraint_ = function
  | C_equation     c -> `List [`String "C_equation"; c_equation c]
  | C_typeclass    c -> `List [`String "C_typeclass"; c_typeclass c]
  | C_access_label c -> `List [`String "C_access_label"; c_access_label c]
  | C_apply        c -> `List [`String "C_apply"; c_apply c]

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
    ("p_row_args", label_map row_value p_row_args);
  ]

and p_abs {arg;ret} =
  `Assoc [
    ("arg", type_variable_to_yojson arg);
    ("ret", type_value ret);
  ]

and p_constraint {pc} =
  `Assoc [
    ("pc", type_constraint pc)
  ]

and row_value {associated_value; michelson_annotation; decl_pos} =
  `Assoc [
    ("associated_value", type_value associated_value);
    ("michelson_annotation", option (fun s -> `String s) michelson_annotation);
    ("decl_pos", `Int decl_pos);
  ]


let c_constructor_simpl {id_constructor_simpl = ConstraintIdentifier.T ci;reason_constr_simpl;original_id;tv;c_tag;tv_list} =
  `Assoc [
    ("id_row_simpl", `String (Format.sprintf "%Li" ci));
    ("original_id", `String (match original_id with Some (ConstraintIdentifier.T x) -> Format.asprintf "%Li" x | None -> "null" ));
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

let c_poly_simpl {id_poly_simpl = ConstraintIdentifier.T ci; reason_poly_simpl; original_id; tv; forall} =
  `Assoc [
    ("id_row_simpl", `String (Format.sprintf "%Li" ci));
    ("original_id", `String (match original_id with Some (ConstraintIdentifier.T x) -> Format.asprintf "%Li" x | None -> "null" ));
    ("reason_poly_simpl", `String reason_poly_simpl);
    ("tv", type_variable_to_yojson tv);
    ("forall", p_forall forall)
  ]

let rec c_typeclass_simpl {tc_bound; tc_constraints; id_typeclass_simpl = ConstraintIdentifier.T ci;reason_typeclass_simpl;original_id;tc;args} =
  `Assoc [
    ("tc_bound", list type_variable_to_yojson tc_bound);
    ("tc_constraints", list type_constraint_simpl tc_constraints);
    ("id_typeclass_simpl", `String (Format.sprintf "%Li" ci));
    ("original_id", `String (match original_id with Some (ConstraintIdentifier.T x) -> Format.asprintf "%Li" x | None -> "null" ));
    ("reason_typeclass_simpl", `String reason_typeclass_simpl);
    ("tc", typeclass tc);
    ("args", list type_variable_to_yojson args)
  ]

and c_access_label_simpl { id_access_label_simpl = ConstraintIdentifier.T ci ; reason_access_label_simpl ; record_type ; label = l ; tv } =
  `Assoc [
    ("id_access_label_simpl", `String (Format.sprintf "%Li" ci));
    ("reason_access_label_simpl", `String reason_access_label_simpl);
    ("record_type", type_variable_to_yojson record_type);
    ("label", label_to_yojson l);
    ("tv", type_variable_to_yojson tv)
  ]

and row_variable {associated_variable; michelson_annotation; decl_pos} =
  `Assoc [
    ("associated_variable", type_variable_to_yojson associated_variable);
    ("michelson_annotation", option (fun s -> `String s) michelson_annotation);
    ("decl_pos", `Int decl_pos);
  ]

and c_row_simpl {id_row_simpl = ConstraintIdentifier.T ci; reason_row_simpl; original_id; tv;r_tag;tv_map} =
  `Assoc [
    ("id_row_simpl", `String (Format.sprintf "%Li" ci));
    ("original_id", `String (match original_id with Some (ConstraintIdentifier.T x) -> Format.asprintf "%Li" x | None -> "null" ));
    ("reason_row_simpl", `String reason_row_simpl);
    ("tv", type_variable_to_yojson tv);
    ("r_tag", row_tag r_tag);
    ("tv_map", label_map row_variable tv_map)
  ]
and c_abs_simpl {id_abs_simpl = ConstraintIdentifier.T ci; reason_abs_simpl; tv;param;body} =
  `Assoc [
    ("id_abs_simpl", `String (Format.sprintf "%Li" ci));
    ("reason_abs_simpl", `String reason_abs_simpl);
    ("tv", type_variable_to_yojson tv);
    ("param", type_variable_to_yojson param);
    ("body", type_value body)
  ]
and c_apply_simpl {id_apply_simpl = ConstraintIdentifier.T ci; reason_apply_simpl; f;arg} =
  `Assoc [
    ("id_apply_simpl", `String (Format.sprintf "%Li" ci));
    ("reason_apply_simpl", `String reason_apply_simpl);
    ("param", type_variable_to_yojson f);
    ("body", type_variable_to_yojson arg)
  ]
and type_constraint_simpl = function
  | SC_Abs         c -> `List [`String "SC_abs"; c_abs_simpl c]
  | SC_Apply       c -> `List [`String "SC_apply"; c_apply_simpl c]
  | SC_Constructor c -> `List [`String "SC_constructor"; c_constructor_simpl c]
  | SC_Alias       c -> `List [`String "SC_alias"; c_alias c]
  | SC_Poly        c -> `List [`String "SC_Poly"; c_poly_simpl c]
  | SC_Typeclass   c -> `List [`String "SC_Typclass"; c_typeclass_simpl c]
  | SC_Access_label c -> `List [`String "SC_Access_label"; c_access_label_simpl c]
  | SC_Row         c -> `List [`String "SC_Row"; c_row_simpl c]

let poly_unionfind f p =
  let lst = (UnionFind.Poly2.partitions p) in
  let lst' = List.map
      (fun l ->
         let repr = f (UnionFind.Poly2.repr (List.hd l) p ) in
         `List (repr :: List.map f l)) lst in
  `Assoc ["UnionFind", `List lst']

let unionfind : type_variable UnionFind.Poly2.t -> _ = poly_unionfind type_variable_to_yojson

let typeVariableMap f tvmap =
  let lst = List.sort (fun (a, _) (b, _) -> Var.compare a b) (RedBlackTrees.PolyMap.bindings tvmap) in
  let aux (k, v) =
    `Assoc [ Format.asprintf "%a" Var.pp k , f v ] in
  let lst' = List.map aux lst in
  `Assoc ["typeVariableMap",  `List lst']
let ciMap f tvmap =
  let lst = List.sort (fun (ConstraintIdentifier.T a, _) (ConstraintIdentifier.T b, _) -> Int64.compare a b) (RedBlackTrees.PolyMap.bindings tvmap) in
  let aux (ConstraintIdentifier.T k, v) =
    `Assoc [ Format.asprintf "%Li" k , f v ] in
  let lst' = List.map aux lst in
  `Assoc ["typeVariableMap",  `List lst']
let jmap (fk : _ -> json) (fv : _ -> json)  tvmap : json =
  let lst = RedBlackTrees.PolyMap.bindings tvmap in
  let aux (k, v) =
    `List [ fk k ; fv v ] in
  let lst' = List.map aux lst in
  `Assoc ["typeVariableMap",  `List lst']
let constraint_identifier_set s =
  let lst = List.sort (fun (ConstraintIdentifier.T a) (ConstraintIdentifier.T b) -> Int64.compare a b) (RedBlackTrees.PolySet.elements s) in
  let aux (ConstraintIdentifier.T ci) =
    `String (Format.asprintf "ConstraintIdentifier %Li" ci) in
  let lst' = List.map aux lst in
  `Assoc ["constraintIdentifierSet",  `List lst']
let type_variable_set s =
  let lst = List.sort Var.compare (RedBlackTrees.PolySet.elements s) in
  let aux v =
    `String (Format.asprintf "%a" Var.pp v) in
  let lst' = List.map aux lst in
  `Assoc ["typeVariableSet",  `List lst']

let constraint_identifier (ConstraintIdentifier.T ci : constraint_identifier) : json =
  `Assoc [
    "ConstraintIdentifier", `String (Format.asprintf "%Li" ci);
  ]

(* let structured_dbs {refined_typeclasses;refined_typeclasses_back;typeclasses_constrained_by;by_constraint_identifier;all_constraints;aliases;assignments;grouped_by_variable;cycle_detection_toposort=_} =
 *   `Assoc [
 *     ("refined_typeclasses", jmap constraint_identifier refined_typeclass refined_typeclasses);
 *     ("refined_typeclasses", jmap constraint_identifier constraint_identifier refined_typeclasses_back);
 *     ("typeclasses_constrained_by", typeVariableMap constraint_identifier_set typeclasses_constrained_by);
 *     ("by_constraint_identifier", ciMap c_typeclass_simpl by_constraint_identifier);
 *     ("all_constrants", list type_constraint_simpl all_constraints);
 *     ("aliases", unionfind aliases);
 *     ("assignments", typeVariableMap c_constructor_simpl assignments);
 *     ("grouped_by_variable", typeVariableMap constraints grouped_by_variable);
 *     ("cycl_detection_toposort", `Null)
 *   ] *)
let constructor_or_row (t : constructor_or_row ) =
  match t with
  | `Row r -> `Assoc [ ("row" , c_row_simpl r) ]
  | `Constructor c -> `Assoc [ ("constructor" , c_constructor_simpl c) ]

