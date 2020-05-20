[@@@coverage exclude_file]
open Types
module J = Yojson.Basic

include Stage_common.PP

module TMap = Map.Make( struct type t = string let compare = compare end)
type 'a tmap = 'a TMap.t

type mytmap = t_content tmap
(* [@to_yojson (fun m -> TMap.bindings m |> [%to_yojson: (string * t_content) list])] *)
and t_content = {
  t1 : t1;
  t2 : t2;
}
and t1 = string
and t2 =
  | Foo of t2
  | Bar of mytmap
(* [@@deriving yojson] *)

(* let pouet = type_constant_to_yojson
let pouett = type_constant_of_yojson *)

(* include Ast_PP_type(Ast_typed_type_parameter) *)
(* 
let rec program (p: program) : J.t =
  `List (List.map
    (fun decl ->
      let (Declaration_constant (var, exp, inline, _)) = Location.unwrap decl in 
      `Assoc [
        ("declaration", `Assoc [
          ("binder", `String (Var.to_name var));
          ("content", expression exp);
          ("inline", `Bool inline);
        ]
        );
      ])
    p
   )

and expression (e : expression) : J.t =
  `Assoc [
    ("location",
      `String (Format.asprintf "%a" Location.pp e.location)
    ) ;
    ("type_expression",
      `String (Format.asprintf "%a" PP.type_expression e.type_expression)
    ) ;
    ("expression",
      (expression_content e.expression_content)
    ) ;
  ]

and expression_content (c : expression_content) : J.t =
  let form t v = `Assoc [ ("type", t) ; ("value", v) ] in
  match c with
  | E_literal l ->
    form (`String "literal") (literal l)
  | E_constant c ->
    form (`String "constant") (constant c)
  | E_variable v ->
    form (`String "variable") (variable v)
  | E_application app ->
    form (`String "application") (application app)
  | E_let_in l ->
    form (`String "let_in") (let_in l)
  | E_record lmap ->
    if Stage_common.Helpers.is_tuple_lmap lmap then form (`String "tuple") (tuple lmap)
    else form (`String "record") (record lmap)
  | E_record_accessor ra ->
    form (`String "accessor") (accessor ra)
  | E_constructor c ->
    form (`String "constructor") (constructor c)
  | E_record_update u ->
    form (`String "record_update") (record_update u)
  | E_map m ->
    form (`String "map") (map m)
  | E_big_map m ->
    form (`String "big_map") (map m)
  | E_look_up d ->
    form (`String "look_up") (look_up d)
  | E_list l ->
    form (`String "list") (list_set l)
  | E_set s ->
    form (`String "set") (list_set s)
  | E_lambda l ->
    form (`String "lambda") (lambda l)
  | E_matching m ->
    form (`String "matching") (matching m)

and matching ({matchee;cases}:matching) : J.t =
  let cases' = match cases with
    | Match_bool {match_true; match_false} -> `Assoc [
      ("match_true", expression match_true);
      ("match_false", expression match_false);
    ]
    | Match_list {match_nil;match_cons=(headn,tailn,match_l,_)} -> `Assoc [
      ("match_nil", expression match_nil);
      ("match_cons", `Assoc [
        ("head_projected_name", `String (Var.to_name headn));
        ("tail_projected_name", `String (Var.to_name tailn));
        ("body", expression match_l);
      ])
    ]
    | Match_option {match_none; match_some=(somen,match_l,_)} -> `Assoc [
      ("match_none", expression match_none);
      ("match_some", `Assoc [
        ("projected_name", `String (Var.to_name somen));
        ("body", expression match_l);
      ])
    ]
    | Match_variant (case_list,_) ->
      let case_list' = List.map
        (fun ((constructor,name),body) ->
          let (Constructor constructor') = constructor in
          `Assoc [
            ("match_constructor", `String constructor');
            ("projected_name", `String (Var.to_name name));
            ("body", expression body);
          ]
        )
        case_list in
      `List case_list'
    | Match_tuple _ -> `String "unsupported"
  in
  `Assoc [
    ("matchee", expression matchee);
    ("cases", cases');
  ]

and lambda ({binder; result}:lambda) : J.t =
  `Assoc [
    ("body", expression result);
    ("parameter_name", `String (Var.to_name binder));
  ]

and look_up ((data_structure, indice) : expression * expression) : J.t =
  `Assoc [
    ("data_structure", expression data_structure);
    ("indice", expression indice);
  ]

and list_set (l: expression list) : J.t =
  let l' = List.map (fun el -> expression el) l in
  `Assoc [
    ("elements", `List l');
  ]

and map (kvl: (expression *expression) list) : J.t =
  let kvl' = List.map (fun (k,v) -> `Assoc [("key", expression k); ("value", expression v)]) kvl in
  `Assoc [
    ("elements", `List kvl');
  ]

and record_update ({record ; path; update}: update) : J.t =
  let (Label p) = path in
  `Assoc [
    ("record", expression record);
    ("path", `String p);
    ("update", expression update);
  ]

and accessor ({expr ; label}: accessor) : J.t =
  let (Label l) = label in
  `Assoc [
    ("record", expression expr);
    ("label", `String l);
  ]

and constructor ({constructor ; element}: constructor) : J.t =
  let (Constructor cn) = constructor in
  `Assoc [
    ("name", `String cn);
    ("element", expression element)
  ]

and tuple (lmap : expression label_map) : J.t =
  let elements = `List (List.map (fun (_,v) -> expression v) (LMap.bindings lmap)) in
  `Assoc [
    ("elements", elements);
  ]

and record (lmap : expression label_map) : J.t =
  let elements = `List
    (List.map (fun (k,v) ->
      let (Label i) = k in
      `Assoc [
        ("field_name", `String i);
        ("value", expression v)]
    )
    (LMap.bindings lmap)) in
  `Assoc [
    ("elements", elements);
  ]

and let_in ({let_binder; rhs; let_result; inline}: let_in) : J.t =
  `Assoc [
    ("binder", `String (Var.to_name let_binder));
    ("rhs", expression rhs);
    ("result", expression let_result);
    ("inline", `Bool inline);
  ]

and literal (l : literal) : J.t = match l with
  | Literal_unit -> `String "unit"
  | Literal_void -> `String "void"
  | Literal_bool b -> `Bool b
  | Literal_int i | Literal_nat i | Literal_mutez i -> `Int i
  | Literal_timestamp t ->
    let notation = Memory_proto_alpha.Protocol.Alpha_context.Timestamp.to_notation
                   @@ Tezos_utils.Time.Protocol.of_seconds (Int64.of_int t) in
    `String notation
  | Literal_string s | Literal_address s
  | Literal_signature s | Literal_key_hash s
  | Literal_chain_id s | Literal_key s ->
    `String s
  | Literal_bytes b ->
    `String (Bytes.to_string b)
  | Literal_operation _o ->
    `String "TODO"

and constant (c: constant) : J.t =
  let name = `String (Format.asprintf "%a" PP.constant c.cons_name) in
  let arguments = `List (List.map expression c.arguments) in
  `Assoc [
    ("name", name);
    ("arguments", arguments);
  ] *)

let variable (v: expression_variable) : J.t =
  `Assoc [
    ("identifier", `String (Var.to_name v));
  ]

(* and application ({expr1 ; expr2}: application) : J.t =
  `Assoc [
    ("function", expression expr1);
    ("argument", expression expr2);
  ] *)