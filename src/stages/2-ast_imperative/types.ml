[@@@warning "-30"]

module Location = Simple_utils.Location

include Stage_common.Types

type type_content =
  | T_variable        of type_variable
  | T_sum             of ty_expr rows
  | T_record          of ty_expr rows
  | T_tuple           of ty_expr  list
  | T_arrow           of ty_expr arrow
  | T_annoted         of (type_expression * string)
  | T_app             of ty_expr type_app
  | T_module_accessor of ty_expr module_access
[@@deriving to_yojson]


and type_expression = {type_content: type_content; location: Location.t} [@@deriving to_yojson]
and ty_expr = type_expression [@@deriving to_yojson]

type program = declaration program' [@@deriving to_yojson]
and declaration =
  | Declaration_type of ty_expr declaration_type
  (* A Declaration_constant is described by
   *   a name
   *   an optional type annotation
   *   attributes
   *   an expression *)
  | Declaration_constant of (expr,ty_expr) declaration_constant
[@@deriving to_yojson]

(* | Macro_declaration of macro_declaration *)
and expression = {expression_content: expression_content; location: Location.t}
[@@deriving to_yojson]
and expr = expression
[@@deriving to_yojson]

and expression_content =
  (* Base *)
  | E_literal of literal
  | E_constant of constant (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_application of expr application
  | E_lambda of (expr, ty_expr) lambda
  | E_recursive of (expr, ty_expr) recursive
  | E_let_in of (expr, ty_expr) let_in
  | E_type_in of (expr, ty_expr) type_in
  | E_raw_code of expr raw_code
  (* Variant *)
  | E_constructor of expr constructor (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expression label_map
  | E_accessor of expr accessor
  | E_update   of expr update
  (* Advanced *)
  | E_ascription of (expr, ty_expr) ascription
  | E_module_accessor of expr module_access
  (* Sugar *)
  | E_cond of expr conditional
  | E_sequence of expr sequence
  | E_skip
  | E_tuple of expression list
  (* Data Structures *)
  | E_map of (expression * expression) list
  | E_big_map of (expression * expression) list
  | E_list of expression list
  | E_set of expression list
  (* Imperative *)
  | E_assign   of expr assign
  | E_for      of expr for_
  | E_for_each of expr for_each
  | E_while    of expr while_loop
[@@deriving to_yojson]

and constant =
  { cons_name: rich_constant (* this is at the end because it is huge *)
  ; arguments: expression list }
[@@deriving to_yojson]



and matching_expr =
  | Match_variant of ((label * expression_variable) * expression) list
  | Match_list of {
      match_nil  : expression ;
      match_cons : expression_variable * expression_variable * expression ;
    }
  | Match_option of {
      match_none : expression ;
      match_some : expression_variable * expression ;
    }
  | Match_tuple of ty_expr binder list  * expression
  | Match_record of (label * ty_expr binder) list * expression
  | Match_variable of ty_expr binder * expression
[@@deriving to_yojson]

and matching =
  { matchee: expression
  ; cases: matching_expr
  }
[@@deriving to_yojson]

and environment_element_definition =
  | ED_binder
  | ED_declaration of (expression * free_variables)
[@@deriving to_yojson]

and free_variables = expression_variable list
[@@deriving to_yojson]

and environment_element =
  { type_value: type_expression
  ; source_environment: environment
  ; definition: environment_element_definition }
[@@deriving to_yojson]

and expr_environment = (expression_variable * environment_element) list
[@@deriving to_yojson]
and type_environment = (type_variable * type_expression) list
[@@deriving to_yojson]

(* SUBST ??? *)
and environment = expr_environment * type_environment
[@@deriving to_yojson]
