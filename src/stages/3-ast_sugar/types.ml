[@@@warning "-30"]

module Location = Simple_utils.Location

include Stage_common.Types

type type_content =
  | T_sum of row_element label_map
  | T_record of row_element label_map
  | T_tuple  of type_expression list
  | T_arrow of arrow
  | T_variable of type_variable
  | T_wildcard
  | T_constant of (type_constant * type_expression list)

and arrow = {type1: type_expression; type2: type_expression}

and row_element = {associated_type : type_expression ; michelson_annotation : string option ; decl_pos : int}

and type_expression = {type_content: type_content; location: Location.t}


type program = declaration Location.wrap list

and declaration =
  | Declaration_type of (type_variable * type_expression)
  (* A Declaration_constant is described by
   *   a name
   *   an optional type annotation
   *   a boolean indicating whether it should be inlined
   *   an expression *)
  | Declaration_constant of (expression_variable * type_expression * bool * expression)

(* | Macro_declaration of macro_declaration *)
and expression = {expression_content: expression_content; location: Location.t}

and expression_content =
  (* Base *)
  | E_literal of literal
  | E_constant of constant (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_application of application
  | E_lambda of lambda
  | E_recursive of recursive
  | E_let_in of let_in
  | E_raw_code of raw_code
  (* Variant *)
  | E_constructor of constructor (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expression label_map
  | E_accessor of accessor
  | E_update   of update
  (* Advanced *)
  | E_ascription of ascription
  (* Sugar *)
  | E_cond of conditional
  | E_sequence of sequence
  | E_skip
  | E_tuple of expression list
  (* Data Structures *)
  | E_map of (expression * expression) list 
  | E_big_map of (expression * expression) list
  | E_list of expression list
  | E_set of expression list
  (* Modules *)
  |E_import of import

and constant =
  { cons_name: constant' (* this is at the end because it is huge *)
  ; arguments: expression list }

and application = {
  lamb: expression ; 
  args: expression ;
}

and lambda = {
  binder: (expression_variable, type_expression) binder; 
  result: expression 
}

and recursive = {
  fun_name :  expression_variable;
  fun_type : type_expression;
  lambda : lambda;
}

and let_in = { 
  let_binder: (expression_variable, type_expression) binder ;
  rhs: expression ;
  let_result: expression ;
  inline: bool ;
  mut: bool;
}

and raw_code = { 
  language : string ;
  code : expression ;
}

and constructor = {constructor: label; element: expression}
and accessor = {record: expression; path: access list}
and update   = {record: expression; path: access list ; update: expression}

and access =
  | Access_tuple of z
  | Access_record of string
  | Access_map of expression

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
  | Match_tuple of (expression_variable * type_expression) list  * expression
  | Match_record of (label * expression_variable * type_expression) list * expression
  | Match_variable of (expression_variable * type_expression ) * expression

and matching =
  { matchee: expression
  ; cases: matching_expr
  }

and ascription = {anno_expr: expression; type_annotation: type_expression}

and conditional = {
  condition : expression ;
  then_clause : expression ;
  else_clause : expression ;
}

and sequence = {
  expr1: expression ;
  expr2: expression ;
}

and import = { path : string * string list }
