[@@@warning "-30"]
module Map = Simple_utils.Map
module Location = Simple_utils.Location

type name = string
type type_name = string
type constructor_name = string

type 'a name_map = 'a Map.String.t
type 'a type_name_map = 'a Map.String.t

type program = declaration Location.wrap list

and type_declaration = (type_name * type_expression)

and declaration =
  | Declaration_type of type_declaration
  | Declaration_constant of (name * type_expression option * expression)
  (* | Macro_declaration of macro_declaration *)

and expr = expression
and te = type_expression
and te_map = type_expression type_name_map
and expr_map = expression name_map

and 'a type_expression_ast =
  | T_tuple of 'a list
  | T_sum of 'a type_name_map
  | T_record of 'a type_name_map
  | T_function of ('a * 'a)
  | T_variable of type_name
  | T_constant of (type_name * 'a list)

and type_expression = {
  type_expression' : type_expression'
}

and type_expression' = type_expression type_expression_ast

and lambda = {
  binder : (name * type_expression option) ;
  input_type : type_expression option ;
  output_type : type_expression option ;
  result : expr ;
}

and let_in = {
  binder : (name * type_expression option) ;
  rhs    : expr ;
  result : expr ;
}

and 'a expression_ast =
  (* Base *)
  | E_literal of literal
  | E_constant of (name * 'a list) (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of name
  | E_lambda of lambda
  | E_application of ('a * 'a)
  | E_let_in of let_in
  (* E_Tuple *)
  | E_tuple of 'a list
  (* Sum *)
  | E_constructor of (name * 'a) (* For user defined constructors *)
  (* E_record *)
  | E_record of 'a name_map
  | E_accessor of ('a * access)
  (* Data Structures *)
  | E_map of (expr * expr) list
  | E_big_map of (expr * expr) list
  | E_list of expr list
  | E_set of expr list
  | E_look_up of (expr * expr)
  (* Matching *)
  | E_matching of ('a * 'a matching)
  | E_failwith of 'a
  (* Replace Statements *)
  | E_sequence of ('a * 'a)
  | E_loop of ('a * 'a)
  | E_assign of (name * access_path * 'a)
  | E_skip
  (* Annotate *)
  | E_annotation of 'a * type_expression

and expression = {
  expression : expression' ;
  location : Location.t ;
}

and access =
  | Access_tuple of int
  | Access_record of string
  | Access_map of expr

and access_path = access list

and literal =
  | Literal_unit
  | Literal_bool of bool
  | Literal_int of int
  | Literal_nat of int
  | Literal_mutez of int
  | Literal_string of string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_timestamp of int
  | Literal_operation of Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation

and 'a matching =
  | Match_bool of {
      match_true : 'a ;
      match_false : 'a ;
    }
  | Match_list of {
      match_nil : 'a ;
      match_cons : name * name * 'a ;
    }
  | Match_option of {
      match_none : 'a ;
      match_some : name * 'a ;
    }
  | Match_tuple of name list * 'a
  | Match_variant of ((constructor_name * name) * 'a) list

and matching_expr = expression matching
