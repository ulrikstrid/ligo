type name
type type_name
type constructor_name

type 'a name_map
type 'a type_name_map

(* Short Name for expression and type_expression and map of those types*)
type expr 
type te
type te_map
type expr_map

type program

type type_expression =
  | T_tuple of te list
  | T_sum of te_map
  | T_record of te_map
  | T_function of te * te
  | T_variable of type_name
  | T_constant of type_name * te list

type expression

type declaration =
  | Declaration_type of (type_name * type_expression)
  | Declaration_constant of (name * type_expression option * expression)

type literal

type lambda
type let_in

type access
type access_path
type expression' =
  (* Base *)
  | E_literal of literal
  | E_constant of (name * expr list) (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of name
  | E_lambda of lambda
  | E_application of (expr * expr)
  | E_let_in of let_in
  (* E_Tuple *)
  | E_tuple of expr list
  (* Sum *)
  | E_constructor of (name * expr) (* For user defined constructors *)
  (* E_record *)
  | E_record of expr_map
  | E_accessor of (expr * access_path)
  (* Data Structures *)
  | E_map of (expr * expr) list
  | E_list of expr list
  | E_set of expr list
  | E_look_up of (expr * expr)
  (* Matching *)
  | E_matching of (expr * matching_expr)
  | E_failwith of expr
  (* Replace Statements *)
  | E_sequence of (expr * expr)
  | E_loop of (expr * expr)
  | E_assign of (name * access_path * expr)
  | E_skip
  (* Annotate *)
  | E_annotation of expr * type_expression


type 'a matching
type matching_expr
