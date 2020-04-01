
include Stage_common.Types

type 'a annotated = string option * 'a

type type_value =
  | T_pair of (type_value annotated * type_value annotated)
  | T_or of (type_value annotated * type_value annotated)
  | T_function of (type_value * type_value)
  | T_base of type_constant
  | T_map of (type_value * type_value)
  | T_big_map of (type_value * type_value)
  | T_list of type_value
  | T_set of type_value
  | T_contract of type_value
  | T_option of type_value

and environment_element = expression_variable * type_value

and environment = environment_element list

and environment_wrap = {
  pre_environment : environment ;
  post_environment : environment ;
}

and var_name = expression_variable
and fun_name = expression_variable

type inline = bool

type value =
  | D_unit
  | D_bool of bool
  | D_nat of int
  | D_timestamp of int
  | D_mutez of int
  | D_int of int
  | D_string of string
  | D_bytes of bytes
  | D_pair of value * value
  | D_left of value
  | D_right of value
  | D_some of value
  | D_none
  | D_map of (value * value) list
  | D_big_map of (value * value) list
  | D_list of value list
  | D_set of value list
  (* | `Macro of anon_macro ... The future. *)
  | D_operation of Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation

and selector = var_name list

and expression' =
  | E_literal of value
  | E_closure of anon_function
  | E_skip
  | E_constant of constant
  | E_application of (expression * expression)
  | E_variable of var_name
  | E_make_none of type_value
  | E_iterator of constant' * ((var_name * type_value) * expression) * expression
  | E_fold of (((var_name * type_value) * expression) * expression * expression)
  | E_if_bool of (expression * expression * expression)
  | E_if_none of expression * expression * ((var_name * type_value) * expression)
  | E_if_cons of (expression * expression * (((var_name * type_value) * (var_name * type_value)) * expression))
  | E_if_left of expression * ((var_name * type_value) * expression) * ((var_name * type_value) * expression)
  | E_let_in of ((var_name * type_value) * inline * expression * expression)
  | E_sequence of (expression * expression)
  | E_record_update of (expression * [`Left | `Right] list * expression)
  | E_while of (expression * expression)

and expression = {
  content : expression' ;
  type_value : type_value ;
}

and constant = {
  cons_name : constant'; (* this is at the end because it is huge *)
  arguments : expression list;
}

and assignment = var_name * inline * expression

and toplevel_statement = assignment * environment_wrap

and anon_function = {
  binder : expression_variable ;
  body : expression ;
}

and program = toplevel_statement list