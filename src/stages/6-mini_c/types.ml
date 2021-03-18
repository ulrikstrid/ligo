include Stage_common.Types

type 'a annotated = string option * 'a

type type_content =
  | T_pair of (type_expression annotated * type_expression annotated)
  | T_or of (type_expression annotated * type_expression annotated)
  | T_function of (type_expression * type_expression)
  | T_base of type_base
  | T_map of (type_expression * type_expression)
  | T_big_map of (type_expression * type_expression)
  | T_list of type_expression
  | T_set of type_expression
  | T_contract of type_expression
  | T_ticket of type_expression
  | T_sapling_state of Z.t
  | T_sapling_transaction of Z.t
  | T_option of type_expression

and type_expression = {
  type_content : type_content;
  location : Location.t;
}

and type_base =
  | TB_unit
  | TB_bool
  | TB_string
  | TB_bytes
  | TB_nat
  | TB_int
  | TB_mutez
  | TB_operation
  | TB_address
  | TB_key
  | TB_key_hash
  | TB_chain_id
  | TB_signature
  | TB_timestamp
  | TB_baker_hash
  | TB_pvss_key
  | TB_baker_operation
  | TB_bls12_381_g1
  | TB_bls12_381_g2
  | TB_bls12_381_fr
  | TB_never

and environment_element = expression_variable * type_expression

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
  | D_nat of Z.t
  | D_timestamp of Z.t
  | D_mutez of Z.t
  | D_int of Z.t
  | D_string of string
  | D_bytes of bytes
  | D_pair of value * value
  | D_left of value
  | D_right of value
  | D_some of value
  | D_none
  | D_map of (value * value) list
  | D_big_map of (value * value) list
  | D_ticket of (value * value)
  | D_list of value list
  | D_set of value list
  (* | `Macro of anon_macro ... The future. *)
  | D_operation of bytes

and selector = var_name list

and expression_content =
  | E_literal of literal
  | E_closure of anon_function
  | E_constant of constant
  | E_application of (expression * expression)
  | E_variable of var_name
  | E_iterator of constant' * ((var_name * type_expression) * expression) * expression
  | E_fold     of (((var_name * type_expression) * expression) * expression * expression)
  | E_fold_right of (((var_name * type_expression) * expression) * (expression * type_expression) * expression)
  | E_if_bool  of (expression * expression * expression)
  | E_if_none  of expression * expression * ((var_name * type_expression) * expression)
  | E_if_cons  of expression * expression * (((var_name * type_expression) * (var_name * type_expression)) * expression)
  | E_if_left  of expression * ((var_name * type_expression) * expression) * ((var_name * type_expression) * expression)
  | E_let_in   of expression * inline * ((var_name * type_expression) * expression)
  | E_let_tuple of expression * (((var_name * type_expression) list) * expression)
  | E_raw_michelson of (Location.t, string) Tezos_micheline.Micheline.node list

and expression = {
  content : expression_content ;
  type_expression : type_expression ;
  location : Location.t;
}

and constant = {
  cons_name : constant';
  arguments : expression list;
}

and assignment = var_name * inline * expression

and toplevel_statement = assignment * environment_wrap

and anon_function = {
  binder : expression_variable ;
  body : expression ;
}

and program = toplevel_statement list
