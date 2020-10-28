open Ast

type t = environment
type element = environment_element

val empty : t
val add_ez_binder : expression_variable -> type_expression -> t -> t
val add_ez_declaration : expression_variable -> expression -> t -> t
val add_module : string -> environment -> t -> t
val add_type : type_variable -> type_expression -> t -> t
val of_list_type : (type_variable * type_expression) list -> t
val get_opt : expression_variable -> t -> element option
val get_type_opt : type_variable -> t -> type_expression option
val get_module_opt : string -> t -> environment option
val get_constructor : label -> t -> (type_expression * type_expression) option
val get_record : row_element label_map -> t -> (type_variable option * rows) option
val get_sum : row_element label_map -> t -> rows option

module PP : sig
  open Format

  val list_sep_scope : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  val environment : formatter -> environment -> unit

end
