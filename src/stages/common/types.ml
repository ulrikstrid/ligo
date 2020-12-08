include Enums

type location = Location.t [@@deriving to_yojson]
type 'a location_wrap = 'a Location.wrap [@@deriving to_yojson]

type attributes = string list [@@deriving to_yojson]

type expression_
and expression_variable = expression_ Var.t Location.wrap
let expression_variable_to_yojson var = Location.wrap_to_yojson (Var.to_yojson) var
let expression_variable_of_yojson var = Location.wrap_of_yojson (Var.of_yojson) var
type type_
and type_variable = type_ Var.t
let type_variable_to_yojson var = Var.to_yojson var
let type_variable_of_yojson var = Var.of_yojson var

type label = Label of string [@@deriving yojson]
let label_to_yojson (Label l) = `List [`String "Label"; `String l]
let label_of_yojson = function
  | `List [`String "Label"; `String l] -> Ok (Label l)
  | _ -> Utils.error_yojson_format "Label of string"


module LMap = struct
  include Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)
  let to_yojson = label_to_yojson
  let of_yojson = label_of_yojson
end
type 'a label_map = 'a LMap.t (* [@@deriving yojson] *)

let const_name = function
  | Deprecated {const;_} -> const
  | Const      const     -> const
let bindings_to_yojson f g xs = `List (List.map (fun (x,y) -> `List [f x; g y]) xs)
let label_map_to_yojson row_elem_to_yojson m =
  bindings_to_yojson label_to_yojson row_elem_to_yojson (LMap.bindings m)
let label_map_of_yojson _ _ = LMap.empty

type 'ty_expr row_element_mini_c = {
  associated_type      : 'ty_expr ;
  michelson_annotation : string option ;
  decl_pos : int ;
} [@@deriving to_yojson]

type 'ty_exp type_app = {
  type_operator : type_variable ;
  arguments     : 'ty_exp list ;
} [@@deriving to_yojson]

type 'ty_expr row_element = {
  associated_type : 'ty_expr ;
  attributes      : string list ;
  decl_pos        : int ;
} [@@deriving to_yojson]

type 'a module_access = {
  module_name : string ;
  element     : 'a ;
} [@@deriving to_yojson]

(* Type level types *)
type 'ty_exp rows = {
  fields     : 'ty_exp row_element label_map;
  attributes : string list ;
} [@@deriving to_yojson]

type 'ty_exp arrow = {
  type1: 'ty_exp ;
  type2: 'ty_exp ;
} [@@deriving to_yojson]

(* Expression level types *)
type 'ty_exp binder = {
  var  : expression_variable ;
  ascr : 'ty_exp option;
} [@@deriving to_yojson]


type 'exp application = {
  lamb: 'exp ;
  args: 'exp ;
} [@@deriving to_yojson]

type 'exp constant = {
  cons_name: constant' ; (* this is in enum *)
  arguments: 'exp list ;
} [@@deriving to_yojson]

type ('exp,'ty_exp) lambda = {
  binder: 'ty_exp binder ;
  output_type : 'ty_exp option;
  result: 'exp ;
} [@@deriving to_yojson]

type ('exp, 'ty_exp) recursive = {
  fun_name :  expression_variable ;
  fun_type : 'ty_exp ;
  lambda   : ('exp, 'ty_exp) lambda ;
} [@@deriving to_yojson]

type ('exp, 'ty_exp) let_in = {
  let_binder: 'ty_exp binder ;
  rhs       : 'exp ;
  let_result: 'exp ;
  attributes: attributes ;
} [@@deriving to_yojson]

type ('exp, 'ty_exp) type_in = {
  type_binder: type_variable ;
  rhs        : 'ty_exp ;
  let_result : 'exp ;
} [@@deriving to_yojson]

type 'exp raw_code = {
  language : string ;
  code : 'exp ;
} [@@deriving to_yojson]

type 'exp constructor = {constructor: label; element: 'exp} [@@deriving to_yojson]

type 'exp access =
  | Access_tuple of z
  | Access_record of string
  | Access_map of 'exp [@@deriving to_yojson]

type 'exp accessor = {record: 'exp; path: 'exp access list} [@@deriving to_yojson]
type 'exp update   = {record: 'exp; path: 'exp access list; update: 'exp} [@@deriving to_yojson]

type 'exp record_accessor = {record: 'exp; path: label} [@@deriving to_yojson]
type 'exp record_update   = {record: 'exp; path: label; update: 'exp} [@@deriving to_yojson]

type ('exp,'ty_exp) ascription = {anno_expr: 'exp; type_annotation: 'ty_exp} [@@deriving to_yojson]

type 'exp conditional = {
  condition   : 'exp ;
  then_clause : 'exp ;
  else_clause : 'exp ;
} [@@deriving to_yojson]

and 'exp sequence = {
  expr1: 'exp ;
  expr2: 'exp ;
} [@@deriving to_yojson]

and 'exp assign = {
  variable    : expression_variable ;
  access_path : 'exp access list ;
  expression  : 'exp ;
} [@@deriving to_yojson]

and 'exp for_ = {
  binder : expression_variable ;
  start  : 'exp ;
  final  : 'exp ;
  incr   : 'exp ;
  f_body : 'exp ;
} [@@deriving to_yojson]

and 'exp for_each = {
  fe_binder : expression_variable * expression_variable option ;
  collection : 'exp ;
  collection_type : collect_type ;
  fe_body : 'exp ;
} [@@deriving to_yojson]

and collect_type =
  | Map
  | Set
  | List [@@deriving to_yojson]

and 'exp while_loop = {
  cond : 'exp ;
  body : 'exp ;
} [@@deriving to_yojson]

(* Declaration types *)
type 'ty_exp declaration_type = {
  type_binder : type_variable ;
  type_expr : 'ty_exp ;
} [@@deriving to_yojson]

type ('exp,'ty_exp) declaration_constant = {
  binder : 'ty_exp binder;
  attr : attributes ;
  expr : 'exp ;
} [@@deriving to_yojson]

(* Program types *)

type 'dec program' = 'dec location_wrap list [@@deriving to_yojson]
