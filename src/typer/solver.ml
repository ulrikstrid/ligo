open Trace

module Core = struct

  type type_variable = string
  let fresh_type_variable =
    let id = ref 0 in
    let inc () = id := !id + 1 in
    fun ?name () ->
      inc () ;
      match name with
      | None -> "type_variable_" ^ (string_of_int !id)
      | Some name -> "tv_" ^ name ^ "_" ^ (string_of_int !id)

  type constant_tag =
    | C_application
    | C_tuple
    | C_map
    | C_list
    | C_set

  type label =
    | L_int of int
    | L_string of string

  type type_value =
    | P_forall   of (type_variable * type_value)
    | P_variable of type_variable
    | P_constant of (constant_tag * type_value list)
    | P_label    of (type_value * label)

  type type_constraint =
    (* | C_assignment of (type_variable * type_pattern) *)
    | C_equation of (type_value * type_value) (* TVA = TVB *)

end

module Wrap = struct
  let aa = 42

  module Local = struct
  module I = Ast_simplified
  module O = Core
  end
  open Local

  type constraints = O.type_constraint list

  (* let add_type state t = *)
  (*   let constraints = Wrap.variable type_name t in *)
  (*   let%bind state' = aggregate_constraints state constraints in *)
  (*   ok state' in *)
  (* let return_add_type ?(state = state) expr t = *)
  (*   let%bind state' = add_type state t in *)
  (*   return expr state' in *)

  let type_expression_to_type_value : I.type_expression -> O.type_value = fun te ->
    match te with
    | _ -> P_variable ""

  let type_declaration : I.type_declaration -> constraints = fun td ->
    let (name , expression) = td in
    let pattern = type_expression_to_type_value expression in
    [C_equation (P_variable (name) , pattern)]

  let failwith : unit -> (constraints * O.type_variable) = fun () ->
    let type_name = Core.fresh_type_variable () in
    [] , type_name

  let variable : I.name -> I.type_expression -> (constraints * O.type_variable) = fun _name expr ->
    let pattern = type_expression_to_type_value expr in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let literal : I.type_expression -> (constraints * O.type_variable) = fun t ->
    let pattern = type_expression_to_type_value t in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let literal_bool : unit -> (constraints * O.type_variable) = fun () ->
    let pattern = type_expression_to_type_value I.t_bool in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let literal_string : unit -> (constraints * O.type_variable) = fun () ->
    let pattern = type_expression_to_type_value I.t_string in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let tuple : I.type_expression list -> (constraints * O.type_variable) = fun tys ->
    let patterns = List.map type_expression_to_type_value tys in
    let pattern = O.(P_constant (C_tuple , patterns)) in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  (* TODO: I think we should take an I.expression for the base+label *)
  let access_label ~base ~label : (constraints * O.type_variable) =
    let base' = type_expression_to_type_value base in
    let left_pattern = O.P_label (base' , label) in
    let right_pattern = O.P_variable (O.fresh_type_variable ()) in
    let expr_type = Core.fresh_type_variable () in
    [O.C_equation (left_pattern , right_pattern)] , expr_type

  let access_int ~base ~index = access_label ~base ~label:(L_int index)
  let access_string ~base ~property = access_label ~base ~label:(L_string property)

  let access_map : base:I.type_expression -> key:I.type_expression -> (constraints * O.type_variable) =
    let mk_map_type key_type element_type =
      O.P_constant O.(C_map , [P_variable element_type; P_variable key_type]) in
    fun ~base ~key ->
    let key_type = Core.fresh_type_variable () in
    let element_type = Core.fresh_type_variable () in
    let base' = type_expression_to_type_value base in
    let key' = type_expression_to_type_value key in
    let base_expected = mk_map_type key_type element_type in
    let expr_type = Core.fresh_type_variable () in
    O.[C_equation (base' , base_expected);
       C_equation (key' , P_variable key_type);
       C_equation (P_variable expr_type , P_variable element_type)] , expr_type

  let constructor
    : I.type_expression -> I.type_expression -> I.type_expression -> (constraints * O.type_variable)
    = fun t_arg c_arg sum ->
      let t_arg = type_expression_to_type_value t_arg in
      let c_arg = type_expression_to_type_value c_arg in
      let sum = type_expression_to_type_value sum in
      let whole_expr = Core.fresh_type_variable () in
      [
        C_equation (P_variable (whole_expr) , sum) ;
        C_equation (t_arg , c_arg)
      ] , whole_expr

  let record : I.type_expression I.type_name_map -> (constraints * O.type_variable) = fun fields ->
    let record_type = type_expression_to_type_value (I.t_record fields) in
    let whole_expr = Core.fresh_type_variable () in
    [C_equation (P_variable whole_expr , record_type)] , whole_expr

  let collection : O.constant_tag -> I.type_expression list -> (constraints * O.type_variable) =
    fun ctor element_tys ->
    let elttype = O.P_variable (Core.fresh_type_variable ()) in
    let aux elt =
      let elt' = type_expression_to_type_value elt
      in O.C_equation (elttype , elt') in
    let equations = List.map aux element_tys in
    let whole_expr = Core.fresh_type_variable () in
    O.[
      C_equation (P_variable whole_expr , O.P_constant (ctor , [elttype]))
    ] @ equations , whole_expr

  let list = collection O.C_list
  let set  = collection O.C_set

  let map : (I.type_expression * I.type_expression) list -> (constraints * O.type_variable) =
    fun kv_tys ->
    let k_type = O.P_variable (Core.fresh_type_variable ()) in
    let v_type = O.P_variable (Core.fresh_type_variable ()) in
    let aux_k (k , _v) =
      let k' = type_expression_to_type_value k in
      O.C_equation (k_type , k') in
    let aux_v (_k , v) =
      let v' = type_expression_to_type_value v in
      O.C_equation (v_type , v') in
    let equations_k = List.map aux_k kv_tys in
    let equations_v = List.map aux_v kv_tys in
    let whole_expr = Core.fresh_type_variable () in
    O.[
      C_equation (P_variable whole_expr , O.P_constant (C_map , [k_type ; v_type]))
    ] @ equations_k @ equations_v , whole_expr

end

open Core

type state = type_constraint list
let initial_state : state = []
let aggregate_constraints : state -> type_constraint list -> state result = fun a b -> ok (a @ b)
