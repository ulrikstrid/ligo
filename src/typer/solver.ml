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

  type label_pattern =
    | L_int of int
    | L_string of string

  type type_pattern =
    | P_forall   of (type_variable * type_pattern)
    | P_variable of type_variable
    | P_constant of (constant_tag * type_pattern list)
    | P_label    of (type_pattern * label_pattern)

  type type_constraint =
    (* | C_assignment of (type_variable * type_pattern) *)
    | C_equation of (type_pattern * type_pattern) (* PA = PB *)

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

  let type_expression_to_pattern : I.type_expression -> O.type_pattern = fun te ->
    match te with
    | _ -> P_variable ""

  let type_declaration : I.type_declaration -> constraints = fun td ->
    let (name , expression) = td in
    let pattern = type_expression_to_pattern expression in
    [C_equation (P_variable (name) , pattern)]

  let failwith : unit -> (constraints * O.type_variable) = fun () ->
    let type_name = Core.fresh_type_variable () in
    [] , type_name

  let variable : I.name -> I.type_expression -> (constraints * O.type_variable) = fun _name expr ->
    let pattern = type_expression_to_pattern expr in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let literal : I.type_expression -> (constraints * O.type_variable) = fun t ->
    let pattern = type_expression_to_pattern t in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let literal_bool : unit -> (constraints * O.type_variable) = fun () ->
    let pattern = type_expression_to_pattern I.t_bool in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let literal_string : unit -> (constraints * O.type_variable) = fun () ->
    let pattern = type_expression_to_pattern I.t_string in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let tuple : I.type_expression list -> (constraints * O.type_variable) = fun tys ->
    let patterns = List.map type_expression_to_pattern tys in
    let pattern = O.(P_constant (C_tuple , patterns)) in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  (* TODO: I think we should take an I.expression for the base+label *)
  let access_label ~base ~label : (constraints * O.type_variable) =
    let base' = type_expression_to_pattern base in
    let left_pattern = O.P_label (base' , label) in
    let right_pattern = O.P_variable (O.fresh_type_variable ()) in
    let expr_type = Core.fresh_type_variable () in
    [O.C_equation (left_pattern , right_pattern)] , expr_type

  let access_int ~base ~index = access_label ~base ~label:(L_int index)
  let access_string ~base ~property = access_label ~base ~label:(L_string property)

  let access_map : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun base key ->
    let base' = type_expression_to_pattern base in
    let left_pattern = O.P_constant (variable "map" , elementtype) in
    let right_pattern = O.P_variable (O.fresh_type_variable ()) in
    let expr_type = Core.fresh_type_variable () in
    [O.C_equation ((*base_type*) left_pattern , right_pattern);
     O.C_equation (expr_type , _pattern)] , expr_type

    let patterns = List.map type_expression_to_pattern tys in
    let pattern = O.(P_constant (C_tuple , patterns)) in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name
end

open Core

type state = type_constraint list
let initial_state : state = []
let aggregate_constraints : state -> type_constraint list -> state result = fun a b -> ok (a @ b)
