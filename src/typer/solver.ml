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

  type label_pattern =
    | L_int of int
    | L_string of string
  
  type type_pattern =
    | P_variable of type_variable
    | P_constant of (constant_tag * type_pattern list)
    | P_label of (type_pattern * label_pattern)

  type type_constraint = 
    (* | C_assignment of (type_variable * type_pattern) *)
    | C_equation of (type_pattern * type_pattern) (* PA = PB *)

end

module Wrap = struct

  open Ast_simplified
  open Core

  type constraints = type_constraint list
  
  let type_expression_to_pattern : type_expression -> Core.type_pattern = fun te ->
    match te with
    | _ -> P_variable ""

  let type_declaration : type_declaration -> constraints = fun td ->
    let (name , expression) = td in
    let pattern = type_expression_to_pattern expression in
    [C_equation (P_variable (name) , pattern)]

  let variable : type_variable -> type_expression -> constraints = fun name expr ->
    let pattern = type_expression_to_pattern expr in
    [C_equation (P_variable (name) , pattern)]

  let access_label ~base ~label =
    let base' = type_expression_to_pattern base in
    let left_pattern = P_label (base' , label) in
    let right_pattern = P_variable (fresh_type_variable ()) in
    [C_equation (left_pattern , right_pattern)]
  
  let access_int ~base ~index = access_label ~base ~label:(L_int index)
  let access_string ~base ~property = access_label ~base ~label:(L_string property)
  
end

open Core

type state = type_constraint list
let initial_state : state = []
let aggregate_constraints : state -> type_constraint list -> state result = fun a b -> ok (a @ b)
