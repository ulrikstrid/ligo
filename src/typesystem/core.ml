  type type_variable = string

  let fresh_type_variable : ?name:string -> unit -> type_variable =
    let id = ref 0 in
    let inc () = id := !id + 1 in
    fun ?name () ->
      inc () ;
      match name with
      | None -> "type_variable_" ^ (string_of_int !id)
      | Some name -> "tv_" ^ name ^ "_" ^ (string_of_int !id)

  type constant_tag =
    | C_arrow     (* * -> * -> * *)
    | C_option    (* * -> * *)
    | C_tuple     (* * … -> * *)
    | C_record    (* ( label , * ) … -> * *)
    | C_variant   (* ( label , * ) … -> * *)
    | C_map       (* * -> * -> * *)
    | C_list      (* * -> * *)
    | C_set       (* * -> * *)
    | C_unit      (* * *)
    | C_bool      (* * *)
    | C_string    (* * *)
    | C_nat       (* * *)
    | C_tez       (* * *)
    | C_timestamp (* * *)
    | C_int       (* * *)
    | C_address   (* * *)
    | C_bytes     (* * *)
    | C_key_hash  (* * *)
    | C_key       (* * *)
    | C_signature (* * *)
    | C_operation (* * *)
    | C_contract  (* * -> * *)

  type label =
    | L_int of int
    | L_string of string

  type type_value =
    | P_forall   of (type_variable * type_constraint list * type_value)
    | P_variable of type_variable
    | P_constant of (constant_tag * type_value list)
    | P_label    of (type_value * label)


  and type_constraint =
    (* | C_assignment of (type_variable * type_pattern) *)
    | C_equation of (type_value * type_value) (* TVA = TVB *)
    | C_typeclass of (type_value * typeclass) (* TV ∈ TVL, for now in extension, later add intensional (rule-based system for inclusion in the typeclass) *)
    (* | … *)

  and typeclass = type_value list
