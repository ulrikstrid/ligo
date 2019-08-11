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
    | C_arrow   (* * -> * -> * *)
    | C_option  (* * -> * *)
    | C_tuple   (* * … -> * *)
    | C_record  (* ( label , * ) … -> * *)
    | C_variant (* ( label , * ) … -> * *)
    | C_map     (* * -> * -> * *)
    | C_list    (* * -> * *)
    | C_set     (* * -> * *)
    | C_unit    (* * *)
    | C_bool    (* * *)

  type label =
    | L_int of int
    | L_string of string

  type type_value =
    | P_forall   of (type_variable * type_constraint list * type_value)
    | P_variable of type_variable
    | P_constant of (constant_tag * type_value list)
    | P_label    of (type_value * label)

  and typeclass = type_value list

  and type_constraint =
    (* | C_assignment of (type_variable * type_pattern) *)
    | C_equation of (type_value * type_value) (* TVA = TVB *)
    | C_typeclass of (type_value * typeclass) (* TV ∈ TVL, for now in extension, later add intensional (rule-based system for inclusion in the typeclass) *)
    (* | … *)

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

  let rec type_expression_to_type_value : I.type_expression -> O.type_value = fun te ->
    match te.type_expression' with
    | T_tuple types ->
      P_constant (C_tuple, List.map type_expression_to_type_value types)
    | T_sum kvmap ->
      P_constant (C_variant, Map.String.to_list @@ Map.String.map type_expression_to_type_value kvmap)
    | T_record kvmap ->
      P_constant (C_record, Map.String.to_list @@ Map.String.map type_expression_to_type_value kvmap)
    | T_function (arg , ret) ->
      P_constant (C_arrow, List.map type_expression_to_type_value [ arg ; ret ])
    | T_variable type_name -> P_variable type_name
    | T_constant (type_name , args) ->
      let csttag = Core.(match type_name with
          | "arrow"  -> C_arrow
          | "option" -> C_option
          | "tuple"  -> C_tuple
          | "map"    -> C_map
          | "list"   -> C_list
          | "set"    -> C_set
          | "unit"   -> C_unit
          | "bool"   -> C_bool
          | _        -> failwith "TODO")
      in
      P_constant (csttag, List.map type_expression_to_type_value args)

  let type_declaration : I.type_declaration -> constraints = fun td ->
    let (name , expression) = td in
    let pattern = type_expression_to_type_value expression in
    [C_equation (P_variable (name) , pattern)] (* TODO: this looks wrong. If this is a type declaration, it should not set any constraints. *)

  (* TODO: this should be renamed to failwith_ *)
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

  (* tuple   : ('label:int, 'v) … -> record ('label : 'v) … *)
  (* constructor   : ('label:string, 'v) -> variant ('label : 'v) *)
  (* record        : ('label:string, 'v) … -> record ('label : 'v) … with independent choices for each 'label and 'v *)
  (* variable      : t_of_var_in_env *)
  (* access_int    : record ('label:int ,    'v) … -> 'label:int    -> 'v *)
  (* access_string : record ('label:string , 'v) … -> 'label:string -> 'v *)

  let forall binder f =
    let () = ignore binder in
    let freshvar = O.fresh_type_variable () in
    f (O.P_variable freshvar)
  let forall2 a b f =
    forall a @@ fun a' ->
    forall b @@ fun b' ->
    f a' b'
  let forall3 a b c f =
    forall a @@ fun a' ->
    forall b @@ fun b' ->
    forall c @@ fun c' ->
    f a' b' c'
  let pair a b = O.P_constant O.(C_tuple , [a; b])
  let map  k v = O.P_constant O.(C_map   , [k; v])
  let (-->) arg ret = O.P_constant O.(C_arrow  , [arg; ret])

  let t_mapcons = forall2 "k" "v" @@ fun k v -> pair k v --> map  k v --> map  k v

  (* cons          : ∀ v, v -> list v -> list v            was: list *)
  (* setcons       : ∀ v, v -> set v  -> set v             was: set  *)
  (* mapcons       : ∀ k v, (k * v) -> map k v -> map k v  was: map  *)
  (* failwith      : ∀ a, a *)
  (* literal_t     : t *)
  (* literal_bool  : bool *)
  (* literal_string : string *)
  (* access_map    : ∀ k v, map k v -> k -> v              *)
  (* application   : ∀ a b, (a -> b) -> a -> b             *)
  (* look_up       : ∀ ind v, map ind v -> ind -> option v *)
  (* sequence      : ∀ b, unit -> b -> b                   *)
  (* loop          : bool -> unit -> unit                  *)

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

  let application : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun f arg ->
    let whole_expr = Core.fresh_type_variable () in
    let f'   = type_expression_to_type_value f in
    let arg' = type_expression_to_type_value arg in
    O.[
      C_equation (f' , P_constant (C_arrow , [arg' ; P_variable whole_expr]))
    ] , whole_expr

  let look_up : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun ds ind ->
    let ds'  = type_expression_to_type_value ds in
    let ind' = type_expression_to_type_value ind in
    let whole_expr = Core.fresh_type_variable () in
    let v = Core.fresh_type_variable () in
    O.[
      C_equation (ds' , P_constant (C_map, [ind' ; P_variable v])) ;
      C_equation (P_variable whole_expr , P_constant (C_option , [P_variable v]))
    ] , whole_expr

  let sequence : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun a b ->
    let a' = type_expression_to_type_value a in
    let b' = type_expression_to_type_value b in
    let whole_expr = Core.fresh_type_variable () in
    O.[
      C_equation (a' , P_constant (C_unit , [])) ;
      C_equation (b' , P_variable whole_expr)
    ] , whole_expr

  let loop : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun expr body ->
    let expr' = type_expression_to_type_value expr in
    let body' = type_expression_to_type_value body in
    let whole_expr = Core.fresh_type_variable () in
    O.[
      C_equation (expr'                 , P_constant (C_bool , [])) ;
      C_equation (body'                 , P_constant (C_unit , [])) ;
      C_equation (P_variable whole_expr , P_constant (C_unit , []))
    ] , whole_expr

  let let_in : I.type_expression -> I.type_expression option -> I.type_expression -> (constraints * O.type_variable) =
    fun rhs rhs_tv_opt result ->
    let rhs'        = type_expression_to_type_value rhs in
    let result'     = type_expression_to_type_value result in
    let rhs_tv_opt' = match rhs_tv_opt with
        None -> []
      | Some annot -> O.[C_equation (rhs' , type_expression_to_type_value annot)] in
    let whole_expr = Core.fresh_type_variable () in
    O.[
      C_equation (result' , P_variable whole_expr)
    ] @ rhs_tv_opt', whole_expr

  let assign : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun v e ->
    let v' = type_expression_to_type_value v in
    let e' = type_expression_to_type_value e in
    let whole_expr = Core.fresh_type_variable () in
    O.[
      C_equation (v' , e') ;
      C_equation (P_variable whole_expr , P_constant (C_unit , []))
    ] , whole_expr

  let annotation : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun e annot ->
    let e' = type_expression_to_type_value e in
    let annot' = type_expression_to_type_value annot in
    let whole_expr = Core.fresh_type_variable () in
    O.[
      C_equation (e' , annot') ;
      C_equation (e' , P_variable whole_expr)
    ] , whole_expr

  let matching : I.type_expression list -> (constraints * O.type_variable) =
    fun es ->
    let aux prev e =
      (e, O.C_equation (prev , e)) in
    let whole_expr = Core.fresh_type_variable () in
    let cs = match (List.map type_expression_to_type_value es) with
        [] -> []
      | hd::tl ->
        O.[C_equation (hd , P_variable whole_expr)]
        @ List.fold_map aux hd tl in
    cs, whole_expr

  let fresh_binder () =
    Core.fresh_type_variable ()

  let lambda
    : I.type_expression ->
      I.type_expression option ->
      I.type_expression option ->
      (constraints * O.type_variable) =
    fun fresh arg body ->
    let whole_expr = Core.fresh_type_variable () in
    let unification_arg = Core.fresh_type_variable () in
    let unification_body = Core.fresh_type_variable () in
    let arg'  = match arg with
        None -> []
      | Some arg -> O.[C_equation (P_variable unification_arg , type_expression_to_type_value arg)] in
    let body'  = match body with
        None -> []
      | Some body -> O.[C_equation (P_variable unification_body , type_expression_to_type_value body)]
    in O.[
        C_equation (type_expression_to_type_value fresh , P_variable unification_arg) ;
        C_equation (P_variable whole_expr ,
                    P_constant (C_arrow , [P_variable unification_arg ;
                                           P_variable unification_body]))
      ] @ arg' @ body' , whole_expr

end

open Core

type state = type_constraint list
let initial_state : state = []
let aggregate_constraints : state -> type_constraint list -> state result = fun a b -> ok (a @ b)
