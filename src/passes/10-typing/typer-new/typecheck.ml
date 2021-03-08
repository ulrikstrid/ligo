open Trace
open Typer_common.Errors
open Ast_typed.Types
open Solver_helpers
open Typecheck_utils

type db_repr = type_variable -> type_variable
type db_assignment = type_variable -> constructor_or_row option
type db_access = { repr : db_repr ; find_assignment : db_assignment; hashconsed_assignments: (type_variable, type_variable) PolyMap.t }

let err_TODO loc = corner_case ("TODO ERROR : check errors have not been written (" ^ loc ^ ")")

let fast_types_are_equal : db_access:db_access -> type_variable -> type_variable -> bool result =
  fun ~db_access a b ->
  let%bind a = trace_option (corner_case (Format.asprintf "internal error: attempted fast type equality on types which are not part of the hash cons raised at %s" __LOC__)) (PolyMap.find_opt (db_access.repr a) db_access.hashconsed_assignments) in
  let%bind b = trace_option (corner_case (Format.asprintf "internal error: attempted fast type equality on types which are not part of the hash cons raised at %s" __LOC__)) (PolyMap.find_opt (db_access.repr b) db_access.hashconsed_assignments) in
  ok @@ Var.equal a b

let fast_assert_types_are_equal ~db_access err a b =
  let%bind eq = fast_types_are_equal ~db_access a b in
  Assert.assert_true err eq

let compare_and_check_vars_type_value_list expected actual =
  Compare_renaming.compare_and_check_vars
    ~compare:(Compare_renaming.list ~compare:Compare_renaming.type_value)
    ~print_whole:(Ast_typed.PP.(list_sep_d type_value))
    expected
    actual

let rec recursive_find_assignnment : db_access:db_access -> type_variable -> type_value result =
  fun ~db_access arg -> 
  let {repr ; find_assignment} = db_access in
  match find_assignment arg with
  | Some `Constructor k ->
    let%bind () = Assert.assert_true (corner_case (Format.asprintf "internal error: invalid assignment: the variable constrained by the rhs is not the same as the lhs being assigned (modulo aliasing) raised at %s" __LOC__)) (Var.equal (repr k.tv) (repr arg)) in
    let%bind p_ctor_args = bind_map_list (recursive_find_assignnment ~db_access) k.tv_list in
    ok @@ Ast_typed.Reasons.wrap (Todo k.reason_constr_simpl) @@ P_constant { p_ctor_tag = k.c_tag ; p_ctor_args }
  | Some `Row r ->
    let%bind () = Assert.assert_true (corner_case (Format.asprintf "internal error: invalid assignment: the variable constrained by the rhs is not the same as the lhs being assigned (modulo aliasing) raised at %s" __LOC__)) (Var.equal (repr r.tv) (repr arg)) in
    let aux {associated_variable;michelson_annotation;decl_pos} : (Ast_typed.row_value,_) Trace.result =
      let%bind associated_value =  recursive_find_assignnment ~db_access associated_variable in
      ok @@ {associated_value;michelson_annotation;decl_pos}
    in
    let%bind p_row_args = Stage_common.Helpers.bind_map_lmap aux r.tv_map in
    ok @@ Ast_typed.Reasons.wrap (Todo r.reason_row_simpl) @@ P_row { p_row_tag = r.r_tag ; p_row_args }
  | None -> fail (corner_case (Format.asprintf "error  raised at %s" __LOC__))

let check_constructor : db_access:db_access -> c_constructor_simpl -> unit result =
  fun ~db_access c ->
    let {repr ; find_assignment} = db_access in
    (* Checks that α = κ(β…) is satisfied by the assignments for α and the β… *)
    match find_assignment (repr c.tv) with
    | Some `Constructor k ->
      (* assert k.tv === c.tv; *)
      let%bind () = Assert.assert_true (err_TODO __LOC__) (Ast_typed.Compare.constant_tag k.c_tag c.c_tag = 0) in
      let%bind () = Assert.assert_true (err_TODO __LOC__) (List.length k.tv_list = List.length c.tv_list) in
      (* ??? Not sure if this is the right thing to do,
        should we allow non-aliases and recursively check
        the equality of the assignments ? That would be slow. *)
      let%bind () = bind_iter_list (fun (left,right) ->
          fast_assert_types_are_equal ~db_access (err_TODO __LOC__) left right) @@
          List.combine k.tv_list c.tv_list
      in
      ok ()
    | Some `Row _ -> fail (err_TODO __LOC__)
    | None ->
      (* unassigned unification variable, see above in the other check there is
        already an error for that (but needs to be checked separately, we don't
        recursively check for missing assignments here see above in the other
        check there is already an error for that (but needs to be checked
        separately, we don't recursively check for missing assignments here *)
      fail (err_TODO __LOC__)

let check_alias : db_access:db_access -> c_alias -> unit result =
  fun ~db_access al ->
    fast_assert_types_are_equal ~db_access (err_TODO __LOC__) al.a al.b

let check_typeclass : db_access:db_access -> c_typeclass_simpl -> unit result =
  fun ~db_access tc ->
    (* (α, …) ∈ [ (type_value…) ; … ] *)
    (* recursively get the assignments of each α in tc.args to obtain a type_value *)
    let%bind () = match tc.tc with
      | [] -> ok ()
      | hd :: _ -> Assert.assert_true (err_TODO __LOC__)(* invalid typeclass constraint: number of variables in the constraint don't match the numer of arguments of the typeclass *)
        ((List.length hd) = (List.length tc.args))
    in
    let%bind args = bind_map_list (recursive_find_assignnment ~db_access) tc.args in
    (*
        tc.args = [ α β χ ]
        tc.tc = [ [a1 b1 c1] [ a2 b2 c2] [a3 b3 c3] [a4 b4 c4] … ]
        
        args = [ α β χ ] (recursive call to find_assignment)
        code bellow check that (a,b,c) = [a1 b1 c1] or = to [ a2 b2 c2] or = to …
    *)
    let aux : type_value list -> bool = fun allowed ->
      assert (List.length allowed = List.length tc.args) ;
      Trace.to_bool @@ compare_and_check_vars_type_value_list allowed args
    in
    Assert.assert_true (err_TODO __LOC__) @@ List.exists aux tc.tc
    
  let check_row : db_access:db_access -> c_row_simpl -> unit result =
    fun ~db_access r ->
    let {repr ; find_assignment} = db_access in
    match find_assignment (repr r.tv) with
    | Some `Row rw -> (
      let%bind () = Assert.assert_true (err_TODO __LOC__) (Ast_typed.Compare.row_tag rw.r_tag r.r_tag = 0) in
      let%bind () = Assert.assert_true (err_TODO __LOC__) (LMap.cardinal rw.tv_map = LMap.cardinal r.tv_map) in
      (* ??? Not sure if this is the right thing to do,
        should we allow non-aliases and recursively check
      the equality of the assignments ? That would be slow. *)
      let aux : (label * row_variable) * (label * row_variable) -> unit result =
        (* TODO : write better row comparisons *)
        fun ( (label_left, {associated_variable=var_left}) , (label_right, {associated_variable=var_right}) ) ->
          let%bind () = Assert.assert_true (err_TODO __LOC__) (Ast_typed.Compare.label label_left label_right = 0) in
          let%bind () = fast_assert_types_are_equal ~db_access (err_TODO __LOC__) var_left var_right in
          ok ()
      in
      bind_iter_list aux (List.combine (LMap.bindings rw.tv_map) (LMap.bindings r.tv_map))
    )
    | Some `Constructor _ -> fail (err_TODO __LOC__)
    | None -> fail (err_TODO __LOC__)(* unassigned unification variable *)

type bound_info = [ `Instantiation of (type_variable * type_variable) | `Constraint of type_constraint ]

(* -> check 3a that (recursive find_assignment α) = τ, stopping at occurrences of one of the 'x (return those in a multi mapping 'x ↦ part_of_α) *)
let rec compare_and_stop_at_bound_vars
    ~(db_access:db_access)
    (lhs : type_variable)
    (rhs : type_value_)
    (bound_by_foralls : type_variable (* repr of var bound by a forall in the rhs *) PolySet.t)
      : bound_info Compare_renaming.tree result =

    (*(instantiated_binder : (type_variable (* repr of var bound by a forall in the rhs *),
                            type_variable (* repr of instantiation for this forall-bound variable *)) PolyMap.t )*)
    match db_access.find_assignment lhs, rhs with
    (* unbound unification variable, _ *)
    | None, _ -> fail (err_TODO __LOC__) (* User error: unassigned variable, could not infer this, please annotate *)

    (* κ₁(ε, …), κ₂(φ, …) *)
    | Some `Constructor k, P_constant { p_ctor_tag ; p_ctor_args } ->
      let%bind () = Assert.assert_true
        (corner_case (Format.asprintf "wrong number of args: %d vs. %d (%a vs. %a)"
           (List.length k.tv_list)
           (List.length p_ctor_args)
           Ast_typed.PP.c_constructor_simpl k
           (PP_helpers.list_sep_d_par Ast_typed.PP.type_value_short) p_ctor_args))
        (List.length k.tv_list = List.length p_ctor_args) in
      (* not a check for equality, just a sanity check: *)
      let%bind () = Assert.assert_true (err_TODO __LOC__)(*internal error*) (Var.equal (db_access.repr k.tv) (db_access.repr lhs)) in
      let%bind () = Assert.assert_true (err_TODO (__LOC__^Format.asprintf "%a %a" Ast_typed.PP.constant_tag k.c_tag Ast_typed.PP.constant_tag p_ctor_tag))(*wrong tag k.c_tag != p_ctor_tag*) (Ast_typed.Compare.constant_tag k.c_tag p_ctor_tag = 0) in
      let%bind l = bind_map_list
        (fun (lhs, (rhs:type_value)) -> compare_and_stop_at_bound_vars ~db_access lhs rhs.wrap_content bound_by_foralls)
        (*instantiated_binder*)
        (List.combine k.tv_list p_ctor_args)
      in
      ok @@ Compare_renaming.List l

    (* Π(ε, …), Γ(φ, …) or Σ(ε, …), Σ(φ, …) or another incompatible combination of Π and Σ *)
    | Some `Row r, P_row { p_row_tag ; p_row_args } ->
      let%bind () = Assert.assert_true (err_TODO __LOC__)(*wrong number of args*) (LMap.cardinal r.tv_map = LMap.cardinal p_row_args) in
      (* not a check for equality, just a sanity check: *)
      let%bind () = Assert.assert_true (err_TODO __LOC__)(*internal error*) (Var.equal (db_access.repr r.tv) (db_access.repr lhs)) in
      let%bind () = Assert.assert_true (err_TODO __LOC__)(*wrong tag k.c_tag != p_ctor_tag*) (Ast_typed.Compare.row_tag r.r_tag p_row_tag = 0) in
      let%bind l = bind_map_list
        (fun ({associated_variable=lhs}, ({associated_value=rhs;_}:row_value)) -> compare_and_stop_at_bound_vars ~db_access lhs rhs.wrap_content bound_by_foralls)
        (*instantiated_binder*)
        (List.combine (LMap.to_list r.tv_map) (LMap.to_list p_row_args))
      in
      ok @@ Compare_renaming.List l

    (* _, ∀ α, constraints => β *)
    | _, P_forall pf ->
      (* TODO: I'm not 100% sure about this *)
      (* continue recursively after adding the new bound variable to bound_by_foralls *)
      let constraints = List.map (fun c -> Compare_renaming.Leaf (`Constraint c)) pf.constraints in
      let%bind tree = compare_and_stop_at_bound_vars ~db_access lhs pf.body.wrap_content (PolySet.add pf.binder bound_by_foralls) in
      ok @@ Compare_renaming.List (tree :: constraints)

    (* _, α *)
    | Some some_assignment, P_variable v ->
      if PolySet.mem (db_access.repr v) bound_by_foralls then
        ok @@ Compare_renaming.Leaf (`Instantiation (v, lhs))
      else
        let () = ignore some_assignment in
        fail (corner_case "Error: unbound type variable v in expected type, actual type was some_assignment")

    (* _, (application of ( τ₁ :: *->* ) to ( τ :: * ) ) *)
    | _, P_apply _ ->
      failwith "TODO(?): P_apply not really used for now, what should it do here? leave it for now"

    (* κ(ε, …), not-a-constructor *)
    | Some `Constructor k, other ->
      (ignore (k, other); failwith "wrong type: expected ctor k but got other")

    (* (ε, …) or Σ(ε, …), not-a-constructor *)
    | Some `Row r, other ->
      (ignore (r, other); failwith "wrong type: expected row r but got other")

let compare_and_stop_at_bound_vars ~db_access tv p_forall =
  compare_and_stop_at_bound_vars ~db_access tv p_forall (PolySet.create ~cmp:Ast_typed.Compare.type_variable)

(* -> check 3b that (recursive find_assignment part_of_α_1) = (recursive find_assignment part_of_α_2) if they were corresponding to the same 'x in the check 3a *)
let check_forall_instantiations_are_unifiable : db_access:db_access ->  bound_info Compare_renaming.tree -> (type_variable, type_variable) PolyMap.t result =
  fun ~db_access binder_instantiations ->
  (* start with an empty map *)
  (* Check if a polymorphic variable already has an instantiation *)
  let aux : (type_variable, type_variable) PolyMap.t -> (type_variable * type_variable) -> (type_variable, type_variable) PolyMap.t result =
    (fun acc (bound_by_forall,new_instantiation) ->
       match (PolyMap.find_opt bound_by_forall acc) with
       (* If no, add the binding to the map *)
       | None ->
         ok (PolyMap.add bound_by_forall new_instantiation acc)
       (* If yes, run fast_assert_types_are_equal existing_instantiation new_instantiation *)
       | Some existing_instantiation ->
         let%bind () =
           fast_assert_types_are_equal
             (corner_case "TODO err : incompatible types unif and existing_unif for the same bound variable")
             ~db_access
             new_instantiation
             existing_instantiation
         in
         ok acc)
  in
  (* finally, return the map built that way. *)
  let instantiations =
    List.filter_map
      (function `Constraint _ -> None | `Instantiation i -> Some i)
      (Compare_renaming.flatten_tree binder_instantiations) in
  bind_fold_list aux (PolyMap.create ~cmp:Ast_typed.Compare.type_variable) instantiations

(* This checks that a (a variable like a) = b (a type_value), using
   the substitutions from bound_var_assignments and the look-up table
   from db_access *)
let rec check_type_variable_and_type_value : db_access:db_access -> bound_var_assignments:(type_variable, type_variable) PolyMap.t -> type_variable -> type_value -> unit result =
  fun ~db_access ~bound_var_assignments a b ->
    match db_access.find_assignment a, b.wrap_content with
    | (None, _) ->
      fail @@ corner_case "unassigned unification variable, please annotate or make sure this variable gets generalized (generalization not supported as of 02020-11-20"
    | (Some (`Constructor ka), P_constant kb) ->
      let%bind () = Assert.assert_true (err_TODO __LOC__) (Ast_typed.Compare.constant_tag ka.c_tag kb.p_ctor_tag = 0) in
      bind_iter_list (fun (a,b) -> check_type_variable_and_type_value ~db_access ~bound_var_assignments a b)
        (List.combine ka.tv_list kb.p_ctor_args)
    | (Some (`Row ra), P_row rb) ->
      let%bind () = Assert.assert_true (err_TODO __LOC__) (Ast_typed.Compare.row_tag ra.r_tag rb.p_row_tag = 0) in
      let%bind () = Assert.assert_true (err_TODO __LOC__) (LMap.cardinal ra.tv_map = LMap.cardinal rb.p_row_args) in
      bind_iter_list
        (fun ((la,a),(lb,b)) ->
          let%bind () = Assert.assert_true (err_TODO __LOC__) (Ast_typed.Compare.label la lb = 0) in
          check_type_variable_and_type_value ~db_access ~bound_var_assignments a b)
        (List.combine (LMap.bindings @@ LMap.map (fun {associated_variable} -> associated_variable) ra.tv_map) (LMap.bindings @@ LMap.map (fun {associated_value;} -> associated_value) rb.p_row_args))
    | (Some _, P_variable vb) -> (
      match PolyMap.find_opt vb bound_var_assignments with
      | None -> fail (corner_case "unbound type variable")
      | Some found -> fast_assert_types_are_equal ~db_access (err_TODO __LOC__) found vb
    )
    | _ -> fail (corner_case "incompatible types")

(* Compares a and b using the given bound_var_assignments for bound variables *)
let rec compare_type_values_using_bound_vars : db_access:db_access -> bound_var_assignments:(type_variable, type_variable) PolyMap.t -> type_value -> type_value -> unit result
  = fun ~db_access ~bound_var_assignments (a : type_value) (b : type_value) ->
    match a.wrap_content , b.wrap_content with
    | P_constant ka , P_constant kb ->
      let%bind () = Assert.assert_true (err_TODO __LOC__) (Ast_typed.Compare.constant_tag ka.p_ctor_tag kb.p_ctor_tag = 0) in
      bind_list_iter
        (fun (a,b) -> compare_type_values_using_bound_vars ~db_access ~bound_var_assignments a b)
        (List.combine ka.p_ctor_args kb.p_ctor_args)
    | P_row      ra , P_row      rb ->
      let%bind () = Assert.assert_true (err_TODO __LOC__) (Ast_typed.Compare.row_tag ra.p_row_tag rb.p_row_tag = 0) in
      let%bind () = Assert.assert_true (err_TODO __LOC__) (LMap.cardinal ra.p_row_args = LMap.cardinal rb.p_row_args) in
      bind_list_iter
        (fun ((la,a),(lb,b)) ->
           let%bind () = Assert.assert_true (err_TODO __LOC__) (Ast_typed.Compare.label la lb = 0) in
           compare_type_values_using_bound_vars ~db_access ~bound_var_assignments a b)
        (List.combine (LMap.bindings @@ LMap.map (fun {associated_value} -> associated_value) ra.p_row_args) (LMap.bindings @@ LMap.map (fun {associated_value} -> associated_value) rb.p_row_args))
    | P_forall (*{ binder; constraints; body }*)_ , P_forall (*{ binder; constraints; body }*)_     ->
      failwith "comparison of foralls is not implemented yet."
    | (P_variable tv , _other)  ->
      (match PolyMap.find_opt tv bound_var_assignments with
       | None -> failwith "unassigned type variable"
       | Some assignment -> check_type_variable_and_type_value ~db_access ~bound_var_assignments assignment b)
    | (_other, P_variable tv)  ->
      (match PolyMap.find_opt tv bound_var_assignments with
       | None -> failwith "unassigned type variable"
       | Some assignment -> check_type_variable_and_type_value ~db_access ~bound_var_assignments assignment a)
    | P_apply _ ,_        ->
      failwith "p_apply is not currently used, this case should not happen. When it gets used, implement this case."
    | _ -> fail (corner_case "incompatible types: a b")

let check_access_label : _ = fun ~db_access ~bound_var_assignments accessor c_access_label_tvar { p_row_tag; p_row_args } ->
  (match p_row_tag with
     Ast_typed.Types.C_record ->
     let%bind field_type = trace_option
         (corner_case
            (Format.asprintf "Type error: field %a is not in record %a"
               Ast_typed.PP.label accessor
               (fun ppf lm -> Ast_typed.PP.(lmap_sep_d row_value) ppf @@ LMap.to_kv_list lm) p_row_args)) @@ LMap.find_opt accessor p_row_args in
     check_type_variable_and_type_value ~db_access ~bound_var_assignments c_access_label_tvar field_type.associated_value
   | Ast_typed.Types.C_variant -> failwith "Type error: cannot access field in variant")

let check_access_label_simpl : db_access:db_access -> bound_var_assignments:(type_variable, type_variable) PolyMap.t -> Stage_common.Types.label -> Ast_typed.Types.type_variable -> Ast_typed.Types.c_row_simpl -> unit result
  = fun ~db_access ~bound_var_assignments accessor c_access_label_tvar ({ reason_row_simpl=_; id_row_simpl=_; original_id=_; tv=_; r_tag; tv_map } : c_row_simpl) ->
  let () = ignore bound_var_assignments in
  match r_tag with
    Ast_typed.Types.C_record ->
    let%bind field_type = trace_option
        (corner_case
           (Format.asprintf "Type error: field %a is not in record %a"
              Ast_typed.PP.label accessor
              (fun ppf lm -> Ast_typed.PP.(lmap_sep_d row_variable) ppf @@ LMap.to_kv_list lm) tv_map)) @@ LMap.find_opt accessor tv_map in
    let%bind field_type_value = recursive_find_assignnment ~db_access field_type.associated_variable in
    let%bind c_access_label_tvar_value = recursive_find_assignnment ~db_access c_access_label_tvar in
    Compare_renaming.compare_and_check_vars
      ~compare:Compare_renaming.type_value
      ~print_whole:(Ast_typed.PP.type_value)
      c_access_label_tvar_value field_type_value
  | Ast_typed.Types.C_variant -> failwith "TODO: cannot access field in variant in typechecker, but works in the rest of the typer."

(* -> check 3c that each constraint is satisfied given the 'x -> part_of_α which was found by the check 3a and "checked all =" by 3b *)
let check_forall_constraints_are_satisfied : db_access:db_access -> bound_var_assignments:(type_variable, type_variable) PolyMap.t -> bound_info Compare_renaming.tree -> unit result =
  fun ~db_access ~bound_var_assignments tree ->
  let {repr ; find_assignment} = db_access in
  (* start with an empty map *)
  (* Check if a polymorphic variable already has an instantiation *)
  let aux : type_constraint -> unit result =
    fun c ->
      match c.c with
      | C_equation { aval; bval } ->
        compare_type_values_using_bound_vars ~db_access ~bound_var_assignments aval bval
      | C_typeclass c_tc ->
        let aux'' (arg, possible) = match Trace.to_option @@ compare_type_values_using_bound_vars ~db_access ~bound_var_assignments arg possible with None -> false | Some () -> true in
        let aux' args allowed_tuple = List.for_all aux'' (List.combine args allowed_tuple) in
        Assert.assert_true (err_TODO __LOC__) @@ List.exists (aux' c_tc.tc_args) c_tc.typeclass
      | C_access_label { c_access_label_record_type ; accessor ; c_access_label_tvar } ->
        (* ....................................................................................................................................... *)
        (
          match c_access_label_record_type.wrap_content with
            Ast_typed.Types.P_forall _ -> failwith "fields cannot be accessed on polymorphic values yet, please implement this check"
          | Ast_typed.Types.P_variable tv ->
            (match PolyMap.find_opt tv bound_var_assignments with
             | None -> failwith "unassigned type variable"
             | Some assignment ->
               (match find_assignment (repr assignment) with
                  None -> fail (corner_case "Error: unbound type variable v in expected type, actual type was some_assignment")
                | Some (`Constructor _) -> failwith "Type error: cannot access field in non-record"
                | Some (`Row r) -> check_access_label_simpl ~db_access ~bound_var_assignments accessor c_access_label_tvar r))
          | Ast_typed.Types.P_constant _ -> failwith "Type error: cannot access field in non-record"
          | Ast_typed.Types.P_apply _ ->
            failwith "p_apply is not currently used, this case should not happen. When it gets used, implement this case."
          | Ast_typed.Types.P_row r -> check_access_label ~db_access ~bound_var_assignments accessor c_access_label_tvar r
          | Ast_typed.Types.P_abs _ -> failwith "P_abs: unimplemented"
          | Ast_typed.Types.P_constraint _ -> failwith "P_constraint: unimplemented"
        )
      | C_apply _ -> failwith "TODO "
        (* compare_type_values_using_bound_vars ~db_access ~bound_var_assignments (c_access_label_record_type . accessor) == c_access_label_tvar *)
  in
  (* finally, return the map built that way. *)
  let constraints =
    List.filter_map
      (function `Constraint c -> Some c | `Instantiation _ -> None)
      (Compare_renaming.flatten_tree tree) in
  bind_list_iter aux constraints

(* check 3 that a forall   constraint α = ∀ 'x, constraints => τ is valid (with nested ∀ allowed inside τ)
    -> check 3a that (recursive find_assignment α) = τ, stopping at occurrences of one of the 'x (store those in a multi mapping 'x ↦ part_of_α)
    -> check 3b that (recursive find_assignment part_of_α_1) = (recursive find_assignment part_of_α_2) if they were corresponding to the same 'x in the check 3a
    -> check 3c that each constraint is satisfied given the 'x -> part_of_α which was found by the check 3a and "checked all =" by 3b

    Example:
                    lhs = rhs
                      start
                      α = forall 'y, constraints => (forall 'x, constraints => map('x,'x))
                          bound_by_∀ = { 'y }
                      α = forall 'x, constraints => map('x,'x)
                          bound_by_∀ = { 'y; 'x }
              map(β, β₂) = map('x,'x)
                  β      =     'x
                          return instantiated_binders = [ 'x ↦ β ]
                    β₂  =        'x
                          return instantiated_binders = [ 'x ↦ β₂ ]
                          return from ctor [ [ 'x ↦ β ]; [ 'x ↦ β₂ ] ]
                      finish
        flatten tree: instantiated_binders = [ 'x ↦ β ; 'x ↦ β₂ ]
        check flattened: { 'x ↦ β } (ok) 'x != β₂ (not ok *this is a bug* )     

            assignments             matching with    
        α = map(β,β₂)        |      map(x,  x  )
        β = map(δ,δ)         |   x
        δ = map(ε,ε)         |
        ε = int              |
        β₂ = map(δ₂,δ₂)      |   x
        δ₂ = map(ε₂,ε₂)      |
        ε₂ = int'            |
        
        (repr ε) === (repr ε') *)
let check_forall ~(db_access:db_access) (p : c_poly_simpl) : unit result =
    (* α = forall β, constraints => δ where δ can be a more complex type *)

    (* recursively traverse both the α (p.tv) and the δ (p.forall.body) at the same time,
        and compare them so that they are equal, but stopping as soon as a p.forall.binder
        is encountered on the rhs *)
    (* each case returns the instantiated_binder if it was found *)
    (* this is a DFS which propagates the instantiated_binder in this order *)
    (* p.tv = forall p.forall.binder , p.forall.constraints => p.forall.body *)

    (* -> check 3a that (recursive find_assignment α) = τ, stopping at occurrences of one of the 'x (store those in a multi mapping 'x ↦ part_of_α) *)
    let%bind (binder_intantiations : bound_info Compare_renaming.tree) =
      Trace.trace (fun err -> Typer_common.Errors.trace_debug "info: calling compare_and_stop_at_bound_vars on binder_intantiations" err) @@
      compare_and_stop_at_bound_vars ~db_access p.tv (P_forall p.forall)
    in
    (* -> check 3b that (recursive find_assignment part_of_α_1) = (recursive find_assignment part_of_α_2) if they were corresponding to the same 'x in the check 3a *)
    (* bound_var_assignments is a mapping from an 'x variable in a type_value to an α variable which can be looked up in the assignments *)
    let%bind bound_var_assignments = check_forall_instantiations_are_unifiable ~db_access binder_intantiations
    in
    (* -> check 3c that each constraint is satisfied given the 'x -> part_of_α which was found by the check 3a and "checked all =" by 3b *)
    (* α = forall β, p.forall.constraints… => δ
        an instantiated_binder is the actual type used to substitute α.
        we need to check that the p.forall.constraints are valid for the corresponding instantiated_type
        but a p.forall.constraints can refer to β or to the β' of any outer ∀, e.g.
        α = forall β₁, (no constraints here) => (forall β₂, (some_typeclass(β1,β2)) => δ)
        at this point we should know the instantiated_binder for β₁ and β₂, and these are
        stored in bound_var_assignments *)
    let%bind () =  check_forall_constraints_are_satisfied ~db_access ~bound_var_assignments binder_intantiations in
    ok ()

let check_access_label_simpl' : db_access:db_access -> c_access_label_simpl -> unit result =
  fun ~db_access { reason_access_label_simpl = _; id_access_label_simpl = _; record_type; label; tv } ->
  match db_access.find_assignment (db_access.repr record_type) with
    None -> fail (err_TODO __LOC__)(* unassigned unification variable *)
  | Some (`Constructor _) -> failwith "Type error: cannot access field on non-record"
  | Some (`Row { r_tag = C_variant }) -> failwith "Type error: cannot access field on variant"
  | Some (`Row r) ->
    check_access_label_simpl ~db_access ~bound_var_assignments:(PolyMap.create ~cmp:Ast_typed.Compare.type_variable) label tv r

let check_apply : db_access:db_access -> c_apply_simpl -> unit result =
  fun ~db_access { reason_apply_simpl = _; id_apply_simpl = _; f; arg } ->
  let _ = db_access, f, arg in
  let () = Format.printf "TODO: inference of type applications is implemented but not its type checking" in
  ok ()

let check : type_constraint_simpl list -> type_variable list -> (type_variable -> type_variable) -> (type_variable -> constructor_or_row option) -> unit result =
  fun all_constraints all_vars repr find_assignment ->
    (* Format.printf "Typechecking"; *)
    let%bind hashconsed_assignments = hashcons all_vars repr find_assignment in
    let db_access : db_access = { repr ; find_assignment ; hashconsed_assignments } in
    let aux : type_constraint_simpl -> unit result = fun c ->
      match c with
      | SC_Apply       a  -> check_apply ~db_access a
      | SC_Abs         _  -> failwith "kind error: expected a constraint (kind Constraint) but got a type abstraction (type-level function with kind _ -> _)"
      | SC_Constructor c  -> check_constructor ~db_access c
      | SC_Alias       al -> check_alias ~db_access al
      | SC_Typeclass   tc -> check_typeclass ~db_access tc
      | SC_Access_label l -> check_access_label_simpl' ~db_access l
      | SC_Row         r  -> check_row ~db_access r
      | SC_Poly        p  -> check_forall ~db_access p
    in
    let%bind () = bind_list_iter aux all_constraints in
    ok ()
