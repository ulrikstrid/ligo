open Trace
open Typer_common.Errors
module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_typed.Types
open Typesystem.Solver_types
open Solver_helpers
open Proof_trace_checker

open Pretty_print_variables
module Formatt = Format

(*  ………………………………………………………………………………………………… Plugin-based solver below ………………………………………………………………………………………………… *)

(* Later on, we'll ensure that all the heuristics register the
   existential/unification variables that they create, as well as the
   new constraints that they create. We will then check that they only
   use a small set of core axioms to derive new constraints, and
   produce traces justifying that instanciations satisfy all related
   constraints, and that all existential variables are instantiated
   (possibly by first generalizing the type and then using the
   polymorphic type argument to instantiate the existential). *)

module Make_solver(Plugins : Plugins) : sig
  type plugin_states = Plugins.Indexers.PluginFields(PerPluginState).flds
  type nonrec typer_state = (typer_error, plugin_states) Typesystem.Solver_types.typer_state
  val pp_typer_state  : Format.formatter -> typer_state -> unit
  val get_alias : Ast_typed.type_variable -> type_variable poly_unionfind -> (type_variable, typer_error) Trace.result
  val main : typer_state -> type_constraint list -> typer_state result
  val initial_state : typer_state
  val placeholder_for_state_of_new_typer : unit -> typer_state
  val discard_state : typer_state -> unit
end = struct
  module Plugin_states = Plugins.Indexers.PluginFields(PerPluginState)
  type plugin_states = Plugins.Indexers.PluginFields(PerPluginState).flds
  type nonrec typer_state = (typer_error, plugin_states) Typesystem.Solver_types.typer_state

  type plugin_units = Plugins.Indexers.PluginFields(PerPluginUnit).flds
  let plugin_fields_unit : plugin_units = Plugins.Indexers.plugin_fields_unit

  let mk_repr state x = UnionFind.Poly2.repr x state.aliases

  let pp_typer_state = fun ppf ({ all_constraints; plugin_states; aliases ; already_selected_and_propagators } : typer_state) ->
    let open Typesystem.Solver_types in
    let open PP_helpers in
    let module MapPP = Plugins.Indexers.MapPlugins(PPPlugin) in
    let pp_indexers ppf states =
      queue_print (fun () -> Formatt.fprintf ppf "@[ <@ %a ]@ >" (fun ppf states -> let _ : plugin_units = MapPP.f ppf states in ()) states)
    in
    queue_print (fun () -> Formatt.fprintf ppf "{@[<hv 2> @ all_constaints = %a;@ plugin_states = %a ;@ aliases = %a ;@ already_selected_and_propagators = %a @]@ }"
      (RedBlackTrees.PolySet.pp PP.type_constraint_) all_constraints
      pp_indexers plugin_states
      (UnionFind.Poly2.pp Ast_typed.PP.type_variable) aliases
      (list_sep pp_ex_propagator_state (fun ppf () -> Formatt.fprintf ppf " ;@ ")) already_selected_and_propagators)

  let aux_remove state to_remove =
    let () = queue_print (fun () -> Formatt.printf "Remove constraint :\n  %a\n\n%!" Ast_typed.PP.type_constraint_simpl_short to_remove) in
    let module MapRemoveConstraint = Plugins.Indexers.MapPlugins(RemoveConstraint) in
    let%bind plugin_states = MapRemoveConstraint.f (mk_repr state, to_remove) state.plugin_states in
    ok {state with plugin_states}

  let aux_update state { remove_constraints; add_constraints; proof_trace } =
    let%bind () = check_proof_trace proof_trace in
    let%bind state = bind_fold_list aux_remove state remove_constraints in
    ok (state, add_constraints)

  let aux_propagator heuristic state selector_output =
    (* TODO: before applying a propagator, check if it does
       not depend on constraints which were removed by the
       previous propagator *)
    let%bind updates = heuristic.plugin.propagator selector_output (mk_repr state) in
    let%bind (state, new_constraints) = bind_fold_map_list aux_update state updates in
    ok (state, List.flatten new_constraints)

  let aux_heuristic_alias demoted_repr new_repr state (Heuristic_state heuristic) =
    let selector_outputs = heuristic.plugin.alias_selector demoted_repr new_repr state.plugin_states in
    let aux = fun (l,already_selected) el ->
      if PolySet.mem el already_selected then (l,already_selected)
      else (el::l, PolySet.add el already_selected)
    in
    let selector_outputs,already_selected = List.fold_left aux ([], heuristic.already_selected) selector_outputs in
    let heuristic = { heuristic with already_selected } in
    let%bind (state, new_constraints) = bind_fold_map_list (aux_propagator heuristic) state selector_outputs in
    (* let () = queue_print (fun () -> Format.printf "Return with new constraints: (%a)\n%!" Ast_typed.PP.(list_sep_d (list_sep_d type_constraint_short)) new_constraints) in *)
    ok (state, (Heuristic_state heuristic, List.flatten new_constraints))

  let add_alias : typer_state -> type_constraint_simpl -> ((typer_state * type_constraint_list) option, typer_error) Simple_utils.Trace.result =
    fun state new_constraint ->
    match new_constraint with
    | Ast_typed.Types.SC_Alias { reason_alias_simpl=_; a; b } ->
      let () = queue_print (fun () -> Format.printf "Add_alias %a=%a\n%!" Ast_typed.PP.type_variable a Ast_typed.PP.type_variable b) in

      (* get the changed reprs due to that alias constraint *)
      let UnionFind.Poly2.{ partition = aliases; changed_reprs } =
        UnionFind.Poly2.equiv a b state.aliases in
      let () = queue_print (fun () -> Format.printf "changed_reprs :(%a)\n%!" Ast_typed.PP.(fun ppf ({demoted_repr=a;new_repr=b}: _ UnionFind.Poly2.changed_reprs) -> Format.fprintf ppf "%a -> %a" type_variable a type_variable b) changed_reprs) in
      let state = { state with aliases } in

      (* apply all the alias_selectors and propagators given the new alias *)
      let%bind (state, new_constraints) = (
        let () = Format.printf "aux_heuristic_alias is disabled temporarily, because enabling it made some other tests fail (it should be enabled, but it makes these tests go through some code paths which are bogus, the tests worked before because they managed to infer without these heuristics, but enabling them is needed)" in
        if false then
          (* TODO: possible bug: here, should be use the demoted_repr
             and new_repr, or the ones as given by the alias? We should
             maintain as much as possible the illusion that aliased
             variables have always been aliased from the start,
             therefore if the alias constraint contains outdated
             references, it makes sense to update them before calling
             the .alias_selector functions. *)
          let%bind (state, hc) = bind_fold_map_list (aux_heuristic_alias UnionFind.Poly2.(changed_reprs.demoted_repr) UnionFind.Poly2.(changed_reprs.new_repr)) state state.already_selected_and_propagators in
          let (already_selected_and_propagators, new_constraints) = List.split hc in
          let state = { state with already_selected_and_propagators } in
          ok (state, List.flatten new_constraints)
        else
          ok (state, [])
      ) in

      let { all_constraints ; added_constraints ; plugin_states ; aliases ; already_selected_and_propagators } = state in
      
      (* Add alias constraint to the set of all constraints *)
      let all_constraints = PolySet.add new_constraint all_constraints in

      let plugin_states =
        let module MapMergeAliases = Plugins.Indexers.MapPlugins(MergeAliases) in
        MapMergeAliases.f changed_reprs plugin_states in

      ok @@ Some ({ all_constraints ; added_constraints ; plugin_states ; aliases ; already_selected_and_propagators }, new_constraints)
    | _ ->
      ok @@ None

  let get_alias variable aliases =
    trace_option (corner_case (Format.asprintf "can't find alias root of variable %a" Var.pp variable)) @@
    (* TODO: after upgrading UnionFind, this will be an option, not an exception. *)
    try Some (UF.repr variable aliases) with Not_found -> None

  let aux_heuristic constraint_ state (Heuristic_state heuristic) =
    (* let () = queue_print (fun () -> Formatt.printf "Apply heuristic %s for constraint : %a\n%!" 
      heuristic.plugin.heuristic_name
      PP.type_constraint_ constraint_ in *)
    let selector_outputs = heuristic.plugin.selector constraint_ state.plugin_states in
    let aux = fun (l,already_selected) el ->
      if PolySet.mem el already_selected then (l,already_selected)
      else (el::l, PolySet.add el already_selected)
    in
    let selector_outputs,already_selected = List.fold_left aux ([], heuristic.already_selected) selector_outputs in
    let heuristic = { heuristic with already_selected } in
    let%bind (state, new_constraints) = bind_fold_map_list (aux_propagator heuristic) state selector_outputs in
    (* let () = queue_print (fun () -> Format.printf "Return with new constraints: (%a)\n%!" Ast_typed.PP.(list_sep_d (list_sep_d type_constraint_short)) new_constraints) in *)
    ok (state, (Heuristic_state heuristic, List.flatten new_constraints))

  (* apply all the selectors and propagators *)
  let add_constraint_and_apply_heuristics state constraint_ =
    (* let () = queue_print (fun () -> Format.printf "Add constraint and apply heuristics for constraint: %a\n%!" Ast_typed.PP.type_constraint_simpl constraint_) in *)
    if PolySet.mem constraint_ state.all_constraints then ok (state, [])
    else
      let state =
        let module MapAddConstraint = Plugins.Indexers.MapPlugins(AddConstraint) in
        { state with plugin_states = MapAddConstraint.f (mk_repr state, constraint_) state.plugin_states }
      in
      let%bind (state, hc) = bind_fold_map_list (aux_heuristic constraint_) state state.already_selected_and_propagators in
      let (already_selected_and_propagators, new_constraints) = List.split hc in
      let state = { state with already_selected_and_propagators } in
      ok (state, List.flatten new_constraints)
  
   (* Takes a list of constraints, applies all selector+propagator pairs
     to each in turn. *)
  let select_and_propagate_all : typer_state -> type_constraint list -> typer_state result =
    fun state initial_constraints ->
    (* To change the order in which the constraints are processed, modify this loop. *)
    let () = queue_print (fun () -> Formatt.printf "In select and propagate all\n%!") in
    until
      (* repeat until the worklist is empty *)
      (function (_, []) -> true | _ -> false)
      (fun (state, constraints) ->
        let aux (set, lst) (el: type_constraint) =
          if PolySet.mem el set then 
            set,lst
          else
            (* let () = queue_print (fun () -> Format.printf "\nTOTO ADD: %a\n" Ast_typed.PP.type_constraint_short el) in *)
            PolySet.add el set, el::lst
        in
        let pp_indented_constraint_list =
          let open PP_helpers in
          let open Ast_typed.PP in
          (list_sep type_constraint_short (tag "\n  ")) in
        let pp_indented_constraint_simpl_list =
          let open PP_helpers in
          let open Ast_typed.PP in
          (list_sep type_constraint_simpl_short (tag "\n  ")) in
        let () = queue_print (fun () -> Formatt.printf "Start iteration with constraints :\n  %a\n\n%!" pp_indented_constraint_list constraints) in
        let added_constraints, constraints' = List.fold_left aux (state.added_constraints,[]) constraints in
        List.iter2 (fun a b -> assert (a = b)) constraints @@ List.rev constraints';
        let state = { state with added_constraints } in 
        (* Simplify constraints *)
        let () = queue_print (fun () -> Formatt.printf "Simplify constraints\n%!") in
        let constraints'' = List.flatten @@ List.map simplify_constraint constraints' in
        (* Extract aliases and apply them *)
        let () = queue_print (fun () -> Formatt.printf "Constraint left : %a\n%!" pp_indented_constraint_simpl_list constraints'') in
        let () = queue_print (fun () -> Formatt.printf "Extract aliases and apply them\n%!") in
        let%bind (state, new_constraints_from_aliases), constraints = bind_fold_map_list
            (fun (state, nc) c ->
               match%bind (add_alias state c) with
                 Some (state, new_constraints) -> ok ((state, new_constraints @ nc), [])
               | None -> ok ((state, nc), [c]))
            (state, [])
            constraints'' in
        let constraints = List.flatten constraints in

        let () = queue_print (fun () -> Formatt.printf "Constraints :%a\n%!" pp_indented_constraint_simpl_list constraints) in
        let%bind (state, new_constraints) = bind_fold_map_list add_constraint_and_apply_heuristics state constraints in
        ok (state, new_constraints_from_aliases @ List.flatten new_constraints))
      (state, initial_constraints)
    >>|? fst
  (* already_selected_and_propagators ; all_constraints ; plugin_states ; aliases *)

  module All_vars = Typecheck_utils.All_vars(Plugins)
  let main : typer_state -> type_constraint list -> typer_state result =
    fun state initial_constraints ->
    let () = queue_print (fun () -> Formatt.printf "In solver main\n%!") in
    let%bind (state : typer_state) = select_and_propagate_all state initial_constraints in
    let () = queue_print (fun () -> Formatt.printf "With assignments :\n  %a\n%!"
      (Plugin_states.Assignments.pp Ast_typed.PP.type_variable) (Plugin_states.assignments state.plugin_states)#assignments) in
    let failure = Typecheck.check (PolySet.elements state.all_constraints)
      (All_vars.all_vars state)
      (fun v -> UnionFind.Poly2.repr v state.aliases)
      (fun v -> Plugin_states.Assignments.find_opt v (Plugin_states.assignments state.plugin_states)#assignments) in
    let () = if not @@ Trace.to_bool failure then Pretty_print_variables.flush_pending_print state in
    let%bind () = failure in
    ok state
  
  (* This function is called when a program is fully compiled, and the
     typechecker's state is discarded. TODO: either get rid of the state
     earlier, or perform a sanity check here (e.g. that types have been
     inferred for all bindings and expressions, etc.
  
     Also, we should check at these places that we indeed do not need the
     state any further. Suzanne *)
  let discard_state (_ : typer_state) = ()
  
  let initial_state : typer_state =
    let module MapCreateState = Plugins.Indexers.MapPlugins(CreateState) in
    let plugin_states = MapCreateState.f () plugin_fields_unit in
    {
      all_constraints                  = PolySet.create ~cmp:Ast_typed.Compare.type_constraint_simpl ;
      added_constraints                = PolySet.create ~cmp:Ast_typed.Compare.type_constraint ;
      aliases                          = UnionFind.Poly2.empty Var.pp Var.compare ;
      plugin_states                     = plugin_states ;
      already_selected_and_propagators = List.map init_propagator_heuristic Plugins.heuristics ;
    }

  let placeholder_for_state_of_new_typer () = initial_state
end

(* TODO: make the typer a fonctor and move this instantiation as further outwards as possible. *)
(* Instantiate the solver with a selection of plugins *)
include Make_solver(Plugins)
type nonrec _ typer_state = typer_state

(*  ………………………………………………………………………………………………… Plugin-based solver above ………………………………………………………………………………………………… *)

let json_typer_state = fun ({ all_constraints=_ ; plugin_states=_ ; aliases=_ ; already_selected_and_propagators } : _ typer_state) : Yojson.Safe.t ->
  let open Typesystem.Solver_types in
  `Assoc[ ("all_constraints", `String "TODO");
          ("plugin_states", (* (Ast_typed.Yojson.structured_dbs structured_dbs) *) `String "TODO");
          ("aliases", `String "TODO");
          ("already_selected_and_propagators", 
           let list f lst = `List (List.map f lst) in
           (list json_ex_propagator_state already_selected_and_propagators))]


