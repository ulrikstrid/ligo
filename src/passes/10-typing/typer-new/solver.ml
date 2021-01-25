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
module SRope = Rope.SimpleRope

let logfile = stderr (* open_out "/tmp/typer_log" *)

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

  (* TODO: replace this with a more efficient SRope.t (needs a "pop" function) *)
  module Pending = struct
    type 'a t = { l : 'a list }
    let empty : 'a t = { l = [] }
    let add : 'a -> 'a t -> 'a t = fun x { l } -> { l = x :: l }
    let add_list : 'a list -> 'a t -> 'a t = fun l1 { l } -> { l = l1 @ l }
    let union : 'a t -> 'a t -> 'a t = fun { l = l1 } { l = l2 } -> { l = l1 @ l2 }
    let of_list : 'a list -> 'a t = fun l -> { l }
    let to_list : 'a t -> 'a list = fun { l } -> l
    let pop : 'a t -> ('a * 'a t) option  = function
        { l = [] } -> None
      | { l = hd :: tl } -> Some (hd, { l = tl })
    let is_empty : 'a t -> bool = function { l = [] } -> true | _ -> false
    let _ = pop, union, add     (* unused warning *)
  end

  type worklist_ = {
    (* I don't know how to "open" the module to get only the fields for { w with … = … } or w.…, so I'm declaring this outside of the module. *)
    pending_type_constraint                        : type_constraint       Pending.t;
    pending_filtered_not_already_added_constraints : type_constraint       Pending.t;
    pending_type_constraint_simpl                  : type_constraint_simpl Pending.t;
    pending_c_alias                                : c_alias               Pending.t;
  }

  type ('part, 'whole) mini_lens = { get : 'whole -> 'part Pending.t; set : 'whole -> 'part Pending.t -> 'whole; }   

  module Worklist = struct
    type t = worklist_

    let decrement_and_check_still_time_to_live time_to_live = 
      let () = time_to_live := !time_to_live - 1 in
      if (!time_to_live) = 0
      then (Format.printf "timeout 78909765.\n"; false)
      else true
  
    let is_empty ~time_to_live
        (_state,
         { pending_type_constraint;
           pending_filtered_not_already_added_constraints;
           pending_type_constraint_simpl;
           pending_c_alias }) =
      let () = Printf.fprintf logfile "size(worklist)=(%d | %d | %d | %d)\n"
          (List.length @@ Pending.to_list pending_type_constraint                       )
          (List.length @@ Pending.to_list pending_filtered_not_already_added_constraints)
          (List.length @@ Pending.to_list pending_type_constraint_simpl                 )
          (List.length @@ Pending.to_list pending_c_alias                               )
      in
      decrement_and_check_still_time_to_live time_to_live &&
      Pending.is_empty pending_type_constraint                        &&
      Pending.is_empty pending_filtered_not_already_added_constraints &&
      Pending.is_empty pending_type_constraint_simpl                  &&
      Pending.is_empty pending_c_alias

    let process lens (state, worklist) f =
      match (Pending.pop (lens.get worklist)) with
        None -> ok (state, worklist)
      | Some (element, rest) ->
        (* set this field of the worklist to the rest of this Pending.t *)
        let worklist = lens.set worklist rest in
        (* Process the element *)
        let%bind (state, new_worklist) = f (state, element) in
        (* While processing, f can queue new tasks in a fresh worklist, we're merging the worklists here *)
        let merged_worklists = {
          pending_type_constraint                        = Pending.union new_worklist.pending_type_constraint                        worklist.pending_type_constraint                        ;
          pending_filtered_not_already_added_constraints = Pending.union new_worklist.pending_filtered_not_already_added_constraints worklist.pending_filtered_not_already_added_constraints ;
          pending_type_constraint_simpl                  = Pending.union new_worklist.pending_type_constraint_simpl                  worklist.pending_type_constraint_simpl                  ;
          pending_c_alias                                = Pending.union new_worklist.pending_c_alias                                worklist.pending_c_alias                                ;
        }
        (* return the state updated by f, and the updated worklist (without the processed element, with the new tasks) *)
        in ok (state, merged_worklists)

    let empty = {
      (* TODO: these should be ropes *)
      pending_type_constraint                        = Pending.empty ;
      pending_filtered_not_already_added_constraints = Pending.empty ;
      pending_type_constraint_simpl                  = Pending.empty ;
      pending_c_alias                                = Pending.empty ;
    }
  end

  let pending_type_constraint                        = { get = (fun { pending_type_constraint                        = x ; _ } -> x); set = (fun w x -> { w with pending_type_constraint                        = x }) }
  let pending_filtered_not_already_added_constraints = { get = (fun { pending_filtered_not_already_added_constraints = x ; _ } -> x); set = (fun w x -> { w with pending_filtered_not_already_added_constraints = x }) }
  let pending_type_constraint_simpl                  = { get = (fun { pending_type_constraint_simpl                  = x ; _ } -> x); set = (fun w x -> { w with pending_type_constraint_simpl                  = x }) }
  let pending_c_alias                                = { get = (fun { pending_c_alias                                = x ; _ } -> x); set = (fun w x -> { w with pending_c_alias                                = x }) }
  let _ = pending_type_constraint, pending_filtered_not_already_added_constraints, pending_type_constraint_simpl, pending_c_alias (* unused warning *)

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
        let () = Format.printf "aux_heuristic_alias is disabled temporarily, because enabling it made some other tests fail (it should be enabled, but it makes these tests go through some code paths which are bogus, the tests worked before because they managed to infer without these heuristics, but enabling them is needed)\n%!" in
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

  let pp_indented_constraint_list =
    let open PP_helpers in
    let open Ast_typed.PP in
    (list_sep type_constraint_short (tag "\n  "))

  let pp_indented_constraint_simpl_list =
    let open PP_helpers in
    let open Ast_typed.PP in
    (list_sep type_constraint_simpl_short (tag "\n  "))

   (* Takes a list of constraints, applies all selector+propagator pairs
     to each in turn. *)
  let select_and_propagate_all : typer_state -> Worklist.t -> typer_state result =
    fun state initial_constraints ->
    (* To change the order in which the constraints are processed, modify this loop. *)
    let () = queue_print (fun () -> Formatt.printf "In select and propagate all\n") in
    let time_to_live = ref 1000 in
    until
      (* repeat until the worklist is empty *)
      (Worklist.is_empty ~time_to_live)
      (fun (state, worklist) ->
        let () = queue_print (fun () -> Formatt.printf "Start iteration with constraints :\n  %a\n\n" pp_indented_constraint_list (Pending.to_list worklist.pending_type_constraint)) in

        let () = Printf.fprintf logfile "aaa" in
        let { PolySet.set = added_constraints; added = constraints'; duplicates = _ } =
          PolySet.add_list (Pending.to_list worklist.pending_type_constraint) state.added_constraints in

        let worklist = { worklist with pending_type_constraint = Pending.empty } in
        let worklist = { worklist with pending_filtered_not_already_added_constraints = Pending.add_list constraints' worklist.pending_filtered_not_already_added_constraints } in

        (* Pretty sure this assert is bogus, commenting it out: *)
        (* let () = List.iter2 (fun a b -> assert (a = b)) (Pending.to_list worklist.pending_type_constraint) @@ List.rev constraints' in *)
        let state = { state with added_constraints } in

        let () = Printf.fprintf logfile "ddd" in
        let%bind (state, worklist) =
          until (fun (_state, worklist) -> Worklist.decrement_and_check_still_time_to_live time_to_live && Pending.is_empty worklist.pending_filtered_not_already_added_constraints)
            (fun (state, worklist) ->
               let () = Printf.fprintf logfile "eee" in
               Worklist.process pending_filtered_not_already_added_constraints (state, worklist)
                 (fun (state, type_constraint) -> ok (state, { Worklist.empty with pending_type_constraint_simpl = Pending.of_list (simplify_constraint type_constraint) }))
            )
            (state, worklist)
        in

        let () = Printf.fprintf logfile "fff: bug: should empty the worklist otherwise it will never be empty of course" in
        let pendingggg_type_constraint_simpl = Pending.to_list worklist.pending_type_constraint_simpl in
        let worklist = { worklist with pending_type_constraint_simpl = Pending.empty } in

        (* Extract aliases and apply them *)
        let () = queue_print (fun () -> Formatt.printf "Constraint left : %a\n" pp_indented_constraint_simpl_list pendingggg_type_constraint_simpl) in
        let () = queue_print (fun () -> Formatt.printf "Extract aliases and apply them\n") in
        let%bind (state, new_constraints_from_aliases), constraints = bind_fold_map_list
            (fun (state, nc) c ->
               match%bind (add_alias state c) with
                 Some (state, new_constraints) -> ok ((state, new_constraints @ nc), [])
               | None -> ok ((state, nc), [c]))
            (state, [])
            pendingggg_type_constraint_simpl in

        let () = Printf.fprintf logfile "ggg" in
        let constraints = List.flatten constraints in

        let () = queue_print (fun () -> Formatt.printf "Constraints :%a\n%!" pp_indented_constraint_simpl_list constraints) in
        let%bind (state, new_constraints) = bind_fold_map_list add_constraint_and_apply_heuristics state constraints in

        let worklist : Worklist.t = {
          worklist with 
          pending_type_constraint = Pending.of_list (new_constraints_from_aliases @ List.flatten new_constraints);
        }
        in
        ok (state, worklist)
      )

      (state, initial_constraints)
    >>|? fst
  (* already_selected_and_propagators ; all_constraints ; plugin_states ; aliases *)

  module All_vars = Typecheck_utils.All_vars(Plugins)
  let main : typer_state -> type_constraint list -> typer_state result =
    fun state initial_constraints ->
    let () = queue_print (fun () -> Formatt.printf "In solver main\n%!") in
    let%bind (state : typer_state) = select_and_propagate_all state {Worklist.empty with pending_type_constraint = Pending.of_list initial_constraints} in
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
      plugin_states                    = plugin_states ;
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


