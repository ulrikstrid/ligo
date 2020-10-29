open Trace
open Typer_common.Errors
module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_typed.Types
open Typesystem.Solver_types

type nonrec 'a result = ('a, typer_error) result

(* TODO: move the propagator_heuristics list to a separate module which calls the solver with a bunch of heuristics *)
let propagator_heuristics =
  [
    (* Heuristic_break_ctor.heuristic ;
     * Heuristic_specialize1.heuristic ;
     * Heuristic_tc_fundep.heuristic ; *)
  ]

let init_propagator_heuristic (Propagator_heuristic { selector ; propagator ; printer ; printer_json ; comparator }) =
  Propagator_state { selector ; propagator ; printer ; printer_json ; already_selected = Set.create ~cmp:comparator }

(*  ………………………………………………………………………………………………… Plugin-based solver below ………………………………………………………………………………………………… *)

(* library function *)
let rec worklist : ('state -> 'element -> ('state * 'element list) result) -> 'state -> 'element list -> 'state result =
  fun f state elements ->
  match elements with
    [] -> ok state
  | element :: rest ->
    let%bind new_state, new_elements = f state element in
    worklist f new_state (new_elements @ rest)

module MakeSolver(Plugins : Plugins) : sig
  module type PluginStates = Plugins.Indexers.PluginFields(PerPluginState).S
  type typer_state = (typer_error, (module PluginStates)) __plugins__typer_state
  (* val main : typer_state -> typer_state *)
  val placeholder_for_state_of_new_typer : typer_state
end = struct
  open  UnionFind
  module type PluginStates = Plugins.Indexers.PluginFields(PerPluginState).S
  module type PluginUnits = Plugins.Indexers.PluginFields(PerPluginUnit).S

  type pluginStates = (module PluginStates)
  let pluginFieldsUnit = (module Plugins.Indexers.PluginFieldsUnit : PluginUnits)

  (* Function which merges all aliases withing a single plugin's state *)
  module MergeAliases = struct
    type extra_args = type_variable UnionFind.Poly2.changed_reprs
    module MakeInType = PerPluginState
    module MakeOutType = PerPluginState
    module F(Plugin : Plugin) = struct
      let f UnionFind.Poly2.{ demoted_repr ; new_repr } state =
        let merge_keys = {
          map = (fun m -> ReprMap.alias ~demoted_repr ~new_repr m);
          set = (fun s -> (*ReprSet.alias a b s*) s);
        }
        in Plugin.merge_aliases merge_keys state
    end
  end

  (* Function which creates a plugin's initial state *)
  module CreateState = struct
    type extra_args = unit
    module MakeInType = PerPluginUnit
    module MakeOutType = PerPluginState
    module F(Plugin : Plugin) = struct
      let f () (() as _state) = Plugin.create_state ~cmp:Ast_typed.Compare.type_variable
    end
  end

  type typer_state = (typer_error, pluginStates) __plugins__typer_state
  type nonrec 'a result = ('a, typer_error) Simple_utils.Trace.result

  (* sub-component: constraint selector (worklist / dynamic queries) *)
  let select_and_propagate : 'old_input 'selector_output . ('old_input, 'selector_output) selector -> ('selector_output , typer_error) propagator -> 'selector_output poly_set -> 'old_input -> pluginStates -> ('selector_output poly_set * updates) result =
    fun selector propagator ->
    fun already_selected old_type_constraint dbs ->
    (* TODO: thread some state to know which selector outputs were already seen *)
    let selected_outputs = selector old_type_constraint dbs in
    let { Set.set = already_selected ; duplicates = _ ; added = selected_outputs } = Set.add_list selected_outputs already_selected in
    (* Call the propagation rule *)
    let%bind new_constraints = bind_map_list propagator selected_outputs in
    (* return so that the new constraints are pushed to some kind of work queue *)
    let () =
      if Ast_typed.Debug.debug_new_typer && false then
        Printf.fprintf stderr "%s" @@ Format.asprintf "propagator produced\nupdates = %a\n"
          Ast_typed.PP.updates_list
          new_constraints
    in
    ok (already_selected , List.flatten new_constraints)

  (* ------------------------------------------------------------------------------------------------------------- *)
  (* ------------------------------------------------------------------------------------------------------------- *)
  (* ------------------------------------------------------------------------------------------------------------- *)
  (* TODO:
     - also iterate over the heuristics in the main loop (cartesian product)
     - move the removal to the main loop *)
  (* ------------------------------------------------------------------------------------------------------------- *)
  (* ------------------------------------------------------------------------------------------------------------- *)
  (* ------------------------------------------------------------------------------------------------------------- *)
  
  let apply_removals : (type_constraint list * typer_state) -> update -> (type_constraint list * typer_state) result =
    fun (acc, dbs) update ->
    let%bind dbs' = bind_fold_list Normalizer.normalizers_remove dbs update.remove_constraints in
    (* TODO: don't append list like this, it's inefficient. *)
    ok @@ (acc @ update.add_constraints(* , dbs' *))

  let apply_multiple_removals : update list -> typer_state -> (type_constraint list * typer_state) result =
    fun updates state ->
    bind_fold_list apply_removals ([], state) updates

  let select_and_propagate_one :
    pluginStates ->
    type_constraint_simpl selector_input ->
    typer_error ex_propagator_state list * type_constraint list ->
    typer_error ex_propagator_state ->
    (typer_error ex_propagator_state list * type_constraint list) result =
    fun
      dbs
      new_constraint
      (new_states , new_constraints)
      (Propagator_state { selector; propagator; printer ; printer_json ; already_selected }) ->
      let sel_propag = (select_and_propagate selector propagator) in
      let%bind (already_selected , updates) =
        sel_propag already_selected new_constraint dbs in
      let%bind new_constraints'', dbs = apply_multiple_removals updates dbs in
      ok @@ (
        (Propagator_state { selector; propagator; printer ; printer_json ; already_selected }
         :: new_states),
        new_constraints'' @ new_constraints,
      )

  (* Takes a constraint, applies all selector+propagator pairs to it.
     Keeps track of which constraints have already been selected. *)
  let select_and_propagate_all' : typer_error ex_propagator_state list -> type_constraint_simpl -> pluginStates -> (typer_error ex_propagator_state list * type_constraint list) result =
    (* TODO: should not return an updated typer_state! *)
    fun already_selected_and_propagators new_constraint dbs ->
    bind_fold_list
      (select_and_propagate_one dbs new_constraint)
      ([], [])
      already_selected_and_propagators

  (* adds a new constraint to the database and applies all the selector+propagator pairs to it *)
  let add_constraint_and_propagate : typer_state -> type_constraint_simpl -> (typer_state * type_constraint list, typer_error) Simple_utils.Trace.result =
    fun { all_constraints_ ; plugin_states ; aliases_ ; already_selected_and_propagators_ } new_constraint ->
    let module MapMergeAliases = Plugins.Indexers.MapPlugins(MergeAliases) in
    match new_constraint with
      Ast_typed.Types.SC_Alias { reason_alias_simpl=_; is_mandatory_constraint=_; a; b } ->
      let all_constraints_ = new_constraint :: all_constraints_ in
      let UnionFind.Poly2.{ partition = aliases_; changed_reprs } =
        UnionFind.Poly2.equiv a b aliases_ in
      let plugin_states = List.fold_left
          (fun state changed_reprs -> MapMergeAliases.f changed_reprs state)
          plugin_states changed_reprs in
      ok ({ all_constraints_ ; plugin_states ; aliases_ ; already_selected_and_propagators_ }, [])
    | new_constraint ->
      let%bind (already_selected_and_propagators_ , new_constraints') =
        select_and_propagate_all' already_selected_and_propagators_ new_constraint plugin_states in
      ok ({ all_constraints_ ; plugin_states ; aliases_ ; already_selected_and_propagators_ }, new_constraints')

  let simplify_constraint : type_constraint -> type_constraint_simpl list =
    fun new_constraint ->
    Normalizer.type_constraint_simpl new_constraint

  let rec until predicate f state = if predicate state then ok state else let%bind state = f state in until predicate f state

  (* Takes a list of constraints, applies all selector+propagator pairs
     to each in turn. *)
  let rec select_and_propagate_all : typer_state -> type_constraint list -> typer_state result =
    fun state new_constraints ->
    (* To change the order in which the constraints are processed, modify this loop. *)
    until
      (function (_, []) -> true | _ -> false)
      (fun (state, constraints) ->
         let new_constraints = List.flatten @@ List.map simplify_constraint new_constraints in
         let%bind (state, new_constraints) = bind_fold_map_list add_constraint_and_propagate state new_constraints in
         ok (state, List.flatten new_constraints @ constraints))
      (state, new_constraints)
    >>|? fst
     (* already_selected_and_propagators_ ; all_constraints_ ; plugin_states ; aliases_ *)

  let placeholder_for_state_of_new_typer : typer_state =
    let module MapCreateState = Plugins.Indexers.MapPlugins(CreateState) in
    let plugin_states = MapCreateState.f () pluginFieldsUnit in
    {
      all_constraints_                  = [] ;
      aliases_                          = UnionFind.Poly2.empty Var.pp Var.compare ;
      plugin_states                    = plugin_states ;
      already_selected_and_propagators_ = List.map init_propagator_heuristic propagator_heuristics ;
    }

end

(* Instantiate the solver with a selection of plugins *)
module Solver = MakeSolver(Plugins)

(*  ………………………………………………………………………………………………… Plugin-based solver above ………………………………………………………………………………………………… *)


let initial_state : _ typer_state = {
    structured_dbs =
      {
        all_constraints            = ([] : type_constraint_simpl list) ;
        aliases                    = UF.empty Var.pp Var.compare;
        assignments                = (Map.create ~cmp:Var.compare : (type_variable, c_constructor_simpl) Map.t);
        grouped_by_variable        = (Map.create ~cmp:Var.compare : (type_variable,         constraints) Map.t);
        cycle_detection_toposort   = ();
        by_constraint_identifier   = (Map.create ~cmp:Ast_typed.Compare.constraint_identifier : (constraint_identifier, c_typeclass_simpl) Map.t);
        refined_typeclasses        = (Map.create ~cmp:Ast_typed.Compare.constraint_identifier : (constraint_identifier, refined_typeclass) Map.t);
        refined_typeclasses_back   = (Map.create ~cmp:Ast_typed.Compare.constraint_identifier : (constraint_identifier, constraint_identifier) Map.t);
        typeclasses_constrained_by = (Map.create ~cmp:Var.compare)
      } ;
    already_selected_and_propagators = List.map init_propagator_heuristic propagator_heuristics
  }

(* ============================================================================ *)

(* TODO : with our selectors, the selection depends on the order in which the constraints are added :-( :-( :-( :-(
   We need to return a lazy stream of constraints. *)

(* The order in which the propagators are applied to constraints is
   entirely accidental (dfs/bfs/something in-between). *)

(* sub-component: constraint selector (worklist / dynamic queries) *)
let select_and_propagate : 'old_input 'selector_output . ('old_input, 'selector_output) selector -> ('selector_output , typer_error) propagator -> 'selector_output poly_set -> 'old_input -> structured_dbs -> ('selector_output poly_set * updates) result =
  fun selector propagator ->
  fun already_selected old_type_constraint dbs ->
  (* TODO: thread some state to know which selector outputs were already seen *)
  let selected_outputs = selector old_type_constraint dbs in
  let { Set.set = already_selected ; duplicates = _ ; added = selected_outputs } = Set.add_list selected_outputs already_selected in
  (* Call the propagation rule *)
  let%bind new_constraints = bind_map_list propagator private_storage selected_outputs in
  (* return so that the new constraints are pushed to some kind of work queue *)
  let () =
    if Ast_typed.Debug.debug_new_typer && false then
      Printf.fprintf stderr "%s" @@ Format.asprintf "propagator produced\nupdates = %a\n"
        Ast_typed.PP.updates_list
        new_constraints
  in
  ok (already_selected , List.flatten new_constraints)

let apply_removals : (type_constraint list * structured_dbs) -> update -> (type_constraint list * structured_dbs) result =
  fun (acc, dbs) update ->
  let%bind dbs' = bind_fold_list Normalizer.normalizers_remove dbs update.remove_constraints in
  (* TODO: don't append list like this, it's inefficient. *)
  ok @@ (acc @ update.add_constraints, dbs')

let apply_multiple_removals : update list -> structured_dbs -> (type_constraint list * structured_dbs) result =
  fun updates dbs ->
  bind_fold_list apply_removals ([], dbs) updates

let select_and_propagate_one :
  type_constraint_simpl selector_input ->
  typer_error ex_propagator_state list * type_constraint list * structured_dbs ->
  typer_error ex_propagator_state ->
  (typer_error ex_propagator_state list * type_constraint list * structured_dbs) result =
  fun
    new_constraint
    (new_states , new_constraints , dbs)
    (Propagator_state { selector; propagator; printer ; printer_json ; already_selected ; private_storage }) ->
  let sel_propag = (select_and_propagate selector propagator) in
  let%bind (already_selected , private_storage, updates) =
    sel_propag already_selected private_storage new_constraint dbs in
  let%bind new_constraints'', dbs = apply_multiple_removals updates dbs in
  ok @@ (
    (Propagator_state { selector; propagator; printer ; printer_json ; already_selected ; private_storage }
     :: new_states),
    new_constraints'' @ new_constraints,
    dbs
  )

(* Takes a constraint, applies all selector+propagator pairs to it.
   Keeps track of which constraints have already been selected. *)
let select_and_propagate_all' : typer_error ex_propagator_state list -> type_constraint_simpl selector_input -> structured_dbs -> (typer_error ex_propagator_state list * type_constraint list * structured_dbs) result =
  fun already_selected_and_propagators new_constraint dbs ->
  bind_fold_list
    (select_and_propagate_one new_constraint)
    ([], [] , dbs)
    already_selected_and_propagators

(* Takes a list of constraints, applies all selector+propagator pairs
   to each in turn. *)
let rec select_and_propagate_all : _ typer_state -> type_constraint selector_input list -> _ typer_state result =
  fun { already_selected_and_propagators ; structured_dbs } new_constraints ->
  match new_constraints with
  | [] -> ok { already_selected_and_propagators ; structured_dbs }
  | new_constraint :: tl ->
     let { state = dbs ; list = modified_constraints } = Normalizer.normalizers new_constraint structured_dbs in
     let%bind (already_selected_and_propagators , new_constraints' , structured_dbs) =
       bind_fold_list
         (fun (already_selected , nc , dbs) c ->
           let%bind (already_selected , new_constraints' , dbs) = select_and_propagate_all' already_selected c dbs in
           ok (already_selected , new_constraints' @ nc , dbs))
         (already_selected_and_propagators , [] , dbs)
         modified_constraints in
     (* DFS *)
     (* TODO: find a way to treat the constraints in a more appropriate order *)
     let new_constraints = new_constraints' @ tl in
     select_and_propagate_all { already_selected_and_propagators ; structured_dbs } new_constraints

(* This is the solver. *)
let aggregate_constraints = select_and_propagate_all



(* Later on, we'll ensure that all the heuristics register the
   existential/unification variables that they create, as well as the
   new constraints that they create. We will then check that they only
   use a small set of core axioms to derive new constraints, and
   produce traces justifying that instanciations satisfy all related
   constraints, and that all existential variables are instantiated
   (possibly by first generalizing the type and then using the
   polymorphic type argument to instantiate the existential). *)



(* This function is called when a program is fully compiled, and the
   typechecker's state is discarded. TODO: either get rid of the state
   earlier, or perform a sanity check here (e.g. that types have been
   inferred for all bindings and expressions, etc.

   Also, we should check at these places that we indeed do not need the
   state any further. Suzanne *)
let discard_state (_ : _ typer_state) = ()

let placeholder_for_state_of_new_typer () = initial_state
