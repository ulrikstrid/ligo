open Trace
open Ast_typed.Types
module Set = RedBlackTrees.PolySet

type 'old_constraint_type selector_input = 'old_constraint_type (* some info about the constraint just added, so that we know what to look for *)
type 'selector_output selector_outputs = 'selector_output list
(* type ('old_contraint_type, 'selector_output) selector = 'old_constraint_type selector_input -> structured_dbs -> 'selector_output selector_outputs *)
type ('selector_output , 'errors) propagator = 'selector_output -> (type_variable -> type_variable) -> (updates, 'errors) result

type ('selector_output, -'flds) selector = (type_variable -> type_variable) -> type_constraint_simpl -> 'flds -> 'selector_output list

(* This is module hell. *)
module Heuristic_plugin_M =
  functor (Required_flds : sig module type S end) -> struct
    module type S = functor (Available_flds : Required_flds.S) -> sig
      type selector_output
      (* Finds in the databases tuples of constraints which are
         interesting for this plugin and include the given
         type_constraint_simpl. *)
      val selector     : (* (selector_output, (module Required_flds)) selector *)
        (type_variable -> type_variable) -> type_constraint_simpl -> (*flds ->*) selector_output list
      (* Select in the databases tuples of constraints which are
         interesting and involve the given two type_viables, knowing that
         these are about to be merged. This function is called before the
         database's merge_aliases functions are called (i.e. the database
         does not reflect the effects of the merge yet). *)
      val alias_selector : type_variable -> type_variable -> (*(module Required_flds) ->*) selector_output list
      val get_referenced_constraints : selector_output -> type_constraint_simpl list
      (* called when two 'data are associated with the same type_constraint *)
      val propagator   : (selector_output , Ast_typed.Typer_errors.typer_error) propagator
      val printer      : Format.formatter -> selector_output -> unit
      val printer_json : selector_output -> Yojson.Safe.t
      val comparator   : selector_output -> selector_output -> int
    end
  end

module type Heuristic_plugin = sig
  module Required_flds : sig
    module type S
  end
  val heuristic_name : string
  module M : Heuristic_plugin_M(Required_flds).S
end

(* This is module hell. *)
module Ex_heuristic_state =
  functor (Required_flds : sig module type S end) -> struct
    module type S = functor (Available_flds : Required_flds.S) -> sig
      type selector_output
      (* Finds in the databases tuples of constraints which are
         interesting for this plugin and include the given
         type_constraint_simpl. *)
      val selector     : (* (selector_output, (module Required_flds)) selector *)
        (type_variable -> type_variable) -> type_constraint_simpl -> (*flds ->*) selector_output list
      (* Select in the databases tuples of constraints which are
         interesting and involve the given two type_viables, knowing that
         these are about to be merged. This function is called before the
         database's merge_aliases functions are called (i.e. the database
         does not reflect the effects of the merge yet). *)
      val alias_selector : type_variable -> type_variable -> (*(module Required_flds) ->*) selector_output list
      val get_referenced_constraints : selector_output -> type_constraint_simpl list
      (* called when two 'data are associated with the same type_constraint *)
      val propagator   : (selector_output , Ast_typed.Typer_errors.typer_error) propagator
      val printer      : Format.formatter -> selector_output -> unit
      val printer_json : selector_output -> Yojson.Safe.t
      val comparator   : selector_output -> selector_output -> int
      val already_selected : selector_output
    end
  end

(* module type Ex_heuristic_state = sig
 *   module Plugin : Heuristic_plugin
 *     
 *   (\* module Required_flds : sig
 *    *   module type S
 *    * end
 *    * module type S = functor (Available_flds : Required_flds.S) -> sig
 *    *   module Plugin : Heuristic_plugin_M(Required_flds).S *\)
 *   val already_selected : Plugin.M(Plugin.Required_flds).selector_output Set.t
 * end *)

(* module type Ex_heuristic_selector = sig
 *   module Plugin : Heuristic_plugin
 *   val selector_output : Plugin.selector_output List.t
 * end *)

(* module Heuristic_state = functor (Selector_output : sig type t end) (Flds : sig type 'self t end) -> struct
 *   module type S = sig
 *     module Plugin : Heuristic_plugin(Selector_output)(Flds).S
 *     val already_selected : Selector_output.t Set.t
 *   end
 * end
 * 
 * module Ex_heuristic_plugin = functor (Flds : sig type 'self t end) -> struct
 *   module type S = sig
 *     module Selector_output : sig type t end
 *     module M : Heuristic_plugin(Selector_output)(Flds).S
 *   end
 * end *)

(* type ('selector_output, -'flds) heuristic_plugin = {
 *   heuristic_name : string ;
 *   (\* Finds in the databases tuples of constraints which are
 *      interesting for this plugin and include the given
 *      type_constraint_simpl. *\)
 *   selector     : ('selector_output, 'flds) selector ;
 *   (\* Select in the databases tuples of constraints which are
 *      interesting and involve the given two type_viables, knowing that
 *      these are about to be merged. This function is called before the
 *      database's merge_aliases functions are called (i.e. the database
 *      does not reflect the effects of the merge yet). *\)
 *   alias_selector : type_variable -> type_variable -> 'flds -> 'selector_output list ;
 *   get_referenced_constraints : 'selector_output -> type_constraint_simpl list ;
 *   (\* called when two 'data are associated with the same type_constraint *\)
 *   propagator   : ('selector_output , Ast_typed.Typer_errors.typer_error) propagator ;
 *   printer      : Format.formatter -> 'selector_output -> unit ;
 *   printer_json : 'selector_output -> Yojson.Safe.t ;
 *   comparator   : 'selector_output -> 'selector_output -> int ;
 * } *)

(* type ('selector_output, -'flds) heuristic_state = {
 *   plugin : ('selector_output, 'flds) heuristic_plugin ;
 *   already_selected : 'selector_output Set.t ;
 * }
 * 
 * type -'flds ex_heuristic_plugin =
 *     Heuristic_plugin : ('selector_output, 'flds) heuristic_plugin -> 'flds ex_heuristic_plugin
 * 
 * type -'flds ex_heuristic_state =
 *     Heuristic_state : ('selector_output, 'flds) heuristic_state -> 'flds ex_heuristic_state
 * 
 * type -'flds ex_heuristic_selector =
 *     Heuristic_selector: ('selector_output, 'flds) heuristic_state * 'selector_output selector_outputs -> 'flds ex_heuristic_selector
 * 
 * module type Heuristic_selector = sig
 *   type 'type_variable flds
 *   type selector_output
 *   val state : (selector_output, type_variable flds) heuristic_state
 *   val outputs : selector_output selector_outputs
 * end
 * 
 * type 'flds heuristic_plugins = 'flds ex_heuristic_plugin list *)

module type Plugins = sig
  module Indexers : IndexerPlugins

  module Available_flds : sig
    module type S = Indexers.PluginFields(PerPluginState).Flds
  end

  val heuristics : (module Heuristic_plugin_M(Available_flds).S) list
  (* module Indexers : IndexerPlugins
   * val heuristics : (module Indexers.PluginFields(PerPluginState).Flds) heuristic_plugins *)
end

module Typer_state =
  functor (* (Errors : sig type t end) *) (Indexer_plugin_states : sig module type S end) -> struct
    module type S = sig
      val all_constraints                  : type_constraint_simpl PolySet.t
      val added_constraints                : type_constraint PolySet.t
      val deleted_constraints              : type_constraint_simpl PolySet.t
      val aliases                          : type_variable UnionFind.Poly2.t
      val indexer_plugin_states            : (module Indexer_plugin_states.S)
      val already_selected_and_propagators : (module Ex_heuristic_state(Indexer_plugin_states).S) list
    end
  end

open Format
open PP_helpers

let pp_already_selected = fun printer ppf set ->
  let lst = (RedBlackTrees.PolySet.elements set) in
    Format.fprintf ppf "Set [@,@[<hv 2> %a @]@,]" (list_sep printer (fun ppf () -> fprintf ppf " ;@ ")) lst

(* let pp_ex_propagator_state = fun ppf (Heuristic_state { plugin = { selector ; propagator ; printer ; printer_json=_ } ; already_selected }) ->
 *   ignore ( selector, propagator );
 *   Format.fprintf ppf "{ selector = (\* OCaml function *\); propagator = (\* OCaml function *\); already_selected = %a }"
 *   (pp_already_selected printer) already_selected *)

let json_already_selected = fun printer_json set : Yojson.Safe.t ->
  let lst = (RedBlackTrees.PolySet.elements set) in
let list f lst = `List (List.map f lst) in
    `List [`String "Set"; (list printer_json lst)]

(* let json_ex_propagator_state = fun (Heuristic_state { plugin = { selector; propagator; printer=_ ; printer_json } ; already_selected }) : Yojson.Safe.t ->
 *   ignore (selector,propagator);
 *   `Assoc[ ("selector", `String "OCaml function"); ("propagator", `String "OCaml function"); ("already_selected" ,          (json_already_selected printer_json) already_selected)] *)

(* TODO: remove lift_state_list_monad and lift: not needed after moving to plugin system *)
(* state+list monad *)
type ('state, 'elt) state_list_monad = { state: 'state ; list : 'elt list }
let lift_state_list_monad ~state ~list = { state ; list }
let lift f =
  fun { state ; list } ->
    let (new_state , new_lists) = List.fold_map_acc f state list in
    { state = new_state ; list = List.flatten new_lists }
