(** Updates the dbs.grouped_by_variable field when new constraints are
   discovered.

    This field contains a map from type variables to lists of
   constraints that are related to that variable (in other words, the
   key appears in the equation).
 *)

open Ast_typed.Types
open UnionFind
open Trace

(* the state is 3 maps from (unionfind) variables to constraints containing them *)

type 'typeVariable t = {
  constructor : ('typeVariable, c_constructor_simpl PolySet.t) ReprMap.t ;
  poly        : ('typeVariable, c_poly_simpl PolySet.t) ReprMap.t ;
  row         : ('typeVariable, c_row_simpl PolySet.t) ReprMap.t ;
}

let create_state ~cmp =
  { constructor = ReprMap.create ~cmp ~merge:PolySet.union ;
    poly        = ReprMap.create ~cmp ~merge:PolySet.union ;
    row         = ReprMap.create ~cmp ~merge:PolySet.union ;}

let update_add_to_constraint_set ~cmp c = function
    None -> PolySet.add c (PolySet.create ~cmp)
  | Some s -> PolySet.add c s

let add_constraint repr (state : _ t) new_constraint =
  match new_constraint with
    SC_Constructor c -> { state with constructor = ReprMap.monotonic_update (repr c.tv) (update_add_to_constraint_set ~cmp:Ast_typed.Compare.c_constructor_simpl c) state.constructor }
  | SC_Row         c -> { state with row         = ReprMap.monotonic_update (repr c.tv) (update_add_to_constraint_set ~cmp:Ast_typed.Compare.c_row_simpl         c) state.row         }
  | SC_Poly        c -> { state with poly        = ReprMap.monotonic_update (repr c.tv) (update_add_to_constraint_set ~cmp:Ast_typed.Compare.c_poly_simpl        c) state.poly        }
  | SC_Typeclass   _ -> state (* Handled by a different indexer (typeclasses_constraining *)
  | SC_Alias       _ -> failwith "TODO: impossible: tc_alias handled in main solver loop"

(* Exception: Could not remove c because the database index entry is missing for the lhs of this constraint (e.g. a = b(c,d) : there is no entry for constraints which are constraining a) *)
exception CouldNotRemove of type_constraint_simpl

let update_remove_constraint_from_set tcs c = function
    None -> raise (CouldNotRemove tcs)
  | Some s -> PolySet.remove c s


let remove_constraint repr (state : _ t) constraint_to_rm =
  (* This update is "monotonic" as required by ReprMap + the solver.
     The `add` function of this indexer is only called once per
     constraint, and constraints are indexed by their lhs variable.
     Therefore, in any sequence of

     add c (* adds repr(v) → c in the multimap *)
     merge v other_v (* one or more merges + unrelated operations *)
     remove c (* removes new_repr(v) → c from the multimap *)

     the order of operations does not matter: the remove is always
     after the add, any merges before or after the add don't cause
     any problem. Indeed, the constraint appears only once in the
     multimap, and the removal will find it using the correct repr()
     at the time of removal. *)
  match
    (
      match constraint_to_rm with
        SC_Constructor c -> { state with constructor = ReprMap.monotonic_update (repr c.tv) (update_remove_constraint_from_set constraint_to_rm c) state.constructor }
      | SC_Row         c -> { state with row         = ReprMap.monotonic_update (repr c.tv) (update_remove_constraint_from_set constraint_to_rm c) state.row         }
      | SC_Poly        c -> { state with poly        = ReprMap.monotonic_update (repr c.tv) (update_remove_constraint_from_set constraint_to_rm c) state.poly        }
      | SC_Typeclass   _ -> state
      | SC_Alias       _ -> failwith "TODO: impossible: tc_alias handled in main solver loop and aliasing constraints cannot be removed"
    )
  with
  exception CouldNotRemove c -> fail (Typer_common.Errors.could_not_remove c)
  | result -> ok result

let merge_aliases =
  fun updater { constructor ; poly ; row } -> {
      constructor = updater.map constructor ;
      poly       = updater.map poly ;
      row        = updater.map row ;
    }

let pp type_variable ppf (state : _ t) =
  let open PP_helpers in
  Format.fprintf ppf "{ constructor = %a ; row = %a ; poly = %a }"
  (list_sep_d (pair type_variable (PolySet.pp Ast_typed.PP.c_constructor_simpl))) (ReprMap.bindings state.constructor)
  (list_sep_d (pair type_variable (PolySet.pp Ast_typed.PP.c_row_simpl))) (ReprMap.bindings state.row)
  (list_sep_d (pair type_variable (PolySet.pp Ast_typed.PP.c_poly_simpl))) (ReprMap.bindings state.poly)

let name = "grouped_by_variable"

let get_constructors_by_lhs : 'type_variable -> 'type_variable t -> c_constructor_simpl PolySet.t =
  fun variable state ->
  match ReprMap.find_opt variable state.constructor with
    Some s -> s
  | None -> PolySet.create ~cmp:Ast_typed.Compare.c_constructor_simpl

let get_rows_by_lhs : 'type_variable -> 'type_variable t -> c_row_simpl PolySet.t =
  fun variable state ->
  match ReprMap.find_opt variable state.row with
    Some s -> s
  | None -> PolySet.create ~cmp:Ast_typed.Compare.c_row_simpl

let get_polys_by_lhs : 'type_variable -> 'type_variable t -> c_poly_simpl PolySet.t =
  fun variable state ->
  match ReprMap.find_opt variable state.poly with
    Some s -> s 
  | None -> PolySet.create ~cmp:Ast_typed.Compare.c_poly_simpl


type 'typeVariable t_for_tests = {
  constructor : ('typeVariable * c_constructor_simpl PolySet.t) list ;
  poly        : ('typeVariable * c_poly_simpl PolySet.t) list ;
  row         : ('typeVariable * c_row_simpl PolySet.t) list ;
}

let constructor_bindings : 'type_variable t -> ('type_variable * c_constructor_simpl PolySet.t) list = fun state -> ReprMap.bindings state.constructor
let row_bindings : 'type_variable t -> ('type_variable * c_row_simpl PolySet.t) list = fun state -> ReprMap.bindings state.row
let poly_bindings : 'type_variable t -> ('type_variable * c_poly_simpl PolySet.t) list = fun state -> ReprMap.bindings state.poly

let bindings (state : _ t) : _ t_for_tests  = { constructor = constructor_bindings state ; row = row_bindings state ; poly = poly_bindings state }
