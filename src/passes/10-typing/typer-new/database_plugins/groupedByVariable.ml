(** Updates the dbs.grouped_by_variable field when new constraints are
   discovered.

    This field contains a map from type variables to lists of
   constraints that are related to that variable (in other words, the
   key appears in the equation).
 *)

open Ast_typed.Types
open UnionFind
open Trace

let merge : constraints -> constraints -> constraints = fun cs1 cs2 ->
  {
    constructor = cs1.constructor @ cs2.constructor ;
    poly        = cs1.poly        @ cs2.poly        ;
    row         = cs1.row         @ cs2.row         ;
  }

(* map from (unionfind) variables to constraints containing them *)
type 'typeVariable t = ('typeVariable, constraints) ReprMap.t
let create_state ~cmp =
  ReprMap.create ~cmp ~merge

let add_constraints_which_are_constraining_variable : _ -> type_variable -> constraints -> _ t -> _ t =
  fun repr variable c state ->
  ReprMap.monotonic_update (repr variable) (function None -> c | Some x -> merge c x) state

let add_constraint repr state new_constraint =
  match new_constraint with
    SC_Constructor c -> add_constraints_which_are_constraining_variable repr c.tv {constructor = [c] ; poly = []  ; (* tc = [] ; *) row = []} state
  | SC_Row         c -> add_constraints_which_are_constraining_variable repr c.tv {constructor = []  ; poly = []  ; (* tc = [] ; *) row = [c]} state
  | SC_Poly        c -> add_constraints_which_are_constraining_variable repr c.tv {constructor = []  ; poly = [c] ; (* tc = [] ; *) row = []} state
  | SC_Typeclass   _ -> state (* Handled by a different indexer (typeclasses_constraining *)
  | SC_Alias       _ -> failwith "TODO: impossible: tc_alias handled in main solver loop"

(* Exception: Could not remove c because the database index entry is missing for the lhs of this constraint (e.g. a = b(c,d) : there is no entry for constraints which are constraining a) *)
exception CouldNotRemove of type_constraint_simpl

let update_for_remove : type_constraint_simpl -> constraints -> constraints = fun c_to_remove cs ->
  match c_to_remove with
    SC_Constructor c_rm -> { cs with constructor = List.remove_element ~compare:Ast_typed.Compare.c_constructor_simpl c_rm cs.constructor }
  | SC_Row         c_rm -> { cs with row         = List.remove_element ~compare:Ast_typed.Compare.c_row_simpl         c_rm cs.row         }
  | SC_Poly        c_rm -> { cs with poly        = List.remove_element ~compare:Ast_typed.Compare.c_poly_simpl        c_rm cs.poly        }
  | SC_Typeclass   _ -> cs (* Handled by a different indexer (typeclasses_constraining *)
  | SC_Alias       _ -> failwith "TODO: impossible: tc_alias handled in main solver loop"

let rm_constraint_which_is_constraining_variable : _ -> type_variable -> type_constraint_simpl -> _ t -> (_ t, _) result =
  fun repr variable c state ->
  (* TODO: remove the empty set if a variable is not associated with any constraint after this removal. *)
  match
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
      ReprMap.monotonic_update (repr variable) (function
            None -> raise (CouldNotRemove c)
          | Some (x : constraints) -> update_for_remove c x)
        state
    with
    exception CouldNotRemove c -> fail (Typer_common.Errors.could_not_remove c)
    | result -> ok result

let remove_constraint repr state constraint_to_rm =
  match constraint_to_rm with
      SC_Constructor c -> rm_constraint_which_is_constraining_variable repr c.tv constraint_to_rm state
    | SC_Row         c -> rm_constraint_which_is_constraining_variable repr c.tv constraint_to_rm state
    | SC_Typeclass   _ -> ok state
    | SC_Poly        c -> rm_constraint_which_is_constraining_variable repr c.tv constraint_to_rm state
    | SC_Alias       _ -> failwith "TODO: impossible: tc_alias handled in main solver loop and aliasing constraints cannot be removed"

let merge_aliases =
  fun updater state -> updater.map state

let pp type_variable ppf state =
  let open PP_helpers in
  list_sep_d (pair type_variable Ast_typed.PP.constraints) ppf (ReprMap.bindings state)

let name = "grouped_by_variable"

let get_constraints_by_lhs : 'type_variable -> 'type_variable t -> constraints =
  fun variable state ->
  (* get the class of the variable *)
  match ReprMap.find_opt variable state with
    Some l -> l
  | None -> {
      constructor = [] ;
      poly        = [] ;
      (* tc          = [] ; *)
      row         = [] ;
    }
let bindings : 'type_variable t -> ('type_variable * constraints) list = fun state -> ReprMap.bindings state
