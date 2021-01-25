open Trace
open Ast_typed.Types

module SRope = Rope.SimpleRope

let rec until predicate f state = if predicate state then ok state else let%bind state = f state in until predicate f state

(* TODO: replace this with a more efficient SRope.t (needs a "pop" function) *)
module Pending = struct
  type 'a t = { l : 'a list }
  let empty : 'a t = { l = [] }
  let add : 'a -> 'a t -> 'a t = fun x { l } -> { l = x :: l }
  let add_list : 'a list -> 'a t -> 'a t = fun l1 { l } -> { l = l1 @ l }
  let union : 'a t -> 'a t -> 'a t = fun { l = l1 } { l = l2 } -> { l = l1 @ l2 }
  let of_list : 'a list -> 'a t = fun l -> { l }
  let singleton : 'a -> 'a t = fun x -> { l = [x] }
  let to_list : 'a t -> 'a list = fun { l } -> l
  let pop : 'a t -> ('a * 'a t) option  = function
      { l = [] } -> None
    | { l = hd :: tl } -> Some (hd, { l = tl })
  let is_empty : 'a t -> bool = function { l = [] } -> true | _ -> false
end

type worklist_ = {
  (* I don't know how to "open" the module to get only the fields for { w with … = … } or w.…, so I'm declaring this outside of the module. *)
  pending_type_constraint                        : type_constraint       Pending.t;
  pending_filtered_not_already_added_constraints : type_constraint       Pending.t;
  pending_type_constraint_simpl                  : type_constraint_simpl Pending.t;
  pending_c_alias                                : c_alias               Pending.t;
  pending_non_alias                              : type_constraint_simpl Pending.t;
}

type ('part, 'whole) mini_lens = { get : 'whole -> 'part Pending.t; set : 'whole -> 'part Pending.t -> 'whole; }   

module Worklist = struct
  type t = worklist_

  let decrement_has_timeout_expired time_to_live = 
    let () = time_to_live := !time_to_live - 1 in
    if (!time_to_live) = 0
    then (Format.printf "timeout 78909765.\n"; true)
    else false

  let show_sizes 
      { pending_type_constraint;
        pending_filtered_not_already_added_constraints;
        pending_type_constraint_simpl;
        pending_c_alias;
        pending_non_alias } =
    Printf.fprintf stderr "size(worklist)=(%d | %d | %d | %d | %d)\n"
      (List.length @@ Pending.to_list pending_type_constraint                       )
      (List.length @@ Pending.to_list pending_filtered_not_already_added_constraints)
      (List.length @@ Pending.to_list pending_type_constraint_simpl                 )
      (List.length @@ Pending.to_list pending_c_alias                               )
      (List.length @@ Pending.to_list pending_non_alias                             )

  let is_empty ~time_to_live
      (_state,
       ({ pending_type_constraint;
          pending_filtered_not_already_added_constraints;
          pending_type_constraint_simpl;
          pending_c_alias;
          pending_non_alias } as worklist)) =
    let () = show_sizes worklist in
    decrement_has_timeout_expired time_to_live
    || (Pending.is_empty pending_type_constraint                        &&
        Pending.is_empty pending_filtered_not_already_added_constraints &&
        Pending.is_empty pending_type_constraint_simpl                  &&
        Pending.is_empty pending_c_alias                                &&
        Pending.is_empty pending_non_alias                               )

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
        pending_non_alias                              = Pending.union new_worklist.pending_non_alias                                worklist.pending_non_alias                                ;
      }
      (* return the state updated by f, and the updated worklist (without the processed element, with the new tasks) *)
      in ok (state, merged_worklists)

  let process_all ~time_to_live lens (state, worklist) f =
    until (fun (_state, worklist) -> decrement_has_timeout_expired time_to_live || Pending.is_empty (lens.get worklist))
      (fun (state, worklist) -> process lens (state, worklist) f)
      (state, worklist)

  let empty = {
    (* TODO: these should be ropes *)
    pending_type_constraint                        = Pending.empty ;
    pending_filtered_not_already_added_constraints = Pending.empty ;
    pending_type_constraint_simpl                  = Pending.empty ;
    pending_c_alias                                = Pending.empty ;
    pending_non_alias                              = Pending.empty ;
  }
end

let pending_type_constraint                        = { get = (fun { pending_type_constraint                        = x ; _ } -> x); set = (fun w x -> { w with pending_type_constraint                        = x }) }
let pending_filtered_not_already_added_constraints = { get = (fun { pending_filtered_not_already_added_constraints = x ; _ } -> x); set = (fun w x -> { w with pending_filtered_not_already_added_constraints = x }) }
let pending_type_constraint_simpl                  = { get = (fun { pending_type_constraint_simpl                  = x ; _ } -> x); set = (fun w x -> { w with pending_type_constraint_simpl                  = x }) }
let pending_c_alias                                = { get = (fun { pending_c_alias                                = x ; _ } -> x); set = (fun w x -> { w with pending_c_alias                                = x }) }
let pending_non_alias                              = { get = (fun { pending_non_alias                              = x ; _ } -> x); set = (fun w x -> { w with pending_non_alias                              = x }) }
