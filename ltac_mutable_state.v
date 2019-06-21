Require List.

Module ltac_mutable_state'.
(* We create a list of binary trees, where each tree has a depth
   equal to its offset in the list. The leaves of these trees are
   existential variables. This allows us to find a writable spot
   in a shared global mutable store in O(log n) time.

   The trees and list are defined using the same data structure.

   The "full" field indicates that the left subtree
   is full when it is set to tt. When it is still an existential
   variable, the left subtree is not full. *)
Module T.
Inductive tree :=
| Node (full : unit) (left : tree) (right : tree)
| Leaf (v : Prop)
| Closed.
End T.
Import T.

(* 
Require Import FunInd.
Function lala (t : tree) {struct t} : Prop := match t with
| Node _ l _ => lala l
| Leaf v => v
end.

Print lala_ind. *)

Import List.

Fixpoint to_list' t acc :=
match t with
| Node _ l r =>
  let acc'  := to_list' r acc in to_list' l acc'
| Leaf v => v :: acc
| Closed => acc
end.

Definition to_list t := to_list' t nil.

Ltac init_state name :=
evar (name : tree).
 
Ltac alloc ex k :=
  let full := fresh "full" in
  let left := fresh "left" in
  let right := fresh "right" in
  evar(full : unit);
  evar(left : tree);
  evar(right : tree);
  unify ex (Node full left right); (* intantiate a new list element *)
  clear full left right;
  k ex.
Tactic Notation "alloc" constr(ex) tactic(k) := alloc ex k.

Ltac find_hole_tree tree max_depth kmark k :=
let max_depth_minus_1 := lazymatch max_depth with S ?n => n end in
lazymatch tree with
| Node tt ?left ?right => find_hole_tree right max_depth_minus_1 kmark k
| Node ?ex ?left ?right =>
  (* when moving upwards, the parent of the first left child should have
     its full bit set (it might already be set) *)
  let mark _ := unify ex tt in
  find_hole_tree left max_depth_minus_1 mark k
| ?ex => lazymatch max_depth_minus_1 with
  | 0 => let hole := fresh "hole" in
         evar(hole : Prop);
         unify ex (Leaf hole);
         clear hole;
         kmark ();
         match ex with (Leaf ?hole') => k hole' end
  | _ => alloc ex fun ex => find_hole_tree ex max_depth kmark k
  end
end.

Ltac close' list_or_tree :=
lazymatch list_or_tree with
| Node tt ?left ?right => close' right
| Node ?ex ?left ?right => unify ex tt; unify right Closed; close' left
| ?ex => unify ex Closed
end.

Ltac find_hole_list list_of_trees max_depth_of_tree k :=
lazymatch list_of_trees with
| Node tt ?hd ?tl =>
  find_hole_list tl constr:(S max_depth_of_tree) k
| Node ?ex ?hd ?tl =>
  let mark _ := unify ex tt in
  find_hole_tree hd max_depth_of_tree mark k
| ?ex => alloc ex fun ex =>
  find_hole_list ex max_depth_of_tree k
end.

Ltac main_find_hole' k :=
lazymatch goal with
uu := ?vuu : tree |- _ => (* TODO: find the correct hyp *)
  find_hole_list vuu 1 k
end.

Ltac set_hole v :=
  let k ex := unify ex v in
main_find_hole' k.

Ltac close_state :=
(* TODO: avoid duplication with main_find_hole' *)
lazymatch goal with
uu := ?vuu : tree |- _ => (* TODO: find the correct hyp *)
  close' vuu
end.

Ltac get_state _ :=
(* TODO: avoid duplication with main_find_hole' *)
lazymatch goal with
uu := ?vuu : tree |- _ => (* TODO: find the correct hyp *)
  let vuu' := eval unfold to_list in (to_list vuu) in
  let vuu'' := eval unfold to_list' in vuu' in
  vuu''
end.

End ltac_mutable_state'.

Ltac set_hole v := ltac_mutable_state'.set_hole v.
Ltac init_state n := ltac_mutable_state'.init_state n.
Ltac close_state := ltac_mutable_state'.close_state.
Ltac get_state n := ltac_mutable_state'.get_state n.
Notation ", a , b" := (ltac_mutable_state'.T.Node _ a b) (only printing, right associativity, at level 0).
Notation "( a )" := (ltac_mutable_state'.T.Leaf a) (only printing, at level 0).
Notation "(mutable storage)" := ltac_mutable_state'.T.tree (only printing).
Notation "." := ltac_mutable_state'.T.Closed (only printing).
