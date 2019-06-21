(* original file under license CC0, can be licensed as MIT, BSD, LGPL, GPL etc. *)

Require Import Omega.
Require Import Bool.

Ltac obviously' :=
  try split; repeat (
    auto;
    simpl;
    try reflexivity;
    try contradiction;
    try discriminate;
    try intuition;
    try omega).

(* TODO: this is O(N²) because on each succeding hypothesis it
   restarts from the start *)
Ltac map_hypotheses tac :=
repeat (
  match goal with
  | [ H : _ |- _ ] => tac H
  | [ |- _ ] => idtac
  end
).

Ltac break_bools := repeat(match goal with
H : ?a && ?b = true |- _ => apply Bool.andb_true_iff in H
end).

Ltac obviously1 :=
intros; (* consume foo -> … *)
obviously'; (* when there are no hypotheses *)
(* TODO: this is O(N²) because on each succeding hypothesis it
   restarts from the start *)
repeat(match goal with
  | [ H : bool |- _ ] =>
    (* TODO: do induction on H only if obviously
       actually makes progress on one branch (or both) *)
    progress (induction H; obviously')
  | [ H : ?Ty |- _ ] =>
    lazymatch Ty with
    | _ = true => progress rewrite -> H in *
    | true = _ => progress rewrite <- H in *
    | _ = false => progress rewrite -> H in *
    ;idtac "???->" H Ty
    | false = _ => progress rewrite <- H in *
    ;idtac "???<-" H Ty
    | ?a = ?b =>
      (
      tryif (is_var b) then
        (progress rewrite <- H in * )
      else
        (progress rewrite <- H in * )
      )
    | _ => idtac
(*
    | _ => progress (try rewrite -> H in *; try rewrite <- H in * )
*)
    end;
    break_bools; obviously'; break_bools
end).

Ltac by_definition_of' f :=
  repeat (unfold f in *; obviously1).

Ltac by_definition_of1 f :=
repeat (try intro); (* consume foo -> … *)
try split;
(* Try to expand P x twice if it's a boolean predicate. *)
match goal with
| [ H : f ?x |- _ ] => by_definition_of' f;
  match goal with
    [ H' : ?f' x = true |- _ ] =>
      match H' with
        H => idtac "unfolding" f' x "in" H';
             by_definition_of' f'
      end
  end
| [ |- _ ] => idtac
end;
by_definition_of' f.

Ltac by_definition1 :=
  cbv; obviously1.
