(*
Require Import Mtac2.Mtac2.
Import T.

Definition aaa z := 42 + z.

Locate "set".

Let toto1 := (intros x y H)%tactic.
Let toto2 := (intros)%tactic.
Let toto3 := (intros)%tactic.
Print toto.
Print gtactic.
Print gs_open.
Print intros_all.

Ltac lala t :=
idtac t;
mrun t.

Goal forall (x y : nat), x = 2 -> x+3 = 5.
lala toto.
MProof.
let f := intros x y in
f &> intros H.
rewrite H.
reflexivity.


PAUSE.*)


Require Import Omega.
Require Import ltac_utils.
Require List.

Require Import ltac_mutable_state.
Require Import ltac_get_all_hyps.
Require Import ltac_sandbox.

Inductive Step {A : Type} (x : A) : Prop := Step'.

(*
Inductive tac :=
| intro {a : Type} (app : forall {b}, (a -> b) -> b)
| rewrite {a : Prop} (p : a)
| auto.

Tactic Notation "trace" constr(f) :=
idtac f;
set_hole (Step f);
lazymatch f with
  @intro ?a ?app => intros ?
| @rewrite ?a ?p => rewrite p
| auto           => auto
end.

Tactic Notation "intro'"             := trace (intro (fun t => t)).
Tactic Notation "rewrite'" constr(H) := trace (rewrite H).
Tactic Notation "auto'"              := trace auto.
*)

Require Import Mtac2.Mtac2.
Import T.
Local Open Scope tactic.


Tactic Notation "trace'" constr(f) :=
idtac f;
set_hole (Step f);
mrun(f).
(*lazymatch f with
  @intro ?a ?app => intros ?
| @rewrite ?a ?p => rewrite p
| auto           => auto
end.*)


Definition aaa z := 42 + z.

Inductive show {a : Type} (p : a) : Prop := Show.
Arguments Show {a} {p}.

Ltac z t := idtac t; t.

(* TODO: keep track of the goal in the mutable storage, and compare
         with the actual one. *)

Goal forall (x y : nat), x = 2 -> y = 1 -> x+3 = 5.
init_state trace.

trace' (intros x y H J).

(* Force the inclusion of variable names: *)

Ltac do_n n f :=
let nn := eval vm_compute in n in
lazymatch nn with
  0 => idtac
| nn => f; let nnn := constr:(n-1) in do_n nnn f
end.
Tactic Notation "do_n" constr(n) tactic1(f) := do_n n f.

Tactic Notation "revert?" :=
lazymatch goal with
  H : _ |- _ => revert H
|       |- _ => fail "no hypothesis to revert"
end.

Ltac found g pos todo :=
assert g; only 1:
(
clear;
restore_goal_and_hyps;
idtac "call do_n";
do_n pos (revert?);
idtac "blah done do_n";
idtac g pos;
lazymatch goal with
H : _ |- _ => do_n pos (intros ?); todo H
end
).

Ltac count h n g todo :=
let nn := eval vm_compute in n in
idtac nn;
lazymatch goal with
  H' : _ |- _ => lazymatch H' with
   | h => found g nn todo
   | _ => revert H';
          let nnn := constr:(1+nn) in
          count h nnn g todo
   end
| |- _ => fail 9999 "hypothesis not found"
end.
Tactic Notation "count" constr(h) constr(n) constr(g) tactic1(todo) := count h n g todo.

clear trace.

(*let g := get_goal_and_hyps () true in
idtac g;
count H 0 g (fun x => idtac x).*)

let g := get_goal_and_hyps () true in
idtac g;
count H 0 g (fun x => rewrite x).

Inductive blah := Nil | Cons : forall (a : nat) (b : blah), blah.
Compute (fun (x : nat) => bool).
Abort.


(* WIP above: in a sandbox, repeat(revert) until we find H.
   Do the same number of repeats on the goal stored in the trace.

   TODO: Compare the goal+hyps of both versions. TODO TODO TODO

   If equal, we know the name for the hyp in the trace. *)

PAUSE.

(* TODO: store a list of names, via functions?

   TODO:
   think about what info we really need
     * name of an H for display
     * name of an H in the replayed goal
   think about what info we have available
     * name of H in real Hyps+Goal but only for display
     * pos of H in real Hyps+Goal
     * shadow goal as predicted by the trace?
   think about how to get from available to needed
   think about how to update available (new shadow goal?)

   Possibility 1: [ "intros H" + fun f g => f get_H_in_g;
                    "rewrite H" ~ fun h => rewrite h ]
   Possibility 2: [ "intros H" + (fun depth => depth+1)
                    "rewrite H" ~ (fun goal => depth_of_H … blah rewrite) *)

clear trace;
let g := get_goal_and_hyps () true in
idtac g;
assert g.
clear.
intros ???.
set(X := Show : (show (@eq nat x (S (S O))))).
clearbody X.
Print eq_refl.



init_state trace2.
set_hole (Step trace).
subst trace.
trace' (rewrite H).
trace' (reflexivity).
STOP.
trace (@intro Prop (fun b t => t (x = 42))).

MProof.
let f := intros x y in
f &> intros H &> rewrite H.

intro x.
init_state trace.

trace (@rewrite x).
trace (@intro Set (fun b t => t nat)).
trace (@intro Prop (fun b t => t (x = 42))).

let e := constr:(Step (rewrite H)) in
let te := type of e in idtac e te.



let sb := sandbox ltac:(fun ret =>
  assert (Step (rewrite H));
  [> clear trace; dbg;
     let hg := get_goal_and_hyps () true in
     ret hg
   | close_goal]) in
idtac "sandbox returned" sb.
match goal with
Z := (ltac_mutable_state'.T.Node _ (ltac_mutable_state'.T.Leaf ?G0) ?G) : ltac_mutable_state'.T.tree |- _ =>
 unify G0 (Step (fun (w : nat) (Z : w = 2) => rewrite Z))
end.
unify ?Goal0 (Leaf (Step H)).N

rewrite' H.
auto'.


intros.
rewrite H.
auto.
Qed.


