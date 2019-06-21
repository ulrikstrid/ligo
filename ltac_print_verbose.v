Require Import ltac_get_all_hyps.

Definition dummy (x : False) : False := x.
Print dummy.
Declare Reduction print := unfold dummy.

Inductive Toto := | TOTO {a : Type} (p : a).

Ltac dosb :=
clear;
let toto := fresh "toto" in
let fals := fresh "fals" in
evar (toto : Toto);
clear toto;
intros fals;
intros;
let gh := get_goal_and_hyps fals false in
instantiate (toto := @TOTO _ gh);
destruct fals.

Ltac blahblah x :=
let sb := constr:(ltac:(dosb) : False -> x) in
match sb with
  (let _ := TOTO ?p in _) => exact p
end.

Ltac blahblah' x := match tt with
  _ => let x' := eval unfold x in x in blahblah x'
| _ => blahblah x
end.

Ltac verbose_detect_sort x :=
match tt with
| _ => let x' := constr:(x : Set)  in idtac x; blahblah' x
| _ => let x' := constr:(x : Type) in idtac x; blahblah' x
| _ => let x' := constr:(x : Prop) in idtac x; blahblah' x
| _ => let x' := type of x in idtac "type of (" x ")"; blahblah x'
end.

Notation "'verbose' t" := (ltac:(verbose_detect_sort t) : _)
  (only parsing, at level 0).
