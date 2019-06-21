(* Run some tactics on a goal without polluting the proof term. *)

Require Import ltac_get_all_hyps.
Require Import ltac_hidefalso.

Ltac close_goal :=
  exfalso;
  match goal with
    fals : False' |- _ =>
      apply (extract_False fals)
  end.

Inductive any {a : Type} (p : a) : Prop := Any.

Ltac tulu to_run :=
let fals   := fresh "fals" in
let result := fresh "result" in
let hg'    := fresh "hg" in
evar(result : Prop);
intro fals;
intro hg';
let G := match goal with HG' : ?G |- _ => match HG' with hg' => G end end in
assert (False' -> G);
[>
  clear;
  intro fals;
  restore_goal_and_hyps;
  (* do some work and store in the result *)
(* TODO: make it accept any type *)
  let ret := (fun x =>
    instantiate(result := any x);
    close_goal)
  in
  to_run ret
| (* close goal *)
  exfalso;
  apply (extract_False fals)
].

Ltac sandbox to_run :=
let hg := get_goal_and_hyps () true in
let result_term := constr:(ltac:(tulu to_run) : False' -> hg -> False) in
match result_term with
  (let _ := (any ?result) in _) => result end.















