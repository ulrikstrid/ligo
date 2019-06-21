(* Get a list of hypotheses without producing garbage in the proof term *)

Module Marker.
Inductive marker {a : Type} (p : a) (visible : bool) := Marker.
End Marker.

Inductive any : Prop := | Any {a : Type} (p : a).

Notation Goal hs x :=
  (Marker.marker hs true -> x)
  (only parsing).

Notation "'Goal' x" :=
  (Marker.marker _ true -> x)
  (only printing, at level 200).

Notation "x" :=
  (Marker.marker _ false -> x)
  (only printing, at level 200).

Ltac revert_all_except except1 except2 except3 visible names :=
pose proof (Marker.Marker names visible) as MARKER;
repeat match goal with H : _ |- _ =>
  lazymatch H with
  | except1 => fail
  | except2 => fail
  | except3 => fail
  | _ => revert H end
end.

Ltac restore_goal_and_hyps :=
repeat (
  intros ?;
  lazymatch goal with
    H' : Marker.marker _ _ |- _ => fail
  | _ => idtac
  end
);
let MARKER := fresh "MARKER" in
intro MARKER;
clear MARKER.

Ltac goal' _ := lazymatch goal with |- ?G => G end.

Ltac continue_reify_goal fals reified_hyps_and_G except3 visible names :=
(* revert all hypotheses, again
   (except the fake ones used to store the goal and close via False h) *)
revert_all_except fals reified_hyps_and_G except3 visible names;

(* Save the goal in hyps_and_G *)
let hyps_and_G' := goal' () in
let hyps_and_G := constr:(Any hyps_and_G') in
instantiate(reified_hyps_and_G := hyps_and_G);

(* Close the goal *)
destruct fals.

Ltac get_hyp_names names k_continue_reify_goal :=
intros ?;
lazymatch goal with
  H' : Marker.marker _ _ |- _ => clear H'; k_continue_reify_goal names
| H' : _ |- _ => get_hyp_names constr:((names, H')) k_continue_reify_goal
end.

Ltac reify_goal except3 visible :=
(* the hyps and goal will be saved in this evar *)
let reified_hyps_and_G := fresh "reified_hyps_and_G" in
evar(reified_hyps_and_G : any);

(* False as a hypothesis will be used to close the goal *)
let fals := fresh "fals" in
intro fals;

(* eliminate the -> Prop part of the goal *)
let G := lazymatch goal with |- ?G -> any => G end in
(* switch to G as a goal *)
cut G; only 1: (intros; exact reified_hyps_and_G);

(* revert all hypotheses
   (except the fake ones used to store the goal and close via False h) *)
revert_all_except fals reified_hyps_and_G except3 visible tt;

(* get the names of all the hypotheses, then continue *)
let k := continue_reify_goal fals reified_hyps_and_G except3 visible in
get_hyp_names tt k.


Ltac get_goal_and_hyps except3 visible :=
let G := goal' () in
let reified_goal := constr:(ltac:(reify_goal except3 visible) : False -> G -> any) in
lazymatch reified_goal with
  (let _ := Any ?reified_hyps_and_G in _) => reified_hyps_and_G
end.
