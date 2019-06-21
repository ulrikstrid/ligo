(* original file under license CC0, can be licensed as MIT, BSD, LGPL, GPL etc. *)

Ltac xloop' :=
set (v := 1 + 1);
(only 1: (set (w := 42); simpl in v));
cycle 1.

Inductive goalid := GOALID : nat -> goalid.

Inductive numberOfOoals := NUMBER_OF_GOALS : nat -> numberOfOoals.

Ltac copy_numgoals_to_goalid :=
lazymatch goal with
| [ H := NUMBER_OF_GOALS ?n |- ?G ] =>
  (* idtac "Goal" n "is" G; *)
  set (goalid := GOALID n)
end.

Ltac incrnumberOfOoals :=
lazymatch goal with
| [ H := NUMBER_OF_GOALS ?n |- ?G ] =>
  clear H;
  let n' := eval simpl in (n+1) in
  set (H := NUMBER_OF_GOALS n')
end.

Ltac number_the_goals :=
set (numberOfOoals := NUMBER_OF_GOALS 0);
let ng := numgoals in
do ng (
  incrnumberOfOoals;
  [> copy_numgoals_to_goalid | idtac ..];
  cycle 1
).

Ltac get_goal_id := match goal with
  [ H := GOALID ?i |- _] => i
end.

Ltac get_number_of_goals := match goal with
  [ H := NUMBER_OF_GOALS ?n |- _] => n
end.

Ltac clear_goal_numbers := match goal with
  [ H := GOALID _, H' := NUMBER_OF_GOALS _ |- _] => clear H; clear H'
end.

Ltac with_goal_numbers' tac :=
(* As soon as a tactic manipulates a term, it gets focussed
   on a single goal.
   Since tac is likely be a closure on some terms, we don't include
   number_the_goals here, otherwise it would only have one goal
   under focus and wouldn't be able to do its job. *)
let gi := get_goal_id in
let gn := get_number_of_goals in
  clear_goal_numbers;
  tac gi gn.
(* We need this to ensure that anonymous (fun gi ni => …) tactics
   are not interpreted as fun terms *)
Tactic Notation "with_goal_numbers" tactic(tac) := with_goal_numbers' tac.

Ltac mk_term term := term.

Ltac reverse_to_propper' l acc :=
lazymatch l with
| (?rest, ?x) => reverse_to_propper' rest (acc, x)
| ?x => mk_term (acc, x)
end.

Ltac reverse_to_propper l := reverse_to_propper' l unit.

Ltac nth' n l :=
let n' := eval simpl in n in
lazymatch n' with
| 0  => fail (* n should be a strictly positive integer *)
| 1  => lazymatch l with (?rest, ?x) => x end
| ?i => lazymatch l with (?rest, ?x) => nth' (i-1) rest end
end.

Ltac nth n l := let l' := reverse_to_propper l in nth' n l'.
