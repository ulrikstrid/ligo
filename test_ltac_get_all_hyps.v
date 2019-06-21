Require Import ltac_get_all_hyps.

Goal forall (tt := 42) a b (y := b = b) c (va : a), b -> c -> True.
intros ???????.
let hyps := get_goal_and_hyps () true in
(* TODO: The order is wrong… *)
let expected_hyps := constr:(
    forall (tt := 42) a b (y := b = b) c (va : a) (X : b),
      Goal (Datatypes.tt, tt, a, b, y, c, va, X) (c -> True)
  ) in
lazymatch hyps with
| expected_hyps => idtac "test pass."
| _ => idtac "expected";
       idtac expected_hyps;
       idtac "but got";
       idtac hyps;
       fail
end.
intros ?.
exact I.
Qed.
