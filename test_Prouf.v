Require Import Prouf.

Inductive mylist elem_type :=
  mynil
| mycons (hd : elem_type) (tl : mylist elem_type).
Definition t := forall (namea : nat), 1 = 1.
Definition t' := forall (nameb : nat), nameb = 1.
Definition t'' := nat -> 1 = 1.

(* Show argument names in function types *)
Eval print in verbose mycons.
Eval print in verbose t.
Eval print in verbose t'.
Eval print in verbose t''.
(* Eval print in verbose mylist. *) (* TODO: show the constructors *)

(* Get advice, debug and brute-force *)
Theorem th1 : forall x y, x = 2 -> y = 5 -> x + y = 3+4.
noob1.              (* gives suggestions, noob2 comming soon! *)
intros ?;
  dbg;              (* shows the goal at this point in the tactic pipeline *)
  intros ?;
  dbg some_message; (* pass an ID or number to disambiguate *)
  intros ?.
obviously1.         (* brute-force, obviously2 comming soon! *)
Qed.

(* See in what context _ or ltac:(…) infer things: *)
Definition v := let a := bool in
                let x := 42 in
                @cons <?> x (@nil <?aaa?>).

(* Inspect in a tactic the number of goals produced by another tactic. *)
Theorem th2 : mylist nat -> True.
intro H.
induction H;
  number_the_goals; (* add hypotheses: current and total number of subgoals *)
  with_goal_numbers (fun gi gn =>
    idtac "Goal" gi (* Number of the goal under focus *)
          "of" gn;  (* Total number of goals produced by induction H. *)
    dbg;
    exact I         (* solves a goal of type True *)
  ).
Qed.
