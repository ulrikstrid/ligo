Require Import ltac_number_goals.

Theorem Truely (X : Set) : X -> True.
intro; exact I. Qed.

Ltac find' expr :=
match expr with
| cons ?hd ?next => find' next
| ?exst => exst
end.

Ltac find lala_result :=
match goal with
| [ LALA_RESULT := ?expr |- _ ] =>
    (* TODO: this doesn't work well with lazymatch *)
    lazymatch LALA_RESULT with lala_result =>
      find' expr
    end
end.

Inductive I_MARKER := MARKER.

Ltac detect_ctor lala_result :=
let lala2 := fresh "lala_result" in
evar (lala2 : list Prop);
set (MARKER);
intros * ctor';
pose (ctor := ctor'); rewrite ctor' in ctor; clear ctor';
revert ctor;
(* re-generalize the parameters *)
repeat(lazymatch goal with
  H : ?t |- _ =>
  match t with
  | I_MARKER => idtac
  | _ => revert H
  end
end);
(* fill the list with the current constructor and extend it
   with one element *)
match goal with
  [ EV := ?EVBODY : _ |- ?G ] =>
  match EV with lala2 =>
  let www := find lala_result in
  let term := constr:(cons G lala2) in
    unify www term
  end
end;
intros; exact I.

Ltac close_list lala_result :=
let www := find lala_result in
let term := constr:(nil : list Prop) in
  unify www term.

Ltac detect_ctors' ty Fals nb_ctors lala_result :=
cut (False -> True);
(only 1: let H := fresh "H" in intro H; clear H; exact I);
intro Fals;

(* do stuff and end temporary goals with destruct Fals: *)
cut ty; (only 2 : destruct Fals);
let H := fresh "H" in 
intro H;
case_eq H; number_the_goals; (with_goal_numbers (fun gi gn =>
  try(instantiate (nb_ctors := gn));
  detect_ctor lala_result
)).

Ltac detect_ctors ty nb_ctors lala_result :=
evar (number_of_constructors : nat);
evar (lala_result : list Prop);
let orig := fresh "orig" in
let Fals := fresh "Fals" in
match goal with
[ |- ?G ] => 
cut (G); only 1:
(intro orig;
 cut True; only 1: (intro Tru; apply orig))
end;
only 1: detect_ctors' ty Fals nb_ctors lala_result;
close_list lala_result.


Inductive foo : Type :=
| Foo : forall (a : bool) (b : bool)
               (c : if a then nat else unit)
               (d : if b then nat else unit),
               foo
| Bar : nat -> foo.

Definition a : nat -> foo.
(* just to have an unrelated goal left at the end. *)
cut (0 = 0); (only 1: (intro tete; clear tete)).
(* just for noize *)
set (blah := False -> foo).


Tactic Notation "with" "temp" ident(H) ":="
                             constr(the_H) "proven" "by"
                             tactic(proof_of_H) "in"
                             tactic(rest) :=
let H := constr:(the_H) in
let PH := constr:(ltac:(proof_of_H) : H) in
rest H PH.     (* in rest we have access to H but we can discard it *)


with temp M := (list Prop)
proven by (detect_ctors foo number_of_constructors list_of_constructors;
  exact list_of_constructors)
in fun H PH =>
let PH := eval compute in PH in
(* Here, PH *)
idtac "PH =" PH;

(*detect_ctors foo number_of_constructors list_of_constructors.*)
exact (fun x => Bar x).

(*clear.*)
reflexivity.
Defined.

Eval compute in a.
Print a.