Require Import ltac_sandbox.

Tactic Notation "check" "unfold" constr(term) "is" constr(expected) :=
cut (term = term);
[> (let H := fresh "H" in intro H; clear H)
 | unfold term at 1;
   match goal with
     |- expected = term =>
     idtac term "unfolded to the expected term"
   | |- ?other          =>
     idtac term "unfolded to" other "but expected" expected;
     fail
   end;
   reflexivity
].

Theorem th_sandbox : forall x, x = 0+0 -> 1+1 = 1 -> True.
intros x H.
let sb := sandbox ltac:(fun ret =>
  match goal with
  |- ?G =>
    match G with
      (1 + 1 = 1 -> True) => idtac "found the expected goal inside sandbox"
    | _                   => idtac "wrong goal inside sandbox";
                             fail 9999999
    end
  end;
  ret (true = false -> false = false)
) in
let expected := constr:(true = false -> false = false) in
match sb with
  expected =>
  idtac "sandbox returned the expected value"
| ?actual =>
  idtac "expected sandbox to return";
  idtac expected;
  idtac "but it returned";
  idtac actual;
  fail 9999999
end.
intros; exact I.
Defined.

Goal True.
check unfold th_sandbox is
  (fun (x : nat) (_ : x = 0 + 0) (_ : 1 + 1 = 1) => I).
exact I.
Qed.
