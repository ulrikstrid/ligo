Require Import ltac_sandbox.
Require Import Omega.

Ltac really_assert_fails tac :=
tryif (
  cut False;
  tryif intros _
  then tac
  else idtac
)
then fail "unexpected success"
else idtac. (* got the expected failure *)

Ltac really_assert_no_progress tac :=
tryif (
  cut False;
  tryif intros _
  then progress tac
  else idtac
)
then fail "unexpected progress"
else idtac. (* got the expected failure
               or got the expected lack of progress. *)

Ltac really_assert_not_solved tac :=
tryif (
  cut False;
  tryif intros _
  then solve [tac]
  else idtac
)
then fail "unexpectedly solved"
else idtac. (* got the expected failure or partial progress. *)

Ltac noob_try tac info_tac name :=
  tryif (really_assert_no_progress tac)
  then (tryif (really_assert_fails tac)
        then idtac "couldn't use:      " name
        else idtac "no progress:       " name)
  else ((tryif (really_assert_not_solved tac)
         then idtac "made some progress:" name
         else idtac "\o/ solved:        " name);
        try(really_assert_fails(info_tac);
            fail)).

(* TODO: bug report: placing ltac:() around noob_try's body in
   a Notation gives an error when invoking the tactic. *)

Inductive tactics :=
| _f_equal | _reflexivity | _trivial | _intuition | _omega | _split
| _simpl | _discriminate | _contradiction | _constructor
| _auto___info_auto | _eauto___info_eauto
| _auto_10 | _auto_100 | _auto_1000.

Notation "f_equal."      := _f_equal      (only printing).
Notation "reflexivity."  := _reflexivity  (only printing).
Notation "trivial."      := _trivial      (only printing).
Notation "intuition."    := _intuition    (only printing).
Notation "omega."        := _omega        (only printing).
Notation "split."        := _split        (only printing).
Notation "simpl."        := _simpl        (only printing).
Notation "discriminate." := _discriminate (only printing).
Notation "contradiction." := _contradiction (only printing).
Notation "constructor." := _constructor (only printing).
Notation "info_auto." := _auto___info_auto (only printing).
Notation "info_eauto." := _eauto___info_eauto (only printing).
Notation "(auto 10)." := _auto_10 (only printing).
Notation "(auto 100)." := _auto_100 (only printing).
Notation "(auto 1000)." := _auto_1000 (only printing).

Ltac auto10    := auto 10.
Ltac auto100   := auto 100.
Ltac auto1000  := auto 1000.
Ltac noob' :=
  noob_try f_equal       idtac      _f_equal;
  noob_try reflexivity   idtac      _reflexivity;
  noob_try trivial       idtac      _trivial;
  noob_try intuition     idtac      _intuition;
  noob_try omega         idtac      _omega;
  noob_try split         idtac      _split;
  noob_try simpl         idtac      _simpl;
  noob_try discriminate  idtac      _discriminate;
  noob_try contradiction idtac      _contradiction;
  noob_try constructor   idtac      _constructor;
  noob_try auto          info_auto  _auto___info_auto;
  noob_try eauto         info_eauto _eauto___info_eauto;
  noob_try auto10        idtac      _auto_10;
  noob_try auto100       idtac      _auto_100;
(*  noob_try auto1000      idtac      _auto_1000;*)
  (* TODO: better guesses on progress made. *)
  (*noob_try destruct    idtac      _destruct;*)
  idtac.

Ltac noob1 :=
  noob';
  match goal with
   |- ?G =>
    let _ := sandbox ltac:(fun ret =>
      try (intros ?;
           intros;
           idtac "";
           idtac "Hint: use `intros.', then `noob1.' will say:";
           idtac "";
           noob');
      ret 0
    ) in idtac
  end.
