Require Import ZArith .

Inductive literal :=
| L_nat : N -> literal
| L_bool : Datatypes.bool -> literal
.

Inductive program :=
| P_literal : literal -> program
| P_add : program -> program -> program
.

Inductive program_type :=
| T_nat
| T_bool
.
         
Inductive typed_program :=
| TP_literal : literal -> typed_program
| TP_add : typed_program -> typed_program -> typed_program
.

Inductive result A :=
| Ok : A -> result A
| Error : result A
.

Arguments Ok {A}.

Definition bind {A} f (x : result A) :=
  match x with
  | Ok x => f x
  | Error _  => Error A
  end .

Notation "x >>? f" := (bind f x) (right associativity, at level 100) .
  
Definition typecheck_literal l :=
  match l with
  | L_bool _ => T_bool
  | L_nat _ => T_nat
  end
.

Fixpoint typecheck (tp : typed_program) : result program_type :=
  match tp with
  | TP_literal l => Ok (typecheck_literal l)
  | TP_add n m => (
      typecheck n >>? fun t_n =>
      typecheck m >>? fun t_m =>
      match (t_n , t_m) with
      | (T_nat , T_nat) => Ok T_nat
      | _ => Error _
      end
    )
  end
.

Definition typed x := exists t , typecheck x = Ok t .

Theorem typecheck_children :
  forall n m t ,
    typecheck (TP_add n m) = Ok t ->
    typed n /\ typed m .
Proof .
  intros n m t TC . simpl in TC .

  case_eq (typecheck n) .
  2: { intro r ; rewrite r in TC ; exfalso ; inversion TC . }
  intros p_n r_tn ; rewrite r_tn in TC ; simpl in TC .

  case_eq (typecheck m) .
  2: { intro r . rewrite r in TC . exfalso . inversion TC . }
  intros p_m r_tm ; rewrite r_tm in TC ; simpl in TC .

  split .
  - now exists p_n .
  - now exists p_m .
Qed .

Theorem typecheck_add :
  forall n m t ,
    typecheck (TP_add n m) = Ok t ->
    (typecheck n = Ok T_nat) /\ (typecheck m = Ok T_nat) .
Proof .
  intros n m t TC . simpl in TC .

  case_eq (typecheck n) .
  2: { intro r ; rewrite r in TC ; exfalso ; inversion TC . }
  intros p_n r_tn ; rewrite r_tn in TC ; simpl in TC .

  case_eq (typecheck m) .
  2: { intro r . rewrite r in TC . exfalso . inversion TC . }
  intros p_m r_tm ; rewrite r_tm in TC ; simpl in TC .

  destruct p_n .
  2: { inversion TC . }
  destruct p_m .
  2: { inversion TC . }
  
  auto .
Qed .


Fixpoint interpret (tp : typed_program) : result literal :=
  match tp with
  | TP_literal l => Ok l
  | TP_add n m => (
      interpret n >>? fun n =>
      interpret m >>? fun m =>
      match (n , m) with
      | (L_nat n , L_nat m) => Ok (L_nat (n + m))
      | _ => Error _
      end
    )
  end
.

(* clear useless hypotheses *)
Ltac autoclear :=
  repeat match goal with
    H : ?a = ?a |- _ => clear H
  end.

Ltac introrewrite a :=
  intros a ; try rewrite a in *.

Tactic Notation "if" "intros" "is" "possible" "then" tactic2(zen) "else" tactic2(els) :=
  (* can also be implemented with try + tryif *)
  lazymatch goal with
    |- (forall var : _, _) => zen
  | |- (let var := _ in _) => zen
  | _ => els
  end.

Ltac introsrewrite_eqH a :=
  introrewrite a;
  if intros is possible
  then idtac
  else (simpl in *; idtac "clear" a; autoclear);
  repeat (
    let b := fresh "b" in introrewrite b;
    if intros is possible
    then idtac
    else (simpl in *; idtac "clear" b; autoclear)
  ).

Ltac inveval EVAL :=
  let f := fresh "f" in
  intros * f ; rewrite f in * ; exfalso ; inversion EVAL .

Ltac assert_ok e a :=
  case_eq e ; only 2: (
    let eqH := fresh "eqH" in
    intros * eqH ;
    rewrite eqH in * ;
    match goal with
      H : _ |- _ =>
      solve[inversion H]
    end
  );
  introsrewrite_eqH a.

Ltac autoassertok :=
match goal with
  H : context toto [(?a >>? ?b) = ?c] |- _ =>
    let lala := fresh "lala" in
    idtac "assert_ok on" a;
    assert_ok a lala
end.

Ltac autoassert :=
match goal with
  H : context toto [?a] |- _ =>
    case_eq a ;
    let nga := numgoals in
    guard nga > 1;
    (* try to solve each goal *)
    try (
      let eqH := fresh "eqH" in
      intros * eqH ;
      rewrite eqH in * ;
      match goal with
        H : _ |- _ =>
        solve[inversion H]
       end
    );
    let ngb := numgoals in
    guard ngb = 1;
    let z := fresh "z" in
    introsrewrite_eqH z;
    idtac "assert_ok-ish on" a
end.

Theorem subred_typecheck :
  forall (tp : typed_program) ,
  forall (t : program_type) ,
  forall (l : literal) , 
    typecheck tp = Ok t ->
    interpret tp = Ok l ->
    typecheck_literal l = t.
Proof .
  intro tp . induction tp .
  - intros t l' TC EVAL .
    simpl in * . inversion TC . inversion EVAL . reflexivity .
  - intros t l' TC EVAL .
    simpl in TC .
    assert_ok (typecheck tp1) t1.
    assert_ok (typecheck tp2) t2.
    assert_ok (interpret tp1) l1.
    assert_ok (interpret tp2) l2.

(* TODO: prevent autoassert from reusing the same fragment all the time *)
do 10 autoassert. (* this is useless *)


    assert_ok t1              tr1.
    assert_ok t2              tr2.
    assert_ok l1              n1.
    assert_ok l2              n2.
    assert_ok t               lala.

    now inversion EVAL.
Qed .

Theorem correct_typecheck :
  forall (tp : typed_program) ,
  forall (t : program_type) ,
    typecheck tp = Ok t ->
    (exists l , interpret tp = Ok l) .
Proof .
  intro tp .
  induction tp .
  exists l ; auto .
  intros t TC .
  simpl in TC .
  case_eq (typecheck tp1) .
  Focus 2 . intros exf ; rewrite exf in TC ; exfalso ; inversion TC .
  intros t1 r_t1 ; rewrite r_t1 in TC ; simpl in TC .
  case_eq (typecheck tp2) .
  Focus 2 . intros exf ; rewrite exf in TC ; exfalso ; inversion TC .
  intros t2 r_t2 ; rewrite r_t2 in TC ; simpl in TC .
  elim (IHtp1 t1) .
  Focus 2 . assumption .
  intros l1 r_l1 .
  elim (IHtp2 t2) .
  Focus 2 . assumption .
  intros l2 r_l2 .
  simpl .
  rewrite r_l1 ; simpl .
  rewrite r_l2 ; simpl .
  case_eq t1 .
  Focus 2 . intros exf ; rewrite exf in TC ; exfalso ; inversion TC .
  intro r_t1_nat ; rewrite r_t1_nat in TC ; simpl .
  case_eq t2 .
  Focus 2 . intros exf ; rewrite exf in TC ; exfalso ; inversion TC .
  intro r_t2_nat ; rewrite r_t2_nat in TC ; simpl .
  assert (typecheck_literal l1 = T_nat) as TC1 .
  rewrite <- r_t1_nat ; now apply (subred_typecheck tp1 t1 l1) .
  assert (typecheck_literal l2 = T_nat) as TC2 .
  rewrite <- r_t2_nat ; now apply (subred_typecheck tp2 t2 l2) .
  case_eq l1 .
  Focus 2 . intros b exf ; rewrite exf in TC1 ; exfalso ; inversion TC1 .
  intros n1 r_l1_nat .
  case_eq l2 .
  Focus 2 . intros b exf ; rewrite exf in TC2 ; exfalso ; inversion TC2 .
  intros n2 r_l2_nat .
  now exists (L_nat (n1 + n2)) .
Qed .

Require Import Michocoq.syntax.
Require Import Michocoq.untyped_syntax .
Require Import Michocoq.error .

Fixpoint transpile_type (t : program_type) : type :=
  match t with
  | T_nat => Comparable_type nat
  | T_bool => Comparable_type bool
  end
.

Fixpoint transpile (tp : typed_program) :=
  match (tp) with
  | (TP_add n m) =>
    transpile n >>? fun n =>
    transpile m >>? fun m =>
    Ok (SEQ (SEQ n m) ADD)
  | (TP_literal (L_nat n)) => Ok (PUSH (Comparable_type nat) (Nat_constant n))
  | (TP_literal (L_bool true)) => Ok (PUSH bool True_)
  | (TP_literal (L_bool false)) => Ok (PUSH bool False_)
  end
.


