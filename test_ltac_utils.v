Require Import ltac_utils.

Goal True -> 43 = 43 -> true <> false -> 42 = 42.
intros H ?;
(*
   H : True
   H0 : (43 = 43)
   ------
   (true <> false -> 42 = 42)
*)
dbg;
(*
   H : True
   H0 : (43 = 43)
   ------ some_unbound_name
   (true <> false -> 42 = 42)
*)
dbg some_unbound_name;
(*
   H : True
   H0 : (43 = 43)
   ------ H
   (true <> false -> 42 = 42)
*)
dbg H;
(*
   H : True
   H0 : (43 = 43)
   ------ 123
   (true <> false -> 42 = 42)
*)
dbg 123;
repeat match goal with H' : _ |- _ => revert H' end;
reflexivity.
Qed.

(*
   ------
   Type

   ------ (aaa)
   Type
 *)
Definition v := @cons <?> 42 (@nil <?aaa?>).

(* Shouldn't display anything related to dbg. *)
Definition v' := @cons _ 42 (@nil _).

(*
   ------
   Type

   ------ (bbb)
   Type
 *)
Program Definition w := @cons <?> 42 (@nil <?bbb?>).


(*
   ------
   Type

   Obligation 1 of w': (Type).
 *)
Program Definition w' := @nil <??>.
(*
   ------
   Type
*)
Next Obligation.
exact nat.
Defined.

(*
   ------ (ccc)
   Type

   Obligation 1 of w'': (Type).
 *)
Program Definition w'' := @nil <?? ccc ??>.
(*
   ------ (ccc)
   Type
*)
Next Obligation.
exact nat.
Defined.

(* Shouldn't display anything related to dbg. *)
Program Definition w''' := @nil _.
Next Obligation.
exact nat.
Defined.
