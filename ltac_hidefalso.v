Require Import BinNat.

(* if a tactic is clever enough to apply the correct arguments to tf,
   make the challenge harder *)
Definition False' :=
  forall (n : N) (m : N),
    if BinNatDef.N.eqb (BinNatDef.N.mul n m) 19326223710861634601%N then
      False
    else
      True.
Opaque False'.

Theorem extract_False : False' -> False.
intro f.
set (F := f 5915587277%N 3267000013%N).
compute in F.
destruct F.
Qed.
