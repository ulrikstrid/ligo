Require Import ltac_hidefalso.

Goal False' -> False.
intro fals'.
apply (extract_False fals').
Qed.
