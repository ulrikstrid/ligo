Load ltac_print_verbose.

(* Example use: *)
Eval print in forall x:nat, 42 = 42.
Eval print in verbose forall x:nat, 42 = 42.
