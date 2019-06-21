Require Import ltac_mutable_state.

Goal True.
init_state(tulu).

cut True; only 1: let Z := fresh in intro Z; clear Z.

set_hole (1 = 1).
set_hole (2 = 1 + 1).
simpl in tulu.
set_hole (3 = 1).
set_hole (4 = 1).
set_hole (5 = 1).
set_hole (6 = 1).
set_hole (7 = 1).
set_hole (8 = 1).
close_state.
exact I.
Import List.ListNotations.
Local Open Scope list.

Fail let st := get_state () in
match st with
(* shouldn't reduce 1+1 into 2 *)
[1 = 1; 2 = 2; 3 = 1; 4 = 1; 5 = 1; 6 = 1; 7 = 1; 8 = 1] =>
idtac
end.

let st := get_state () in
match st with
[1 = 1; 2 = 1 + 1; 3 = 1; 4 = 1; 5 = 1; 6 = 1; 7 = 1; 8 = 1] =>
idtac
end.

exact I.
Qed.

Fail Definition t := tree.

