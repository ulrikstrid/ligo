(* original file under license CC0, can be licensed as MIT, BSD, LGPL, GPL etc. *)

Require Import ltac_number_goals.

Goal True.
Require Import String.
Local Open Scope string.
let th := nth 3 ("a","b","c","d","e","f","g","h","i") in
  lazymatch th with
     "c" => idtac "test pass: th is ""c"""
   | _ => fail 9999999 "test fail: th is not ""c"""
  end.
exact I.
Qed.

(* Example 1: call get_goal_id and get_number_of_goals *)
Goal (Type * nat * bool).
cut True; (only 1: intro; clear).
refine(_,_,_);
number_the_goals;

let gi := get_goal_id in
let gn := get_number_of_goals in
  clear_goal_numbers;
  idtac gi "/" gn;
  match gi with
  | 1 => refine(nat)
  | 2 => refine(1)
  | 3 => refine(true)
  end.

exact I.
Defined.

(* Example 2: supply callback to with_goal_numbers *)
Goal (Type * nat * bool).
cut True; (only 1: intro; clear).
refine(_,_,_);
number_the_goals;
with_goal_numbers (fun gi gn => match gi with
  | 1 => refine(nat)
  | 2 => refine(1)
  | 3 => refine(true)
  end).

exact I.
Defined.

(* Example 2: take value from a list *)
Goal (Type * nat * bool).
cut True; (only 1: intro; clear).
refine(_,_,_);
number_the_goals;
with_goal_numbers (fun gi gn =>
  let answer := nth gi (nat, 1, true) in
  refine answer).

exact I.
Defined.
