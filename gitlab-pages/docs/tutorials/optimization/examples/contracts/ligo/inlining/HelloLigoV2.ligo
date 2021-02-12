(* Let's inline `h_plus_one` and see if the contract
   gets smaller and consumes less gas *)

type a_complex_record is
  record [
    h : int;
    e : int;
    l : int;
    l_: int;
    o : int;
    ligo : int;
  ]

[@inline]
function h_plus_one (const r : a_complex_record) is
    r with record [ h = r.h + 1 ]

function main (const p : int; const s : a_complex_record) is
    ((list [] : list(operation)), h_plus_one(h_plus_one(s)))
