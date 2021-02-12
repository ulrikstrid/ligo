(* Can we benefit from removing the function definition from
   the compiled Michelson code? First, let's look at the
   non-inlined version of a sample contract *)

type a_complex_record is
  record [
    h : int;
    e : int;
    l : int;
    l_: int;
    o : int;
    ligo : int;
  ]

(* A function that doesn't perform a lot of computations
   but has a complex type *)
function h_plus_one (const r : a_complex_record) is
    r with record [ h = r.h + 1 ]

function main (const p : int; const s : a_complex_record) is
    ((list [] : list(operation)), h_plus_one(h_plus_one(s)))
