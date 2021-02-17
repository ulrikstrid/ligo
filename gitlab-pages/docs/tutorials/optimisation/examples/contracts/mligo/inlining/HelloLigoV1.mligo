(* Can we benefit from removing the function definition from
   the compiled Michelson code? First, let's look at the
   non-inlined version of a sample contract *)

type a_complex_record =
  {h : int; e : int; l : int; l_ : int; o : int; ligo : int}

(* A function that doesn't perform a lot of computations
   but has a complex type *)
let h_plus_one (r : a_complex_record) = {r with h = r.h + 1}

let main (p, s : int * a_complex_record) =
  ([] : operation list), h_plus_one (h_plus_one s)
