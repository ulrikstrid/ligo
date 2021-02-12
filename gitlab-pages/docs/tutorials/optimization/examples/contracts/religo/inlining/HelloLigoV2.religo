/*  Let's inline `h_plus_one` and see if the contract
   gets smaller and consumes less gas */

type a_complex_record = {
  h : int,
  e : int,
  l : int,
  l_: int,
  o : int,
  ligo : int
}

[@inline]
let h_plus_one =
    (r : a_complex_record) => { ...r, h: r.h + 1 }

let main = ((p, s) : (int, a_complex_record)) =>
  ([] : list(operation), h_plus_one(h_plus_one(s)));
