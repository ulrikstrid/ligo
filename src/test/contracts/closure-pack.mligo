(* example involving several closure variables *)

let main (p, s : unit * (int -> int) list) : operation list * (int -> int) list =
  let x0 = 1 in
  let x1 = x0+x0+1 in
  let x2 = x1+x1+1 in
  let x3 = x2+x2+1 in
  let x4 = x3+x3+1 in
  let x5 = x4+x4+1 in
  let x6 = x5+x5+1 in
  let f0 = fun (y : int) -> y in
  let f1 = fun (y : int) -> x2 + y in
  let f2 = fun (y : int) -> x2 + x4 + y in
  let f3 = fun (y : int) -> x2 + x4 + x6 + y in
  (([] : operation list), [f0; f1; f2; f3])
