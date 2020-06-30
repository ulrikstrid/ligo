let cmp2 f1 f2 (a1, a2) (b1, b2) =
  match f1 a1 b1 with
  | 0 -> f2 a2 b2
  | c -> c

let cmp3 f1 f2 f3 (a1, a2, a3) (b1, b2, b3) =
  match f1 a1 b1 with
  | 0 ->
    (match f2 a2 b2 with
     | 0 -> f3 a3 b3
     | c -> c)
  | c -> c
