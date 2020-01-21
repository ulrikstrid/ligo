let negate (i: int) : int =
  (michelson_1 "{ NEG }" : int -> int)

let main (p: int) : int =
  negate p
