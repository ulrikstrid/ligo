(**

The boolean type represents logical truth, that is to say a value of true or false.

*)

(**

(**

`||` represents the boolean OR. The OR is true if one of its operands is true, 
otherwise it's false.

true || true = true
true || false = true
false || true = true
false || false = false

*)
let or_true (b : bool) : bool =
   b || true

let or_false (b : bool) : bool =
   b || false

(**

`&&` represents the boolean AND. The AND is true if both of its operands are true.
Otherwise it's false.

true || true = true
true || false = false
false || true = false
false || false = false

*)
let and_true (b : bool) : bool =
   b && true

let and_false (b : bool) : bool =
   b && false

(**

`not` is the boolean NOT. The NOT is true if its operand is false and false if
its operand is true. NOT is also known by the name negation.

not true = false
not false = true

*)
let not_bool (b: bool) : bool =
   not b
