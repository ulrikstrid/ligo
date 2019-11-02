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
let or_true: bool = true || false

let or_false: bool = false || false

(**

`&&` represents the boolean AND. The AND is true if both of its operands are true.
Otherwise it's false.

true || true = true
true || false = false
false || true = false
false || false = false

*)
let and_true: bool = true && true

let and_false: bool = true && false

(**

`not` is the boolean NOT. The NOT is true if its operand is false and false if
its operand is true. NOT is also known by the name negation.

not true = false
not false = true

*)
let not_bool: bool = not true
