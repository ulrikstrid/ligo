(**

In the LIGO type system, functions have a signature of the form 
`(param_type -> ret_type)`. For example, the signature `(int -> bool * int)` 
is a function that takes an integer and returns a pair-tuple with a boolean on
the left and integer on the right.

During an actual definition the syntax is a little different, as you can see in
the function below.
*)
let example (n: int) : bool * int = (true, n)

