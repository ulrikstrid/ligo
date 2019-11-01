(**

This series of annotated files will show you how to use CameLIGO. CameLIGO is a
syntax based on the OCaml programming language. Excepting some corner cases, it is
mostly compatible with the OCaml syntax and should be familiar to an OCaml user.

This file demonstrates how to use the arithmetic operators in CameLIGO.

 *)

(**

The 'let' statement allows you to bind a variable to the value of the expression
on the righthand side of the equal sign. Importantly, these bindings are not
mutable state. The only way to change the value once bound is to shadow with a
new value.

*)
let plus_op: int  = 10 + 42

let minus_op: int = 82 - 42

let times_op: int = 2 * 42

let div_op: int = 42 / 2

(**

Notice that the name of the variable is followed by `: int`, this is the variables
*type signature*. In a language which emphasizes its type system, signatures
can become quite complex. In ordinary OCaml a type inference system is used to
figure out the type of many variables without the user providing a signature.
LIGO doesn't do this yet, so for now all CameLIGO code needs to provide
explicit type signatures where in OCaml they might be optional. When type
inference is supported later, code written with explicit types will still run
without issues.

(**

`mod` is the modulus or remainder operator. It takes two numbers X, Y, and
reports the remainder of evenly dividing X into Y pieces. So `10 mod 2` is
zero, and `11 mod 2` is one.

*)
let mod_op  = 10 mod 2

let neg_op  = -(10)

(**

Of course, a Tezos contract requires `storage` and `parameter`. For now we'll
focus on LIGO the language. Details on how to write complete contracts can be
found in the [contracts section](tour/contracts/toc.md) of this series.

*)
