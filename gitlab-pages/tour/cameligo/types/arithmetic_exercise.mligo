(**

Lets try writing some arithmetic expressions of our own. The following values
are checked by the `main` routine at the bottom of the file for equality to 57,
but the lazy programmer forgot to define them as more than placeholders! Replace
the righthand expressions on each side with an appropriate arithmetic expression
to complete the exercise.

*)

(**

This first value works just fine, it's here as an example of how the others
should be written. `add` would be a reasonable response to the direction:

"Bind a value which adds 10 to `start`."

*)

let start: int = 5

let add: int = start + 10

(**

Bind a value which subtracts `add` by 9.

*)
let sub: int = 0

(**

Bind a value which multiplies `sub` by 4.

*)
let mul: int = 0

(**

Bind a value which divides `mul` by 3.

*)
let div: int = 0

(**

This binding already works, it's here to demonstrate order of operations.
An expression wrapped in parenthesis is evaluated before others, as in the PEMDAS
ordering in traditional mathematics. (For the unfamiliar: parenthesis, exponents,
multiplication, division, addition, subtraction.)

`add_div` would be a reasonable response to the direction:

"Bind a value which adds two to `div` and then divides it by two."

*)
let add_div: int = (div + 2) / 2

(**

Bind a value which subtracts `add_div` by two, then multiplies by nine,
then adds three.

*)
let sub_mul_add: int = 0

(**

Bind a value which divides `sub_mul_add` by two, then adds five, then subtracts one,
then multiplies it by three.

*)
let div_add_sub_mul: int = 0

(**

Once you've filled in the values above, run the program with the `main` entrypoint
and a parameter of `unit`. If it returns `true` you've finished the exercise.

*)
let main () : bool = div_add_sub_mul = 57
