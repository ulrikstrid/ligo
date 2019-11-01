(**

Lets try writing some arithmetic expressions of our own. The following functions
are used by the `main` routine at the bottom of the file to compute a number,
but the lazy programmer forgot to define them as more than placeholders! Replace
the righthand expressions on each side with an appropriate arithmetic expression
to complete the exercise.

*)

(**

This first function works just fine, it's here as an example of how the others
should be written. `add` would be a reasonable response to the direction:

"Write a function which adds 10 to the input."

*)
let add (n: int) : int = n + 10

(**

Write a function which subtracts the input by 9.

*)
let sub (n: int) : int = n

(**

Write a function which multiplies the input by 4.

*)
let mul (n: int) : int = n

(**

Write a function which divides the input by 3.

*)
let div (n: int) : int = n

(**

This function already works, it's here to demonstrate order of operations.
An expression wrapped in parenthesis is evaluated before others, as in the PEMDAS
ordering in traditional mathematics. (For the unfamiliar: parenthesis, exponents,
multiplication, division, addition, subtraction.)

`add_div` would be a reasonable response to the direction:

"Write a function which adds two to the input and then divides it by two."

*)
let add_div (n: int) : int = (n + 2) / 2

(**

Write a function which subtracts the input by two, then multiplies by nine,
then adds three.

*)
let sub_mul_add (n: int) : int = n

(**

Write a function which divides the input by two, then adds five, then subtracts one,
then multiplies it by three.

*)
let div_add_sub_mul (n:int) : int = n

(**

Once you've filled in the functions above, run the program with the `main` entrypoint
and a parameter of `unit`.

*)
let main () : bool =
  (div_add_sub_mul (sub_mul_add (add_div (div (mul (sub (add 5))))))) = 57
