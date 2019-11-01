(**

This series of annotated files will show you how to use CameLIGO. CameLIGO is a
syntax based on the OCaml programming language. Excepting some corner cases, it is
mostly compatible with the OCaml syntax and should be familiar to an OCaml user.

 *)

(**

For this first file, we're going to go through the basic arithmetic operators
and demonstrate the editor features you'll use through the rest of this series.

*)

(**

The function `plus_op` takes an integer n and adds 42 to it. At various points in
these tutorials you'll be asked to use functions as an *entrypoint*, or place
where the program begins execution. You can do that with `plus_op` now by typing
`plus_op` into the 'entrypoint' box at the top of the editor. `plus_op` also
requires a parameter, which can be given below the entrypoint box. Try running
`plus_op` with `1` and make sure you get 43 back.

*)
let plus_op (n : int) : int =
  n + 42

(**

These other functions show the different arithmetic operators in CameLIGO.

*)

let minus_op (n : int) : int =
  n - 42

let times_op (n : int) : int =
  n * 42

let div_op (n : int) : int =
  n / 2

(**

`mod` is the modulus or remainder operator. It takes two numbers X, Y, and
reports the remainder of evenly dividing X into Y pieces. So `10 mod 2` is
zero, and `11 mod 2` is one.

*)
let mod_op (n : int) : nat =
  n mod 2

let neg_op (n : int) : int =
  -n

(**

Of course, a Tezos contract requires `storage` and `parameter`. For now we'll
focus on LIGO the language. Details on how to write complete contracts can be
found in the [contracts section](tour/contracts/toc.md) of this series.

*)
