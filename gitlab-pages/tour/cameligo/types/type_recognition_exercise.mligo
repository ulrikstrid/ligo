(**

This file is a spot check to make sure you understand LIGO's types. It presents a
series of type signatures with obfuscated names. From the righthand side of the
type definition you want to infer what the type is in plain english. At the bottom
you can find the solutions to check your answers. Good luck.

*)

type a = int

type b = nat

type c = string

type d = bool

type e = bool option

type f =
  | Add of int
  | Sub of int

type g = (bool * int)

type h = (int -> mutez)

type i = (int * nat) option

type j = int * nat option

type k = (int * int -> f)

type l = int list

type m = ((int option) list -> nat)

type n = (int list -> int set)

type o = (string -> int) list

type p = (int, int) map

type q = ((int , int) map -> int set)

type r = (string, (string set -> bool)) map

type s = ((string -> int) list) * (string list)

type t = (string, int) map option list

type u

type v

type w

type x

type y

type z

(**

Here are the solutions. So you don't accidentally spoil yourself they've been 
ROT13'd. To unscramble them paste the block into a ROT13 decoder.

*)

1.

2.

3.

4.

5.

6.

7.

8.

9.

10.

11.

12.

13.

14.

15.

16.

17.

18.

19.

20.

