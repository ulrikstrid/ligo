(*



 *)

(**

String.size returns the size of a given string. The string "hello" would yield
a value of 5.

*)
let size_op (s : string) : nat =
  String.size s

(**

String.slice takes two natural numbers representing positions and a string. It then
produces the substring starting at the first position and ending at the second
position. This means that `String.slice 1p 2p "tata"` produces the string "at".
The positions are zero indexed, so that 1 would be the second character 'a' and
2 would be the third character 't'.

*)
let slice_op (s : string) : string =
  String.slice 1p 2p s

(**

String concatenation can be done with the syntax `s ^ s'` where both operands
are strings. `"foo" ^ "bar"` produces `"foobar"`.

*)
let concat_syntax (s: string) =
  s ^ "test_literal"
