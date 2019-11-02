(**

As you would expect from a functional language, LIGO uses immutable strings.
New strings can be created from old strings, but strings cannot be modified in-place.

Strings are denoted by the type `string`.

*)

let foo: string = "foo"

let bar: string = "bar"

let foobar: string = "foobar"

(**

String.size returns the size of a given string. The string "foobar" would yield
a value of 6.

*)
let size_op: int = String.size foobar

(**

String.slice takes two natural numbers representing positions and a string. It then
produces the substring starting at the first position and ending at the second
position. This means that `String.slice 1p 2p "foobar"` produces the string "oo".
The positions are zero indexed, so that 1 would be the second character 'o' and
2 would be the third character 'o'.

*)
let slice_op: string = String.slice 1p 2p foobar

(**

String concatenation can be done with the syntax `s ^ s'` where both operands
are strings. `"foo" ^ "bar"` produces `"foobar"`.

*)
let concat: string = foo ^ bar
