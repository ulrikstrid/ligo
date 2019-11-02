(**

The record type is essentially like a struct in C, named tuple in Python, or an
object with attributes and no methods. It has a set of named elements which can
be of different types, sort of like a tuple. But unlike the tuple there is no
defined ordering and elements are accessed by name instead of position.

*)
type foobar = {
 foo : int ;
 bar : int ;
}

let fb : foobar = {
  foo = 0 ;
  bar = 0 ;
}

(**

The following actually demonstrates an important point. Type names are not variables, and
do not exist in the variable namespace. If you try to use a type name as an
expression, the compiler might tell you it doesn't exist. It could take you a
while to figure out the problem.

*)

type abc = {
  a : int ;
  b : int ;
  c : int
}

let abc : abc = {
  a = 42 ;
  b = 142 ;
  c = 242
}

let a : int = abc.a
let b : int = abc.b
let c : int = abc.c

let projection (r : foobar) : int = r.foo + r.bar

(**

It is possible to modify a record by creating a new instance that copies over the
elements of the original.

*)
let modify (r : foobar) : foobar = {foo = 256; bar = r.bar}

let modify_abc (r : abc) : abc = {a = r.a; b = 2048; c = r.c}
