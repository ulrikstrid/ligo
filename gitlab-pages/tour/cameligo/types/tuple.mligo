(**

A tuple is a simple, unchanging indexed collection of values. It's distinct from
a list because unlike a list tuples are immutable, and lists are restricted to
values of the same type.

For our examples, we'll create a book tuple which has a title string,
author string, and simple integer year of publication.

Do note that while it's easy to confuse the asterisks ('*') in tuple type
definitions with the multiplication operator, seeing an asterisk in an OCaml
type signature indicates a tuple rather than multiplication.

*)
type book = string * string * int

(**

The syntax for creating a tuple is not the same as the syntax for defining its
type. A series of comma separated values with optional parenthesis defines a tuple.

*)
let iliad = ("The Iliad", "Homer", -850)

(**

We can use the syntax `tuple_instance.(position)` to extract the individual values
of a tuple.

*)
let author = iliad.(1)

(**

As we'll see later, it's also possible to use pattern matching to extract the
contents of tuples.

*)
