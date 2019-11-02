(**

Sometimes a value can be one of multiple types. This type of value is called a
*variant*. A variant defines a list of type signatures which a value belonging to
the variant can have.

*)
type foobar =
| Foo of int
| Bar of bool
| Kee of nat

let foo : foobar = Foo 42

let bar : foobar = Bar true

let kee : foobar = Kee 23p

(**

We can work with the specific type that a variant happens to be using pattern
matching.

*)
let matched: int =
  match foo with
    Foo i -> i
  | Bar b -> if b then 50 else 25
  | Kee n -> int n
