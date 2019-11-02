(**

The option type can be described succinctly as representing something or nothing.
That is, option types are used if a particular value may or may not exist when
it's evaluated. For example, a type representing a persons name might use a
`string option` for the middle name because not everyone has a middle name.

*)
type foobar = int option

(**

Options take a value if they're something, otherwise they're nothing. When options
are decomposed the presence of something is unwrapped to get the value. Options
which are nothing can be dealt with by a default value, failure, skip, or other
construction.

*)
let s : foobar = Some 42
let n : foobar = None

(**

It is typical to unwrap an option using pattern matching.

*)
let matched: int =
  match s with
    Some n -> n
  | None -> 0
