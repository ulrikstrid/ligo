(**

The match construct is one of the less familiar features of OCaml to programmers
coming from a background of Python, Java, etc. Match expressions are used to
decompose types with multiple pieces into parts based on *patterns*.

The idea behind patterns is that they're shaped to be the opposite of a constructor.
We're already familiar with constructors such as that for the tuple:

*)
let t: int * int * int = (5, 10, 15)

(**

A match pattern does the opposite operation, taking a constructed tuple and
pulling it back out into its component values.

*)
let match_t (t: int * int * int) : int =
  match t with
    (n1, n2, 15) -> n1 + n2 + 15
  | (_, _, _) -> 0

(**

This match expression already demonstrates multiple important properties of match
patterns:

- Match patterns can represent the contents they match as variables, which are then
used to evaluate a result expression.
- Match patterns can mix literals and variables.
- Match patterns can skip certain elements of the structure they decompose, declining
to assign them a variable.

*)

(**

We can match other constructors too, such as the boolean.

*)
let match_bool (b: bool) : int =
  match b with
    true -> 10
  | false -> 0

(**

We can also match variant types.

*)

type param =
  Add of int
| Sub of int

let main (p: param) =
  let out =
      (match p with
         Add n -> n
       | Sub n -> 0-n)
  in out

(**

Match statements are also how option types are dealt with at use.

*)
let match_option (i : int option) : int =
  match i with
    Some n -> n
  | None -> 0

(**

The list concatenation is a common match pattern. In `hd :: tl` you get the
head of the list (its first element) and then the rest (its tail). It's
common to use the hail, tail, nil match pattern shown below in recursive
functions to process lists.

*)
let match_list (l: int list) : int =
  match l with
    hd :: tl -> hd
  | [] -> 10


