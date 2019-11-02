(**

CameLIGO (and OCaml by extension) uses a fairly basic if conditional.

It can be a simple `if <bool_clause> then <expression>`, as below.

*)
let if_then (i : int) = if i = 2 then 42

(**

It is also possible to add an else clause, like below.

*)
let if_then_else (i : int) = if i = 2 then 42 else 0

(**

Not sure what to write about this yet.

*)
let if_then_else_annot (i : int) =
  if (i = 2 : bool) then
    (42 : int)
  else
    (0 : int)

