(**

The set type is a collection of unique elements. LIGO sets are currently a bit
basic. They have addition, removal, and membership testing.

*)

let add_op (s : string set) : string set =
   Set.add "foobar" s

let remove_op (s : string set) : string set =
   Set.remove "foobar" s

let mem_op (s : string set) : bool =
   Set.mem "foobar" s

(**

It's possible to remove elements from sets even if they're still inside another
data structure such as a tuple.

*)
let remove_deep (s : string set * nat) : string set * nat =
  Set.remove "foobar" s.(0)

(**

It's also possible to get the size of a set.

*)
let size_op (s: string set) : nat =
  Set.size s

(**

Finally, sets support a fold operation. The fold is a loop which extracts an
element of the set on each iteration. It then provides this element and an
existing value to a folding function which combines them. On the first iteration,
the existing value is an initial expression given by the programmer. On each
subsequent iteration it is the result of the previous iteration. It eventually
returns the result of combining all the elements.

*)
let aggregate (i : int) (j : int) : int = i + j

let fold_op (s : int set) : int =
  Set.fold s 15 aggregate
