// Test a function which takes another function as an argument
let foobar (i : int) : int =
  let foo (i : int) : int = i
  in
  let bar (f : int -> int) : int = f i
  in
  bar foo

// higher order function with more than one argument
let higher2 (i: int) (f: int -> int): int =
  let ii: int = f i in ii

let foobar2 (i : int) : int =
  let foo2 (i : int) : int = i
  in
  higher2 i foo2

let a : int = 0

let foobar3 (i : int) : int =
  let foo2 (i : int) : int =  a + i
  in
  higher2 i foo2

let f (i : int) : int = i

let g (i : int) : int = f i

let foobar4 (i : int) : int = g (g i)

let higher3 (i: int) (f: int -> int) (g: int -> int) : int =
  let ii: int = f (g i) in ii

let foobar5 (i : int) : int =
  let a : int = 0 in
  let foo (i : int) : int = a + i
  in
  let goo (i : int) : int = foo i
  in
  higher3 (i (foo goo))
