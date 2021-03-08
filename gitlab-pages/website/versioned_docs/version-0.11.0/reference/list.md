---
id: list-reference
title: List
description: List operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
function length : nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val length : nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let length: nat
</SyntaxTitle>

Get the number of elements in a list.

<SyntaxTitle syntax="pascaligo">
function size : nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val size : nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let size: nat
</SyntaxTitle>

Get the number of elements in a list.

Synonym for `List.length`.

<SyntaxTitle syntax="pascaligo">
function head_opt : list('a) -> option('a)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val head_opt : 'a list -> 'a option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let head_opt : list('a) => option('a)
</SyntaxTitle>

Get the head of a list

<SyntaxTitle syntax="pascaligo">
function tail_opt : list('a) -> option(list('a))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val tail_opt : 'a list -> 'a list option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let tail_opt : list('a) => option(list('a))
</SyntaxTitle>

Get the tail of a list

<SyntaxTitle syntax="pascaligo">
function iter : ('a -> unit) -> list('a) -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val iter : ('a -> unit) -> 'a list -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let iter: (('a => unit), list('a)) => unit
</SyntaxTitle>

Iterate over items in a list.

<Syntax syntax="pascaligo">

```pascaligo group=lists
function iter_op (const l : list (int)) : unit is
  block {
    function iterated (const i : int) : unit is
      if i > 3 then Unit else (failwith ("Below range.") : unit)
  } with List.iter (iterated, l)
```

> Note that `list_iter` is *deprecated*.

Alternatively it's also possible to use [loops](../language-basics/loops.md).

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=lists
let iter_op (l : int list) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in List.iter predicate l
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=lists
let iter_op = (l : list (int)) : unit => {
  let predicate = (i : int) => assert (i > 3);
  List.iter (predicate, l);
};
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function map : ('a -> 'b) -> list('a) -> list('b)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val map : ('a -> 'b) -> 'a list -> 'b list
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let map: (('a => 'b), list('a)) => list('b)
</SyntaxTitle>

Apply a function to items of a list to create a new list.

<Syntax syntax="pascaligo">

```pascaligo group=lists
const larger_list: list(int) = list [1; 2; 3]

function increment (const i : int): int is i + 1

// Creates a new list with all elements incremented by 1
const plus_one : list (int) = List.map (increment, larger_list)
```

> Note that `list_map` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=lists
let larger_list: int list = [1; 2; 3]

let increment (i : int) : int = i + 1

// Creates a new list with all elements incremented by 1
let plus_one : int list = List.map increment larger_list
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=lists
let larger_list: list(int) = [1, 2, 3];

let increment = (i : int) : int => i + 1;

// Creates a new list with all elements incremented by 1
let plus_one : list (int) = List.map (increment, larger_list);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function fold : (('accumulator -> 'item -> 'accumulator) -> list('item) -> 'accumulator) -> 'accumulator
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val fold : ('accumulator -> 'item -> 'accumulator) -> 'item list -> 'accumulator -> 'accumulator
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let fold: ((('accumulator, 'item) => 'accumulator), list('item), 'accumulator) => 'accumulator
</SyntaxTitle>

[Fold over items in a list](../language-basics/sets-lists-tuples.md#folded-operation-over-lists);

<Syntax syntax="pascaligo">

```pascaligo group=lists
const my_list: list(int) = list [1; 2; 3]

function sum (const acc : int; const i : int): int is acc + i

const sum_of_elements : int = List.fold (sum, my_list, 0)
```

> Note that `list_fold` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=lists
let my_list : int list = [1; 2; 3]

let sum (acc, i : int * int) : int = acc + i

let sum_of_elements : int = List.fold sum my_list 0
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=lists
let my_list : list(int) = [1, 2, 3];

let sum = ((result, i): (int, int)): int => result + i;

let sum_of_elements : int = List.fold (sum, my_list, 0);
```

</Syntax>
