---
id: list-reference
title: Lists
description: List operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
type list(t)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type 't list
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type list('t)
</SyntaxTitle>

An sequence of elements of the same type.

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
function iter : (item -> unit) -> list(item) -> unit
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
function map : (item -> mapped_item) -> list(item) -> list(mapped_item)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val map : ('a -> 'b) -> 'a list -> 'b list
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let map: (('a => 'b), list('a)) => list('b)
</SyntaxTitle>

We may want to change all the elements of a given list by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure.



<Syntax syntax="pascaligo">

```pascaligo group=lists
function increment (const i : int): int is i + 1

// Creates a new list with all elements incremented by 1
const plus_one : list (int) = List.map (increment, larger_list)
```

> Note that `list_map` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=lists
let increment (i : int) : int = i + 1

// Creates a new list with all elements incremented by 1
let plus_one : int list = List.map increment larger_list
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=lists
let increment = (i : int) : int => i + 1;

// Creates a new list with all elements incremented by 1
let plus_one : list (int) = List.map (increment, larger_list);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function fold : (((accumulator -> item -> accumulator) -> list(item) -> accumulator) -> accumulator
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val fold : ('accumulator -> 'item -> 'accumulator) -> 'item list -> 'accumulator -> 'accumulator
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let fold: ((('accumulator, 'iten) => 'accumulator), list('item), 'accumulator) => 'accumulator
</SyntaxTitle>

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over.


<Syntax syntax="pascaligo">

```pascaligo group=lists
function sum (const acc : int; const i : int): int is acc + i
const sum_of_elements : int = List.fold (sum, my_list, 0)
```

> Note that `list_fold` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=lists
let sum (acc, i: int * int) : int = acc + i
let sum_of_elements : int = List.fold sum my_list 0
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=lists
let sum = ((result, i): (int, int)): int => result + i;
let sum_of_elements : int = List.fold (sum, my_list, 0);
```

</Syntax>
