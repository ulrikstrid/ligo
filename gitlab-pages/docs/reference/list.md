---
id: list-reference
title: List
---

Lists are linear collections of elements of the same type. Linear
means that, in order to reach an element in a list, we must visit all
the elements before (sequential access). Elements can be repeated, as
only their order in the collection matters. The first element is
called the *head*, and the sub-list after the head is called the
*tail*. For those familiar with algorithmic data structure, you can
think of a list a *stack*, where the top is written on the left.

## Declaring a List

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

In PascaLIGO, the type of a list of values of type `t` is `list (t)`.
```pascaligo group=list
type series is list (int)
```

<!--CameLIGO-->

In CameLIGO, the type of a list of values of type `t` is `t list`.
```cameligo group=list
type series = int list
```

<!--ReasonLIGO-->
In ReasonLIGO, the type of a list of values of type `t` is `list (t)`.

```reasonligo group=list
type series = list (int);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Creating an Empty List

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
In PascaLIGO, the empty list can be written in many ways: `nil` or
`list []`.
```pascaligo group=list
const empty_list : list (int) = nil // Or list []
```

<!--CameLIGO-->
In CameLIGO, the only way to denote the empty list is `[]`.
```cameligo group=list
let empty_list : int list = []
```

<!--ReasonLIGO-->
In ReasonLIGO, the only way to denote the empty list is `[]`.
```reasonligo group=list
let empty_list : list (int) = [];
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Creating a Non-Empty List

A non-empty list can be created by giving all its elements.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

In PascaLIGO, the elements are separated by semicolons inside the
notation for empty lists.
```pascaligo group=list
const my_list : list (int) = list [1; 2; 2] // The head is 1
```

<!--CameLIGO-->

In CameLIGO, the elements are separated by semicolons inside the
notation for empty lists.
```cameligo group=list
let my_list : int list = [1; 2; 2] // The head is 1
```

<!--ReasonLIGO-->

In ReasonLIGO, the elements are separated by commas inside the
notation for empty lists.
```reasonligo group=list
let my_list : list (int) = [1, 2, 2]; // The head is 1
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Adding to Lists

Lists can be augmented by adding an element before the head, so it
becomes the new head (or, in terms of stack, by *pushing an element on
the top*).

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

In PascaLIGO, the expression `item # list` is a list starting with the
element `item` and followed by all the items of list `list` in the
same order.

```pascaligo group=list
const larger_list : list (int) = 5 # my_list // [5;1;2;2]
```

<!--CameLIGO-->

In CameLIGO, the expression `item :: list` is a list starting with the
element `item` and followed by all the items of list `list` in the
same order.

```cameligo group=list
let larger_list : int list = 5 :: my_list // [5;1;2;2]
```

<!--ReasonLIGO-->

In ReasonLIGO, the expression `item, ...list` is a list starting with
the element `item` and followed by all the items of list `list` in the
same order.

```reasonligo group=list
let larger_list : list (int) = [5, ...my_list]; // [5,1,2,2]
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Functional Iteration over Lists

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is possible in PascaLIGO:
*loops* (see the relevant section).

There are three kinds of functional iterations over LIGO lists: the
*iterated operation*, the *map operation* (not to be confused with the
*map data structure*) and the *fold operation*.

### Iterated Operation over Lists

The first, the *iterated operation*, is an iteration over the list
with a unit return value. It is useful to enforce certain invariants
on the element of a list, or fail.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo group=list
function iter_op (const l : list (int)) : unit is
  block {
    function iterated (const i : int) : unit is
      if i > 3 then Unit else (failwith ("Below range.") : unit)
  } with List.iter (iterated, l)
```

> Note that `list_iter` is *deprecated*. Use `List.iter`.


<!--CameLIGO-->

```cameligo group=list
let iter_op (l : int list) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in List.iter predicate l
```

<!--ReasonLIGO-->

```reasonligo group=list
let iter_op = (l : list (int)) : unit => {
  let predicate = (i : int) => assert (i > 3);
  List.iter (predicate, l);
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Mapped Operation over Lists

We may want to change all the elements of a given list by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo group=list
function increment (const i : int): int is i + 1

// Creates a new list with all elements incremented by 1
const plus_one : list (int) = List.map (increment, larger_list)
```

> Note that `list_map` is *deprecated*. Use `List.map`.

<!--CameLIGO-->

```cameligo group=list
let increment (i : int) : int = i + 1

// Creates a new list with all elements incremented by 1
let plus_one : int list = List.map increment larger_list
```

<!--ReasonLIGO-->

```reasonligo group=list
let increment = (i : int) : int => i + 1;

// Creates a new list with all elements incremented by 1
let plus_one : list (int) = List.map (increment, larger_list);
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Folded Operation over Lists

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo group=list
function sum (const acc : int; const i : int): int is acc + i
const sum_of_elements : int = List.fold (sum, my_list, 0)
```

> Note that `list_fold` is *deprecated*. Use `List.fold`.

<!--CameLIGO-->

```cameligo group=list
let sum (acc, i: int * int) : int = acc + i
let sum_of_elements : int = List.fold sum my_list 0
```

<!--ReasonLIGO-->

```reasonligo group=list
let sum = ((result, i): (int, int)): int => result + i;
let sum_of_elements : int = List.fold (sum, my_list, 0);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## List Length

Get the number of elements in a list.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function size_of (const l : list (int)) : nat is List.length (l)
```

> Note that `size` is *deprecated*. Use `List.length`.

<!--CameLIGO-->
```cameligo
let size_of (l : int list) : nat = List.length l
```

> Note that `List.size` is *deprecated*. Use `List.length`.

<!--ReasonLIGO-->
```reasonligo
let size_of = (l : list (int)) : nat => List.length (l);
```

> Note that `List.size` is *deprecated*. Use `List.length`.

<!--END_DOCUSAURUS_CODE_TABS-->
