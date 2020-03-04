---
id: map-reference
title: Map
---

import Syntax from '@theme/Syntax';

*Maps* are a data structure which associate values of the same type to
values of the same type. The former are called *key* and the latter
*values*. Together they make up a *binding*. An additional requirement
is that the type of the keys must be *comparable*, in the Michelson
sense.

## Declaring a Map


<Syntax syntax="pascaligo">

In PascaLIGO, the type of a map from values of type `key` to values of
type `value` is `map (key, value)`.

```pascaligo group=map
type move is int * int
type register is map (address, move)
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, the type of a map from values of type `key` to values of
type `value` is `(key, value) map`.

```cameligo group=map
type move = int * int
type register = (address, move) map
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, the type of a map from values of type `key` to values
of type `value` is `map (key, value)`.

```reasonligo group=map
type move = (int, int);
type register = map (address, move);
```

</Syntax>



## Creating an Empty Map

Empty maps need a type annotation, either as part of a declaration or
an expression.


<Syntax syntax="pascaligo">

```pascaligo group=map
const empty : register = map []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=map
let empty : register = Map.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=map
let empty : register = Map.empty
```

</Syntax>



## Creating a Non-Empty Map

A non-empty map can be created by giving all its bindings.


<Syntax syntax="pascaligo">

```pascaligo group=map
const moves : register =
  map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=map
let moves : register =
  Map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=map
let moves : register =
  Map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

</Syntax>


## Accessing Values

Given a map and a key we may retrieve the bound value. Because the key
may be missing in the map, the result is an *optional value*.


<Syntax syntax="pascaligo">

```pascaligo group=map
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=map
let my_balance : move option =
  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=map
let my_balance : option (move) =
  Map.find_opt (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), moves);
```

</Syntax>


Notice how the value we read is an optional value: this is to force
the reader to account for a missing key in the map. This requires
*pattern matching*.


<Syntax syntax="pascaligo">

```pascaligo group=map
function force_access (const key : address; const moves : register) : move is
  case moves[key] of
    Some (move) -> move
  | None -> (failwith ("No move.") : move)
  end
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=map
let force_access (key, moves : address * register) : move =
  match Map.find_opt key moves with
    Some move -> move
  | None -> (failwith "No move." : move)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=map
let force_access = ((key, moves) : (address, register)) : move => {
  switch (Map.find_opt (key, moves)) {
  | Some (move) => move
  | None => failwith ("No move.") : move
  }
};
```

</Syntax>


## Updating Maps

Given a map and a key, we might want to remove the key (and its value)
or change its bound value.



<Syntax syntax="pascaligo">

In PascaLIGO, the instruction `m[key] := value` has the effect to add
the binding from the key `key` to the value `value` in the map
`m`. If the key did not exist before, the binding is added, if it
existed, then `value` becomes the new value.

```pascaligo group=map
function assign (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m

const updated_map : register = assign (moves)
```

If multiple bindings need to be updated, PascaLIGO offers a *patch
instruction* for maps, similar to that for records.

```pascaligo group=map
function assignments (var m : register) : register is
  block {
    patch m with map [
      ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (4,9);
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2)
    ]
  } with m
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, the call `Map.update key (Some value) m` is a map where
the key `key` is bound to the value `value`, and all other bindings
are those found in the map `m`.

The call `Map.update key None m` is a map containing all the bindings
in the map `m`, except for the key `key` (binding removal).

```cameligo group=map
let updated_map : register =
  Map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```
Notice the optional value `Some (4,9)` instead of `(4,9)`. If we had
use `None` instead, that would have meant that the binding is removed.

As a particular case, we can only add a key and its associated value.

```cameligo group=map
let add (m : register) : register =
  Map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=map
let updated_map : register =
  Map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), Some ((4,9)), moves);
```

Notice the optional value `Some (4,9)` instead of `(4,9)`. If we had
use `None` instead, that would have meant that the binding is removed.

As a particular case, we can only add a key and its associated value.

```reasonligo group=map
let add = (m : register) : register =>
  Map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4,9), m);
```

</Syntax>


## Removing Bindings


<Syntax syntax="pascaligo">

In PascaLIGO, the instruction `remove key from map m` removes the key
`key` from the big map `m` (note that the keyword is `map`, not
`big_map`).

```pascaligo group=map
function rem (var m : register) : register is
  block {
    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves
  } with m

const updated_map : register = rem (moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=map
let updated_map : register =
  Map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=map
let updated_map : register =
  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

</Syntax>


## Functional Iteration over Maps

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is possible in PascaLIGO:
*loops* (see the relevant section).

There are three kinds of functional iterations over LIGO maps: the
*iterated operation*, the *map operation* (not to be confused with the
*map data structure*) and the *fold operation*.

### Iterated Operation over Maps

The first, the *iterated operation*, is an iteration over the map with
no return value: its only use is to produce side-effects. This can be
useful if for example you would like to check that each value inside
of a map is within a certain range, and fail with an error otherwise.



<Syntax syntax="pascaligo">

```pascaligo group=map
function iter_op (const m : register) : unit is
  block {
    function iterated (const i : address; const j : move) : unit is
      if j.1 > 3 then Unit else (failwith ("Below range.") : unit)
  } with Map.iter (iterated, m)
```

> Note that `map_iter` is *deprecated*. Use `Map.iter`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=map
let iter_op (m : register) : unit =
  let predicate = fun (i,j : address * move) -> assert (j.0 > 3)
  in Map.iter predicate m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=map
let iter_op = (m : register) : unit => {
  let predicate = ((i,j) : (address, move)) => assert (j[0] > 3);
  Map.iter (predicate, m);
};
```

</Syntax>


### Map Operations over Maps

We may want to change all the bindings of a map by applying to them a
function. This is called a *map operation*, not to be confused with
the map data structure. The predefined functional iterator
implementing the map operation over maps is called `Map.map`.



<Syntax syntax="pascaligo">

```pascaligo group=map
function map_op (const m : register) : register is
  block {
    function increment (const i : address; const j : move) : move is
      (j.0, j.1 + 1)
  } with Map.map (increment, m)
```

> Note that `map_map` is *deprecated*. Use `Map.map`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=map
let map_op (m : register) : register =
  let increment = fun (i,j : address * move) -> j.0, j.1 + 1
  in Map.map increment m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=map
let map_op = (m : register) : register => {
  let increment = ((i,j): (address, move)) => (j[0], j[1] + 1);
  Map.map (increment, m);
};
```

</Syntax>


### Folded Operations over Maps

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over.



<Syntax syntax="pascaligo">

```pascaligo group=map
function fold_op (const m : register) : int is
  block {
    function folded (const i : int; const j : address * move) : int is
      i + j.1.1
  } with Map.fold (folded, m, 5)
```

> Note that `map_fold` is *deprecated*. Use `Map.fold`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=map
let fold_op (m : register) : register =
  let folded = fun (i,j : int * (address * move)) -> i + j.1.1
  in Map.fold folded m 5
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=map
let fold_op = (m : register) : register => {
  let folded = ((i,j): (int, (address, move))) => i + j[1][1];
  Map.fold (folded, m, 5);
};
```

</Syntax>

