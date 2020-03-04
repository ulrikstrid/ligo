---
id: big-map-reference
title: Big_map
---

import Syntax from '@theme/Syntax';

## Big Maps

Ordinary maps are fine for contracts with a finite lifespan or a
bounded number of users. For many contracts however, the intention is
to have a map holding *many* entries, potentially millions of
them. The cost of loading those entries into the environment each time
a user executes the contract would eventually become too expensive
were it not for *big maps*. Big maps are a data structure offered by
Michelson which handles the scaling concerns for us. In LIGO, the
interface for big maps is analogous to the one used for ordinary maps.

## Declaring a Big Map


<Syntax syntax="pascaligo">

In PascaLIGO, the type of a big map from values of type `key` to
values of type `value` is `big_map (key, value)`.

```pascaligo group=big_map
type move is int * int
type register is big_map (address, move)
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, the type of a big map from values of type `key` to values
of type `value` is `(key, value) big_map`.

```cameligo group=big_map
type move = int * int
type register = (address, move) big_map
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, the type of a big map from values of type `key` to
values of type `value` is `big_map (key, value)`.

```reasonligo group=big_map
type move = (int, int);
type register = big_map (address, move);
```

</Syntax>



## Creating an Empty Big Map

Empty big maps need a type annotation, either as part of a declaration
or an expression.


<Syntax syntax="pascaligo">

```pascaligo group=big_map
const empty : register = big_map []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let empty : register = Big_map.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let empty : register = Big_map.empty
```

</Syntax>



## Creating a Non-empty Big Map

A non-empty big map can be created by giving all its bindings.


<Syntax syntax="pascaligo">

```pascaligo group=big_map
const moves : register =
  big_map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let moves : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let moves : register =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

</Syntax>


## Accessing Values

Given a big map and a key we may retrieve the bound value. Because the
key may be missing in the big map, the result is an *optional value*.


<Syntax syntax="pascaligo">

```pascaligo group=big_map
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let my_balance : option (move) =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);
```

</Syntax>


Notice how the value we read is an optional value: this is to force
the reader to account for a missing key in the map. This requires
*pattern matching*.



<Syntax syntax="pascaligo">

```pascaligo group=big_map
function force_access (const key : address; const moves : register) : move is
  case moves[key] of
    Some (move) -> move
  | None -> (failwith ("No move.") : move)
  end
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let force_access (key, moves : address * register) : move =
  match Big_map.find_opt key moves with
    Some move -> move
  | None -> (failwith "No move." : move)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let force_access = ((key, moves) : (address, register)) : move => {
  switch (Big_map.find_opt (key, moves)) {
  | Some (move) => move
  | None => failwith ("No move.") : move
  }
};
```

</Syntax>

## Updating Big Maps

Given a big map and a key, we might want to remove the key (and its
value) or change its bound value.



<Syntax syntax="pascaligo">

In PascaLIGO, the instruction `m[key] := value` has the effect to add
the binding from the key `key` to the value `value` in the big map
`m`. If the key did not exist before, the binding is added, if it
existed, then `value` becomes the new value.

```pascaligo group=big_map
function assign (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m

const updated_map : register = assign (moves)
```

If multiple bindings need to be updated, PascaLIGO offers a *patch
instruction* for maps, similar to that for records.

```pascaligo group=big_map
function assignments (var m : register) : register is
  block {
    patch m with map [
      ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (4,9);
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2)
    ]
  } with m
```

> Note the use of the keyword `map` instead of `big_map` (which is not
> a keyword).

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, the call `Big_map.update key (Some value) m` is a big map
where the key `key` is bound to the value `value`, and all other
bindings are those found in the big map `m`.

The call `Big_map.update key None m` is a big map containing all the
bindings in the big map `m`, except for the key `key` (binding
removal).

```cameligo group=big_map
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

As a particular case, we can only add a key and its associated value.

```cameligo group=big_map
let add (m : register) : register =
  Big_map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, the call `Big_map.update (key, Some (value), m)` is a
big map where the key `key` is bound to the value `value`, and all
other bindings are those found in the big map `m`.

The call `Big_map.update (key, None, m)` is a big map containing all
the bindings in the big map `m`, except for the key `key` (binding
removal).

```reasonligo group=big_map
let updated_map : register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some ((4,9)), moves);
```

As a particular case, we can only add a key and its associated value.

```reasonligo group=big_map
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

```pascaligo group=big_map
function rem (var m : register) : register is
  block {
    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves
  } with m

const updated_map : register = rem (moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let updated_map : register =
  Map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let updated_map : register =
  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

</Syntax>

