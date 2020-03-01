---
id: unit-option-pattern-matching
title: Unit, Option, Pattern matching
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';


Optionals are a pervasive programing pattern in OCaml. Since Michelson
and LIGO are both inspired by OCaml, *optional types* are available in
LIGO as well. Similarly, OCaml features a *unit* type, and LIGO
features it as well. Both the option type and the unit types are
instances of a more general kind of types: *variant types* (sometimes
called *sum types*).

## The unit Type

The `unit` type in Michelson or LIGO is a predefined type that
contains only one value that carries no information. It is used when
no relevant information is required or produced. Here is how it used.

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

In PascaLIGO, the unique value of the `unit` type is `Unit`.
```pascaligo group=a
const n : unit = Unit // Note the capital letter
```

</TabItem>
<TabItem value="cameligo">

In CameLIGO, the unique value of the `unit` type is `()`, following
the OCaml convention.
```cameligo group=a
let n : unit = ()
```

</TabItem>
<TabItem value="reasonligo">

In ReasonLIGO, the unique value of the `unit` type is `()`, following
the OCaml convention.
```reasonligo group=a
let n : unit = ();
```

</TabItem>
</Tabs>

## Variant types

A variant type is a user-defined or a built-in type (in case of
options) that defines a type by cases, so a value of a variant type is
either this, or that or... The simplest variant type is equivalent to
the enumerated types found in Java, C++, JavaScript etc.

Here is how we define a coin as being either head or tail (and nothing
else):

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

```pascaligo group=b
type coin is Head | Tail
const head : coin = Head
const tail : coin = Tail
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=b
type coin = Head | Tail
let head : coin = Head
let tail : coin = Tail
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=b
type coin = Head | Tail;
let head : coin = Head;
let tail : coin = Tail;
```

</TabItem>
</Tabs>

The names `Head` and `Tail` in the definition of the type `coin` are
called *data constructors*, or *variants*. In this particular, they
carry no information beyond their names, so they are called *constant
constructors*.

In general, it is interesting for variants to carry some information,
and thus go beyond enumerated types. In the following, we show how to
define different kinds of users of a system.

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>

<TabItem value="pascaligo">

```pascaligo group=c
type id is nat

type user is
  Admin   of id
| Manager of id
| Guest

const u : user = Admin (1000n)
const g : user = Guest
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=c
type id = nat

type user =
  Admin   of id
| Manager of id
| Guest

let u : user = Admin 1000n
let g : user = Guest
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=c
type id = nat;

type user =
| Admin   (id)
| Manager (id)
| Guest;

let u : user = Admin (1000n);
let g : user = Guest;
```

</TabItem>
</Tabs>

In LIGO, a constant constructor is equivalent to the same constructor
taking an argument of type `unit`, so, for example, `Guest` is the
same value as `Guest (unit)`.

## Optional values

The `option` type is a predefined variant type that is used to express
whether there is a value of some type or none. This is especially
useful when calling a *partial function*, that is, a function that is
not defined for some inputs. In that case, the value of the `option`
type would be `None`, otherwise `Some (v)`, where `v` is some
meaningful value *of any type*. An example in arithmetic is the
division operation:

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

```pascaligo group=d
function div (const a : nat; const b : nat) : option (nat) is
  if b = 0n then (None: option (nat)) else Some (a/b)
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=d
let div (a, b : nat * nat) : nat option =
  if b = 0n then (None: nat option) else Some (a/b)
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=d
let div = ((a, b) : (nat, nat)) : option (nat) =>
  if (b == 0n) { (None: option (nat)); } else { Some (a/b); };
```

</TabItem>
</Tabs>


## Pattern matching

*Pattern matching* is similiar to the `switch` construct in
Javascript, and can be used to route the program's control flow based
on the value of a variant. Consider for example the definition of a
function `flip` that flips a coin.

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

```pascaligo group=e
type coin is Head | Tail

function flip (const c : coin) : coin is
  case c of
    Head -> Tail
  | Tail -> Head
  end
```

You can call the function `flip` by using the LIGO compiler like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/unit-option-pattern-matching/flip.ligo
flip "Head"
# Outputs: Tail(Unit)
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=e
type coin = Head | Tail

let flip (c : coin) : coin =
  match c with
    Head -> Tail
  | Tail -> Head
```

You can call the function `flip` by using the LIGO compiler like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/unit-option-pattern-matching/flip.mligo
flip Head
# Outputs: Tail(Unit)
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=e
type coin = | Head | Tail;

let flip = (c : coin) : coin =>
  switch (c) {
  | Head => Tail
  | Tail => Head
  };
```

You can call the function `flip` by using the LIGO compiler like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/unit-option-pattern-matching/flip.religo
flip Head
# Outputs: Tail(Unit)
```

</TabItem>
</Tabs>
