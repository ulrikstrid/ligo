---
id: boolean-if-else
title: Booleans and Conditionals
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

## Booleans

The type of a boolean value is `bool`. Here is how to define a boolean
value:

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

```pascaligo group=a
const a : bool = True   // Also: true
const b : bool = False  // Also: false
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=a
let a : bool = true
let b : bool = false
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=a
let a : bool = true;
let b : bool = false;
```

</TabItem>
</Tabs>

## Comparing Values

In LIGO, only values of the same type can be compared. Moreover, not
all values of the same type can be compared, only those with
*comparable types*, which is a concept lifted from
Michelson. Comparable types include, for instance, `int`, `nat`,
`string`, `tez`, `timestamp`, `address`, etc. As an example of
non-comparable types: maps, sets or lists are not comparable: if you
wish to compare them, you will have to write your own comparison
function.

### Comparing Strings

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
const a : string = "Alice"
const b : string = "Alice"
const c : bool = (a = b) // True
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=b
let a : string = "Alice"
let b : string = "Alice"
let c : bool = (a = b) // true
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=b
let a : string = "Alice";
let b : string = "Alice";
let c : bool = (a == b); // true
```

</TabItem>
</Tabs>

### Comparing numbers

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
const a : int  = 5
const b : int  = 4
const c : bool = (a = b)
const d : bool = (a > b)
const e : bool = (a < b)
const f : bool = (a <= b)
const g : bool = (a >= b)
const h : bool = (a =/= b)
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=c
let a : int  = 5
let b : int  = 4
let c : bool = (a = b)
let d : bool = (a > b)
let e : bool = (a < b)
let f : bool = (a <= b)
let g : bool = (a >= b)
let h : bool = (a <> b)
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=c
let a : int  = 5;
let b : int  = 4;
let c : bool = (a == b);
let d : bool = (a > b);
let e : bool = (a < b);
let f : bool = (a <= b);
let g : bool = (a >= b);
let h : bool = (a != b);
```

</TabItem>
</Tabs>

### Comparing tez

> 💡 Comparing `tez` values is especially useful when dealing with an
> amount sent in a transaction.

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
const a : tez  = 5mutez
const b : tez  = 10mutez
const c : bool = (a = b) // False
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=d
let a : tez  = 5mutez
let b : tez  = 10mutez
let c : bool = (a = b) // false
```

</TabItem>
<TabItem value="reasonligo">
```reasonligo group=d
let a : tez  = 5mutez;
let b : tez  = 10mutez;
let c : bool = (a == b); // false
```

</TabItem>
</Tabs>


## Conditionals

Conditional logic enables forking the control flow depending on the
state.

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
type magnitude is Small | Large // See variant types.

function compare (const n : nat) : magnitude is
  if n < 10n then Small else Large
```

You can run the `compare` function defined above using the LIGO compiler
like this:
```shell
ligo run-function
gitlab-pages/docs/language-basics/boolean-if-else/cond.ligo compare 21n'
# Outputs: Large(Unit)
```

When the branches of the conditional are not a single expression, as
above, we need a block:

```pascaligo skip
if x < y then
  block {
    const z : nat = x;
    x := y; y := z
  }
else skip;
```

As an exception to the rule, the blocks in a conditional branch do not
need to be introduced by the keyword `block`, so we could have written
instead:
```pascaligo skip
if x < y then {
  const z : nat = x;
  x := y; y := z
}
else skip;
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=e
type magnitude = Small | Large // See variant types.

let compare (n : nat) : magnitude =
  if n < 10n then Small else Large
```

You can run the `compare` function defined above using the LIGO compiler
like this:
```shell
ligo run-function
gitlab-pages/docs/language-basics/boolean-if-else/cond.mligo compare 21n'
# Outputs: Large
```

> Notice that, as in OCaml, in CameLIGO, if a conditional has a branch
> `else ()`, that branch can be omitted. The resulting so-called
> *dangling else* problem is parsed by associating any `else` to the
> closest previous `then`.

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=e
type magnitude = Small | Large; // See variant types.

let compare = (n : nat) : magnitude =>
  if (n < 10n) { Small; } else { Large; };
```

You can run the `compare` function defined above using the LIGO compiler
like this:
```shell
ligo run-function
gitlab-pages/docs/language-basics/boolean-if-else/cond.religo compare 21n'
# Outputs: Large
```

</TabItem>
</Tabs>
