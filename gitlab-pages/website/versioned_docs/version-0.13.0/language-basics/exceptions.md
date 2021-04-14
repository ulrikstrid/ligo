---
id: exceptions
title: Exceptions
---

import Syntax from '@theme/Syntax';

In some cases it's necessary to interrupt the flow of execution and fail the
contract. This is where the `failwith` function comes in.

## The failwith function

The failwith function raises an error that cannot be caught, and terminates the
contract.

<Syntax syntax="pascaligo">

```pascaligo group=failwith
type parameter is
  Zero of nat
| Pos  of nat

type storage is unit

type return is list (operation) * storage

function main (const p : parameter; const s : storage) : return is
  block {
    case p of
      Zero (n) -> if n > 0n then failwith ("fail") else skip
    | Pos (n)  -> if n > 0n then skip else failwith ("fail")
    end
  }
  with ((nil : list (operation)), s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=failwith
type storage = unit

let main (p, store : unit * storage) : operation list * storage =
  (failwith "This contract always fails" : operation list * storage)
```

The call to failwith should be annoted with a type as the typechecker cannot infer the correct type yet.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=failwith
let main = (p : unit, s : unit) =>
  if (true) { 
    failwith("This contract always fails"); 
  };
```

The call to failwith should be annoted with a type as the typechecker cannot infer the correct type yet.

</Syntax>

## Assertions

Assertions can be used to ensure a certain condition is met when running a 
contract. 
`assert` is used to check if a boolean condition is `true` and `assert_some` is used 
to check if an option value is not `None`. When a condition is not met, the 
contract will stop executing and display an error.

<Syntax syntax="pascaligo">

```pascaligo group=failwith
function main (const p : bool; const s : storage) : return is
  block {
	  assert (p)
  }
  with ((nil : list (operation)), s)

function some (const o : option (unit)) is assert_some (o)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=failwith
let main (p, s : bool * unit) =
  let u : unit = assert p
  in ([] : operation list), s

let some (o : unit option) =
  assert_some o
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=failwith
let main = (p : bool, s : unit) => {
  let u : unit = assert (p);
  ([]: list (operation), s);
};

let some = (o : option (unit)) => {
  assert_some (o)
};
```

</Syntax>
