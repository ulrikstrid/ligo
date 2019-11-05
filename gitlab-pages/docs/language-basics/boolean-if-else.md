---
id: boolean-if-else
title: Boolean, If, Else
---

## Boolean

The type of a Boolean is `bool` and the possible values are `True` and `False`.

Here's how to define a boolean:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const a: bool = True;
const b: bool = False;
```
<!--Cameligo-->
```cameligo
let a: bool = true
let b: bool = false
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Comparing two values

In LIGO, only values of a certain type can be compared together, we call those of a comparable type. Comparable types include e.g. `int`, `nat`, `string`, `tez`, `timestamp`, `address`, ...

### Comparing strings

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const a: string = "Alice";
const b: string = "Alice";
// True
const c: bool = (a = b);
```
<!--Cameligo-->
```cameligo
let a: string = "Alice"
let b: string = "Alice"
// true
let c: bool = (a = b)
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Comparing numbers

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const a: int = 5;
const b: int = 4;
const c: bool = (a = b);
const d: bool = (a > b);
const e: bool = (a < b);
const f: bool = (a <= b);
const g: bool = (a >= b);
const h: bool = (a =/= b);
```
<!--Cameligo-->
```cameligo
let a: int = 5
let b: int = 4
let c: bool = (a = b)
let d: bool = (a > b)
let e: bool = (a < b)
let f: bool = (a <= b)
let g: bool = (a >= b)
let h: bool = (a =/= b)
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Comparing tez

> 💡 Comparing `tez` values is especially useful, when dealing with an `amount` sent in a transaction.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const a: tez = 5mtz;
const b: tez = 10mtz;
const c: bool = (a = b);
```
<!--Cameligo-->
```cameligo
let a: tez = 5mtz
let b: tez = 10mtz
// false
let c: bool = (a = b)
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Conditionals, if staments and more

Conditional logic is an important part of every real world program.

### If/else statements

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const min_age: nat = 16n;

(*
    This function is really obnoxious, but it serves the purpose of
    showcasing how the if statement and it's syntax can be used.

    Normally, you'd do `with (age > min_age)` instead.
*)
function is_adult(const age: nat): bool is
    block {
        var is_adult: bool := False;
        if (age > min_age) then begin
            is_adult := True;
        end else begin
            is_adult := False;
        end
    } with is_adult
```

<br/>

> You can run the function above with
> ```
> ligo run-function -s pascaligo src/if-else.ligo is_adult 21n
> ```

<!--Cameligo-->
```cameligo
let min_age: nat = 16n

(**

    This function is really obnoxious, but it serves the purpose of
    showcasing how the if statement and it's syntax can be used.

    Normally, you'd do `with (age > min_age)` instead.

*)
let is_adult (age: nat) : bool =
  if (age > min_age) then true else false
```

<br/>

> You can run the function above with
> ```
> ligo run-function -s cameligo src/if-else.mligo is_adult 21n
> ```

<!--END_DOCUSAURUS_CODE_TABS-->


---

## 🛠 Excercise

### Implement a name length validator function

Let's pretend we're trying to implement a contract, where we require a minimal string length for a name, in case the name is not long enough, our validation function will return `False`.
Your task is to implement a function, same as the `is_adult` in the example above, that will return `False` in case the provided parameter of type `string` will not be at least `5` characters long. Your function should be callable as `is_long_enough("name goes here")`.
