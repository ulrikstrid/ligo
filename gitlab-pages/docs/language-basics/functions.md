---
id: functions
title: Functions
---

Writing code is fun, as long as it doesn't get out of hand. To make sure our code stays put, and doesn't turn into spaghetti, we can group certain logic into functions.

# Defining a function

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

Functions in PascaLIGO are defined using the `function` keyword followed by their `name`, `parameters` and `return` type definitions.

Here's how you define a basic function that accepts two `ints` and returns an `int` as well:


```pascaligo
function add(const a: int; const b: int): int is
    block { skip } with a + b
```

Function body consists of two parts:

- `block {<code>}` - logic of the function
- `with <value>` - can be viewed as a return value of the function

> 💡 `skip` can be used as a placeholder for empty function blocks, when all the neccessary logic fits into `with` at the end. It is also possible to omit the `block { skip } with`
in the above example, leaving only `a + b`.


<!--Cameligo-->

Functions in CameLIGO are defined using the `let` keyword, like value bindings.
The difference is that after the value name a list of function parameters is provided,
along with a return type.

Here's how you define a basic function that accepts two `ints` and returns an `int` as well:

```cameligo
let add (a: int) (b: int) : int = a + b
```

The function body is a series of expressions, which are evaluated to give the return
value.

<!--END_DOCUSAURUS_CODE_TABS-->

---

## 🛠 Excercise

Now it's time to combine our knowledge from the previous sections of the docs, we know that `vars` can be used within functions, the same applies for `pattern matching` and `if statements`. 

### #1 Build a function, that returns an optional reward

Build a function `check_reward(age: nat): option(reward)` where `reward is string`, that checks if the user is over `25 years old`. If the 'user' (age provided) is over `25n` then grant a reward, otherwise do not grant a reward. Try splitting up the logic into smaller functions.
