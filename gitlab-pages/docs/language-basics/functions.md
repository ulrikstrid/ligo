---
id: functions
title: Functions
---

Writing code is fun, as long as it doesn't get out of hand. To make sure our code stays put, and doesn't turn into spaghetti, we can group certain logic into functions.

# Defining a function

Functions in LIGO are defined using the `function` keyword followed by their `name`, `parameters` and `return` type definitions.

Here's how you define a basic function that accepts two `ints` and returns an `int` as well:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function add(const a: int; const b: int): int is 
    block { skip } with a + b
```
<!--END_DOCUSAURUS_CODE_TABS-->


Function body consists of two parts:

- `block {<code>}` - logic of the function
- `with <value>` - can be viewed as a return value of the function

> 💡 `skip` can be used as a placeholder for empty function blocks, when all the neccessary logic fits into `with` at the end.


---

## 🛠 Excercise

Now it's time to combine our knowledge from the previous sections of the docs, we know that `vars` can be used within functions, the same applies for `pattern matching` and `if statements`. 

### #1 Build a function, that returns an optional reward

Build a function `check_reward(age: nat): option(reward)` where `reward is string`, that checks if the user is over `25 years old`. If the 'user' (age provided) is over `25n` then grant a reward, otherwise do not grant a reward. Try splitting up the logic into smaller functions.