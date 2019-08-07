---
id: tezos-beginners-guide-chapter-three-variables-and-constants
title: Chapter 3 - Variables and Constants
---

## Variables and Constants
There are two basic ways to define variables, one being with the `var` keyword and another with the `const`.

### Variables
The `var` variable is mutable, and is only supposed to be used inside a function scope. Meaning any `var` declared outside a function will not work. Function signatures will be covered in Chapter 5, but take a look at the example below: 


<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```js
var foo : int = 42 // NOT VALID!!!!!

function main (const i : int) : int is
  begin
    var foo : int = 42; // VALID!!!!!!
  end with i + foo
```

<!--CameLIGO-->

<!--END_DOCUSAURUS_CODE_TABS-->

Variable declarations use the following syntax: 
<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```js
//var <name> : <type> = <value>;
var foo : int = 42 
```

<!--CameLIGO-->

<!--END_DOCUSAURUS_CODE_TABS-->

<br><br>
Now let's use the variable: 
<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```js
foo := foo + 10;
```

<!--CameLIGO-->

<!--END_DOCUSAURUS_CODE_TABS-->