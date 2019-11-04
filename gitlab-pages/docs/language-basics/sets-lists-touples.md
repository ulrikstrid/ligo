---
id: sets-lists-touples
title: Sets, Lists, Touples
---

Apart from complex data types such as `maps` and `records`, ligo also exposes `sets`, `lists` and `tuples`.

> ⚠️ Make sure to pick the appropriate data type for your use case, as it carries not only semantical, but also gas related costs.

## Sets

Sets are similar to lists, the main difference is that elements of a `set` are supposed to be *unique*.

### Defining a set

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type int_set is set(int);
const my_set: int_set = set 
    1; 
    2; 
    3; 
end
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Checking if set contains an element

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const contains_three: bool = my_set contains 3;
// or alternatively
const contains_three_fn: bool = set_mem(3, my_set);
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Obtaining the size of a set
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const set_size: nat = size(my_set);
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Modifying a set
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const larger_set: int_set = set_add(4, my_set);
const smaller_set: int_set = set_remove(3, my_set);
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Folding a set
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function sum(const result: int; const i: int): int is block { skip } with result + i;
// Outputs 6
const sum_of_a_set: int = set_fold(my_set, 0, sum);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Lists

Lists are similar to sets, but their elements don't need to be unique, and they don't offer the same range of built-in functions.

> 💡 Lists are useful when returning operations from a smart contract's entrypoint.

### Defining a list

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type int_list is list(int);
const my_list: int_list = list
    1;
    2;
    3;
end
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Appending an element to a list

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const larger_list: int_list = cons(4, my_list);
const even_larger_list: int_list = 5 # larger_list;
```
<!--END_DOCUSAURUS_CODE_TABS-->

<br/>
> 💡 Lists can be iterated, folded or mapped to different values. You can find additional examples [here](https://gitlab.com/ligolang/ligo/tree/dev/src/test/contracts) and other built-in operators [here](https://gitlab.com/ligolang/ligo/blob/dev/src/passes/operators/operators.ml#L59)

### Mapping of a list

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function increment(const i: int): int is block { skip } with i + 1;
// Creates a new list with elements incremented by 1
const incremented_list: int_list = list_map(even_larger_list, increment);
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Folding of a list:
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function sum(const result: int; const i: int): int is block { skip } with result + i;
// Outputs 6
const sum_of_a_list: int = list_fold(my_list, 0, sum);
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Tuples

Tuples are useful for carrying data that belong together, but don't necessarily have an index or a specific name.

### Defining a touple

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type full_name is string * string;
const full_name: full_name = ("Alice", "Johnson");
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Accessing an element in a touple
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const first_name: string = full_name.1;
```
<!--END_DOCUSAURUS_CODE_TABS-->

---

## 🛠 Exercises

### #1 Access control functions

Implement a program, that stores a set of `addresses` as owners, and implement a function, that checks if the provided address is indeed an `owner` of this program.

### #2 Find the oldest person in a list

Implement a program which stores a `list` of `persons`, where `person` is a `record` with `name` and `age` fields. Use `list_fold` to find the oldest person in the list.
