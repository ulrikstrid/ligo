---
id: strings
title: Strings
---



Strings can be defined using the built-in `string` type like this:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
const a: string = "Hello Alice";
```
<!--Cameligo-->
```
let a: string = "Hello Alice"
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Concatenating strings

Strings are concatenated using the `^` operator.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
const name: string = "Alice";
const greeting: string = "Hello";
// Hello Alice
const full_greeting: string = greeting ^ " " ^ name;
```
<!--Cameligo-->
```
let name: string = "Alice"
let greeting: string = "Hello"
let full_greeting: string = greeting ^ " " ^ name
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Slicing strings

Strings can be sliced using the built-in function `string_slice(offset, length, string)`

Here's how:
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
const name: string = "Alice";
// slice = "A"
const slice: string = string_slice(0n, 1n, name);
```
<!--Cameligo-->
```
let name: string = "Alice"
let slice: string = String.slice 0n 1n name
```
<!--END_DOCUSAURUS_CODE_TABS-->

<br/>
> ⚠️ Notice that the `offset` and slice `length` are `nats`

## Aquiring a length of a string

Length of a string can be found using the built-in function `size(string)`:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
const name: string = "Alice";
// length = 5
const length: nat = size(name);
```
<!--Cameligo-->
```
let name: string = "Alice"
let length: nat = String.size name
```
<!--END_DOCUSAURUS_CODE_TABS-->

---

## 🛠 Exercises

### Slice a word in half

Find a sentence/name/word with an even number of characters, and combine your LIGO math skills, with your newly learned string skills to split it in half.
