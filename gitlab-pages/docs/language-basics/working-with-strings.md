---
id: working-with-strings
title: Working with strings
---



Strings can be defined using the built-in `string` type like this:

```
const a: string = "Hello Alice";
```

## Concatenating strings

Strings are concatenated using the `^` operator.

```
const name: string = "Alice";
const greeting: string = "Hello";
// Hello Alice
const full_greeting: string = greeting ^ " " ^ name;
```

## Slicing strings

Strings can be sliced using the built-in function `string_slice(offset, length, string)`

Here's how:
```
const name: string = "Alice";
// slice = "A"
const slice: string = string_slice(0n, 1n, name);
```

> ⚠️ Notice that the `offset` and slice `length` are `nats`

## Aquiring a length of a string

Length of a string can be found using the built-in function `size(string)`:

```
const name: string = "Alice";
// length = 5
const length: nat = size(name);
```

---

## 🛠 Exercises

### Slice a word in half

Find a sentence/name/word with an even number of characters, and combine your LIGO math skills, with your newly learned string skills to split it in half.