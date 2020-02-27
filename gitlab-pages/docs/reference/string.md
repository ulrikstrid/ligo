---
id: string-reference
title: String
---

## Length of a String

Get the size of a string as a natural
number. [Michelson only supports ASCII strings](http://tezos.gitlab.io/whitedoc/michelson.html#constants)
so for now you can assume that each character takes one byte of
storage.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=string
function string_len (const s: string) : nat is String.length (s)
```

> Note that `size` and `String.size` are *deprecated*. Use
> `String.length`.

<!--CameLIGO-->
```cameligo group=string
let string_len (s : string) : nat = String.length s
```

> Note that `String.size` is *deprecated*. Use `String.length`.

<!--ReasonLIGO-->
```reasonligo group=string
let string_len = (s : string) : nat => String.length (s);
```

> Note that `String.size` is *deprecated*. Use `String.length`.
<!--END_DOCUSAURUS_CODE_TABS-->


## Extracting a Substring

Given a string, the predefined function `String.sub` extracts a
chunk between two positions, knowing that the first character (byte)
in the string has position 0 and both positions are inclusive. If at
least one of the two positions is incorrect, then the call to
`String.sub`.

For example the string `"tata"` given to the function below returns
`"at"`.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=string
function slice (const s : string) : string is String.sub (1n, 2n, s)
```

> Note that `string_slice` is *deprecated*. Use `String.sub`.

<!--CameLIGO-->
```cameligo group=string
let slice (s : string) : string = String.sub 1n 2n s
```

> Note that `String.slice` is *deprecated*. Use `String.sub`.

<!--ReasonLIGO-->
```reasonligo group=string
let slice = (s : string) : string => String.sub (1n, 2n, s);
```

> Note that `String.slice` is *deprecated*. Use `String.sub`.

<!--END_DOCUSAURUS_CODE_TABS-->

## Concatenating two Strings

LIGO offers a predefined operator to concatenate two strings.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=string
function greet (const name : string) : string is "Hello " ^ name
```

<!--CameLIGO-->
```cameligo group=string
let greet (name : string) = "Hello " ^ name
```

<!--ReasonLIGO-->
```reasonligo group=string
let greet = (name : string) => "Hello " ++ name;
```

<!--END_DOCUSAURUS_CODE_TABS-->
