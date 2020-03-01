---
id: string-reference
title: String — Manipulate string data
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

## String.size(s: string) : nat

Get the size of a string. [Michelson only supports ASCII strings](http://tezos.gitlab.io/whitedoc/michelson.html#constants) 
so for now you can assume that each character takes one byte of storage.

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>

<TabItem value="pascaligo">

```pascaligo
function string_size (const s: string) : nat is size(s)
```

</TabItem>
<TabItem value="cameligo">

```cameligo
let size_op (s: string) : nat = String.size s
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
let size_op = (s: string): nat => String.size(s);
```

</TabItem>
</Tabs>

## String.length(s: string) : nat

Alias for `String.size`.

## String.slice(pos1: nat, pos2: nat, s: string) : string

Get the substring of `s` between `pos1` inclusive and `pos2` inclusive. For example
the string "tata" given to the function below would return "at".

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

```pascaligo
function slice_op (const s : string) : string is string_slice(1n , 2n , s)
```

</TabItem>
<TabItem value="cameligo">

```cameligo
let slice_op (s: string) : string = String.slice 1n 2n s
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
let slice_op = (s: string): string => String.slice(1n, 2n, s);
```

</TabItem>
</Tabs>

## String.sub(pos1: nat, pos2: nat, s: string) : string

Alias for `String.slice`.

## String.concat(s1: string, s2: string) : string

Concatenate two strings and return the result.

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>

<TabItem value="pascaligo">

```pascaligo
function concat_op (const s : string) : string is s ^ "toto"
```

</TabItem>
<TabItem value="cameligo">

```cameligo
let concat_syntax (s: string) = s ^ "test_literal"
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
let concat_syntax = (s: string) => s ++ "test_literal";
```

</TabItem>
</Tabs>
