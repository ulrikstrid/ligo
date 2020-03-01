---
id: strings
title: Strings
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

Strings are defined using the built-in `string` type like this:

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

```
const a : string = "Hello Alice"
```

</TabItem>
<TabItem value="cameligo">

```
let a : string = "Hello Alice"
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
let a : string = "Hello Alice";
```

</TabItem>
</Tabs>


## Concatenating Strings

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

Strings can be concatenated using the `^` operator.

```pascaligo group=a
const name : string = "Alice"
const greeting : string = "Hello"
const full_greeting : string = greeting ^ " " ^ name
```

</TabItem>
<TabItem value="cameligo">

Strings can be concatenated using the `^` operator.

```cameligo group=a
let name : string = "Alice"
let greeting : string = "Hello"
let full_greeting : string = greeting ^ " " ^ name
```

</TabItem>
<TabItem value="reasonligo">

Strings can be concatenated using the `++` operator.

```reasonligo group=a
let name : string = "Alice";
let greeting : string = "Hello";
let full_greeting : string = greeting ++ " " ++ name;
```

</TabItem>
</Tabs>


## Slicing Strings

Strings can be sliced using a built-in function:

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

```pascaligo group=b
const name  : string = "Alice"
const slice : string = String.slice (0n, 1n, name)
```

> Note that `string_slide` is *deprecated*.

</TabItem>
<TabItem value="cameligo">

```cameligo group=b
let name  : string = "Alice"
let slice : string = String.slice 0n 1n name
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=b
let name  : string = "Alice";
let slice : string = String.slice (0n, 1n, name);
```

</TabItem>
</Tabs>

> ⚠️ Notice that the offset and length of the slice are natural
> numbers.

## Length of Strings

The length of a string can be found using a built-in function:

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

```pascaligo group=c
const name : string = "Alice"
const length : nat = String.length (name) // length = 5
```

> Note that `size` is *deprecated*.

</TabItem>
<TabItem value="cameligo">

```cameligo group=c
let name : string = "Alice"
let length : nat = String.size name  // length = 5
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=c
let name : string = "Alice";
let length : nat = String.size (name);  // length == 5
```

</TabItem>
</Tabs>
