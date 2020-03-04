---
id: bytes-reference
title: Bytes
---

import Syntax from '@theme/Syntax';

## Concatenating two Bytes

The predefined function `Bytes.concat` concatenates two sequences of
bytes (that is, two values of type `bytes`), yielding a new sequence
containing both in the given order.

<Syntax syntax="pascaligo">

```pascaligo group=bytes
function concat (const b : bytes) : bytes is Bytes.concat (b, 0x7070)
```

> Note that `bytes_concat` is *deprecated*. Use `Bytes.concat`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=bytes
let concat (b : bytes) : bytes = Bytes.concat b 0x7070
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=bytes
let concat = (b : bytes): bytes => Bytes.concat (b, 0x7070);
```

</Syntax>

## Extracting Subsequences of Bytes

Bytes in a sequence of bytes, that is, a value of type `bytes` can be
extracted by slicing the original sequence between two positions
thanks to the predefined function `Bytes.sub`. *The first byte has
position 0 and both positions are inclusive*. So, for example, slicing
`0xff7a7aff` between positions `1n` and `2n` (they cannot be negative)
yields the sequence `0x7a7a`. If any of the two positions is invalid,
the call to `Bytes.sub` fails.



<Syntax syntax="pascaligo">

```pascaligo group=bytes
function slice (const b : bytes) : bytes is Bytes.sub (1n, 2n, b)
```

> Note that `bytes_slice` is *deprecated*. Use `Bytes.sub`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=bytes
let slice (b : bytes) : bytes = Bytes.sub 1n 2n b
```

> Note that `Bytes.slice` is *deprecated*. Use `Bytes.sub`.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=bytes
let slice = (b : bytes): bytes => Bytes.sub (1n, 2n, b);
```

> Note that `Bytes.slice` is *deprecated*. Use `Bytes.sub`.

</Syntax>



## Pack and Unpack

As Michelson provides the `PACK` and `UNPACK` instructions for data
serialization, so does LIGO with `Bytes.pack` and `Bytes.unpack`.  The
former serializes Michelson data structures into a binary format, and
the latter reverses that transformation. Unpacking may fail, so the
return type of `Byte.unpack` is an option that needs to be annotated.

> ⚠️ `PACK` and `UNPACK` are Michelson instructions that are intended
> to be used by people that really know what they are doing. There are
> several risks and failure cases, such as unpacking a lambda from an
> untrusted source or casting the result to the wrong type. Do not use
> the corresponding LIGO functions without doing your homework first.


<Syntax syntax="pascaligo">

```pascaligo group=a
function id_string (const p : string) : option (string) is block {
  const packed : bytes = Bytes.pack (p)
} with (Bytes.unpack (packed) : option (string))
```

> Note that `bytes_pack` and `bytes_unpack` are *deprecated*. Use
> `Bytes.pack` and `Bytes.unpack`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=a
let id_string (p : string) : string option =
  let packed: bytes = Bytes.pack p in
  (Bytes.unpack packed : string option)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=a
let id_string = (p : string) : option (string) => {
  let packed : bytes = Bytes.pack (p);
  (Bytes.unpack(packed) : option (string));
};
```

</Syntax>

