---
id: crypto-reference
title: Crypto
---

import Syntax from '@theme/Syntax';

The module `Crypto` gathers crypotographic primitives.

## BLAKE2b

Runs the [blake2b hash algorithm](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE2)
over the given `bytes` data and returns a `bytes` representing the hash.



<Syntax syntax="pascaligo">

```pascaligo group=crypto
function hasherman_blake (const s : bytes) : bytes is Crypto.blake2b (s)
```

> Note that `blake2b` is *deprecated*. Use `Crypto.blake2b`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=crypto
let hasherman_blake (s : bytes) : bytes = Crypto.blake2b s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=crypto
let hasherman_blake = (s : bytes) => Crypto.blake2b (s);
```

</Syntax>


## SHA256

Runs the [sha256 hash algorithm](https://en.wikipedia.org/wiki/SHA-2)
over the given `bytes` data and returns a `bytes` representing the
hash.


<Syntax syntax="pascaligo">

```pascaligo group=crypto
function hasherman (const s : bytes) : bytes is Crypto.sha256 (s)
```

> Note that `sha_256` is *deprecated*. Use `Crypto.sha256`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=crypto
let hasherman (s : bytes) : bytes = Crypto.sha256 s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=crypto
let hasherman = (s : bytes): bytes => Crypto.sha256 (s);
```

</Syntax>


## SHA512

Runs the [sha512 hash algorithm](https://en.wikipedia.org/wiki/SHA-2)
over the given `bytes` data and returns a `bytes` representing the
hash.



<Syntax syntax="pascaligo">

```pascaligo group=crypto
function hasherman512 (const s : bytes) : bytes is Crypto.sha512 (s)
```

> Note that `sha_256` is *deprecated*. Use `Crypto.sha256`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=crypto
let hasherman512 (s : bytes) : bytes = Crypto.sha512 s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=crypto
let hasherman512 = (s : bytes) => Crypto.sha512 (s);
```

</Syntax>


## Hashing Keys

It is often desirable to hash a public key. In Michelson, certain data
structures such as maps will not allow the use of the `key` type. Even
if this were not the case, hashes are much smaller than keys, and
storage on blockchains comes at a cost premium. You can hash keys with
a predefined functions returning a value of type `key_hash`.


<Syntax syntax="pascaligo">

```pascaligo group=b
function check_hash_key (const kh1 : key_hash; const k2 : key) : bool * key_hash is
  block {
    var kh2 : key_hash := Crypto.hash_key (k2)
  } with ((kh1 = kh2), kh2)
```

> Note that `hash_key` is *deprecated*. Use `Crypto.hash_key`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let check_hash_key (kh1, k2 : key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2
  in (kh1 = kh2), kh2
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let check_hash_key = ((kh1, k2) : (key_hash, key)) : (bool, key_hash) => {
  let kh2 : key_hash = Crypto.hash_key (k2);
  ((kh1 == kh2), kh2);
};
```

</Syntax>


## Checking a Signature

The predefined function `Crypto.check` checks that a given message has
been signed by a particular key.

> ⚠️ There is no way to *generate* a signed message in LIGO. This is
> because that would require storing a private key on chain, at which
> point it is not... private anymore.


<Syntax syntax="pascaligo">

```pascaligo group=crypto
function check_signature
    (const pk     : key;
     const signed : signature;
     const msg    : bytes) : bool
  is Crypto.check (pk, signed, msg)
```

> Note that `crypto_check` is *deprecated*. Use `Crypto.check`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=crypto
let check_signature (pk, signed, msg : key * signature * bytes) : bool =
  Crypto.check pk signed msg
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=crypto
let check_signature =
  ((pk, signed, msg) : (key, signature, bytes)) : bool =>
  Crypto.check (pk, signed, msg);
```

</Syntax>

