---
id: crypto-reference
title: Crypto — Cryptographic functions
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

## Crypto.blake2b(data: bytes): bytes

Runs the [blake2b hash algorithm](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE2)
over the given `bytes` data and returns a `bytes` representing the hash.

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
function hasherman_blake (const s: bytes) : bytes is blake2b(s)
```

</TabItem>
<TabItem value="cameligo">

```cameligo
let hasherman_blake (s: bytes) : bytes = Crypto.blake2b s
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
let hasherman_blake = (s: bytes) => Crypto.blake2b(s);
```

</TabItem>
</Tabs>

## Crypto.sha256(data: bytes) : bytes

Runs the [sha256 hash algorithm](https://en.wikipedia.org/wiki/SHA-2) over the given
`bytes` data and returns a `bytes` representing the hash.

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
function hasherman (const s : bytes) : bytes is
  begin skip end with sha_256(s)
```

</TabItem>
<TabItem value="cameligo">

```cameligo
let hasherman (s : bytes) : bytes =
   Crypto.sha256 s
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
let hasherman = (s: bytes): bytes => Crypto.sha256(s);
```

</TabItem>
</Tabs>

## Crypto.sha512(data: bytes) : bytes

Runs the [sha512 hash algorithm](https://en.wikipedia.org/wiki/SHA-2) over the given
`bytes` data and returns a `bytes` representing the hash.

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
function hasherman512 (const s: bytes) : bytes is sha_512(s)
```

</TabItem>
<TabItem value="cameligo">

```cameligo
let hasherman512 (s: bytes) : bytes = Crypto.sha512 s
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
let hasherman512 = (s: bytes) => Crypto.sha512(s);
```

</TabItem>
</Tabs>

## Crypto.hash_key(k: key) : key_hash

Hashes a key for easy comparison and storage.

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
function check_hash_key (const kh1 : key_hash; const k2 : key) : bool * key_hash is block {
  var ret : bool := False ;
  var kh2 : key_hash := crypto_hash_key(k2) ;
  if kh1 = kh2 then ret := True else skip; 
} with (ret, kh2)
```

</TabItem>
<TabItem value="cameligo">

```cameligo
let check_hash_key (kh1, k2: key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2 in
  if kh1 = kh2
  then (true, kh2)
  else (false, kh2)
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
let check_hash_key = ((kh1, k2): (key_hash, key)) : (bool, key_hash) => {
  let kh2 : key_hash = Crypto.hash_key(k2);
  if (kh1 == kh2) {
    (true, kh2);
  }
  else {
    (false, kh2);
  }
};
```

</TabItem>
</Tabs>

## Crypto.check(pk: key, signed: signature, data: bytes) : bool

Check that a message has been signed by a particular key.

> ⚠️ There is no way to *generate* a signed message in LIGO. This is because that would require storing a private key on chain, at which point it isn't very private anymore.

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
function check_signature
    (const pk: key;
     const signed: signature;
     const msg: bytes) : bool
  is crypto_check(pk, signed, msg)
```

</TabItem>
<TabItem value="cameligo">

```cameligo
let check_signature (pk, signed, msg: key * signature * bytes) : bool =
  Crypto.check pk signed msg
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
let check_signature = ((pk, signed, msg): (key, signature, bytes)) : bool => {
  Crypto.check(pk, signed, msg);
};
```

</TabItem>
</Tabs>
