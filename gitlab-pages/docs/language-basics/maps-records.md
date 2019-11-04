---
id: maps-records
title: Maps, Records
---

So far we've seen pretty basic data types, however LIGO offers a bit more in terms of built-in constructs, such as Maps and Records.

## Maps

Maps are natively available in Michelson, and LIGO builds on top of them. A strong requirement for a Map is that it's keys need to be of the same type, and that type must be comparable.

Here's how a custom map type is defined:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type ledger is map(address, tez);
```
<!--END_DOCUSAURUS_CODE_TABS-->

<br/>
And here's how a map value is populated:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo
const ledger: ledger = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> 1000mtz;
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) -> 2000mtz;
end
```
<br/>
> Notice the `->` between the key an it's value and `;` to separate individual map entries.
>
> `("<string value>": address)` means that we type-cast a string into an address.

<!--END_DOCUSAURUS_CODE_TABS-->

### Accessing map values by key

If we want to access a balance from our ledger above, we can use the `[]` operator/accessor to read the associated `tez` value. However, the value we'll get will be wrapped as an optional, so in our case `option(tez)`, here's an example:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const balance: option(tez) = ledger[("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)];
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Records

Records are a construct introduced in LIGO, and are not natively available in Michelson. The LIGO compiler translates records into Michelson `Pairs`.

Here's how a custom record type is defined:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type user is record 
    id: nat;
    is_admin: bool;
    name: string;
end
```
<!--END_DOCUSAURUS_CODE_TABS-->

<br/>
And here's how a record value is populated:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const user: user = record
    id = 1n;
    is_admin = True;
    name = "Alice";
end
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Accessing record keys by name

If we want to obtain a value from a record for a given key, we can do the following:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const is_admin: bool = user.is_admin;
```
<!--END_DOCUSAURUS_CODE_TABS-->


---

## 🛠 Excercise

### #1 Building a simple ledger

You should now be able to build a simple token ledger. The task is to build a ledger, that holds a map of `address -> ledger_entry_map` where `ledger_entry_map` is a `map(string, ledger_entry)` where the key in this map is a `token_id` representing a token name, e.g. `XTZ`. While `ledger_entry` itself is just a `nat` balance.

Second task is to implement a function `get_balance`, that returns a `nat` balance when given an `address` and a `token_id`.


