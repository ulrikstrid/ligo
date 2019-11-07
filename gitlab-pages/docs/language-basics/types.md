---
id: types
title: Types
---

LIGO is strongly and statically typed, which means that the compiler checks your program at compilation time and makes sure there won't be any type related runtime errors. The language itself features types built on top of Michelson's type system.

## Type aliases

Type aliasing is a great choice when working towards a readable / maintainable smart contract. One well typed type/variable is worth a thousand words. For example we can choose to *alias* a string, as an animal breed - this will allow us to comunicate our intent with added clarity.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type animalBreed is string;
const dogBreed : animalBreed = "Saluki";
```

<!--Cameligo-->

```cameligo
type animal_breed = string
let dog_breed: animal_breed = "Saluki"
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Simple types
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
// accountBalances is a simple type, a map of address <-> tez
type accountBalances is map(address, tez);

const ledger: accountBalances = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> 10mutez
end
```

<!--Cameligo-->
```cameligo
// account_balances is a simple type, a map of address <-> tez
type account_balances is (address, tez) map

let ledger: account_balances = Map.literal
  [(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address), 10mutez)]
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Composed types

Often our contracts will require complex data structures, which will in turn require a well-typed storage, or functions to work with. LIGO offers a simple way to compose simple types, into larger & more expressive composed types.

In the example below you can see definition of data types for a ledger, that keeps a balance & number of previous transactions for a given account.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
// alias two types
type account is address;
type numberOfTransactions is nat;
// accountData consists of a record with two fields (balance, numberOfTransactions)
type accountData is record
    balance: tez;
    numberOfTransactions: numberOfTransactions;
end
// our ledger / accountBalances is a map of account <-> accountData
type accountBalances is map(account, accountData);

// pseudo-JSON representation of our map
// { "tz1...": {balance: 10mutez, numberOfTransactions: 5n} }
const ledger: accountBalances = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> record
      balance = 10mutez;
      numberOfTransactions = 5n;
    end
end
```

<!--Cameligo-->
```cameligo
(* alias two types *)
type account = address
type number_of_transactions = nat
(* account_data consists of a record with two fields (balance, number_of_transactions) *)
type account_data = {
 balance: tez;
 number_of_transactions: number_of_transactions;
}
(* our ledger / account_balances is a map of account <-> account_data *)
type account_balances = (account, account_data) map

// pseudo-JSON representation of our map
// {"tz1...": {balance: 10mutez, number_of_transactions: 5n}}
let ledger: account_balances = Map.literal
  [(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address),
    {balance = 10mutez;
     number_of_transactions = 5n;}
   )]
```

<!--END_DOCUSAURUS_CODE_TABS-->


---

## 🛠 Exercises

### #1 Defining a voting contract storage type

Best way to learn, is by practice, so let's try to define a new type called `voting_storage`,
which will represent the storage of our contract. Let's pretend we're running a presidential election on the chain, our storage needs to be a `map` of candidates, where the map key is an `address` of a candidate, and the value associated in this map is a `record`, which holds two `nat`(ural numbers) representing the number of votes, where voters can vote for `yay` or `nay` for the respective candidate.

#### Advanced

To spice things up, try using a `tuple` instead of a record, where the first element represents a *yay* vote, and the second element a *nay* vote.
