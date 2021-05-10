# Smart contract security
In this article, we will cover the basics of Tezos smart contract security. We will describe several potential vulnerabilities that stem from developers' misconceptions about the distributed nature of blockchains. We will also suggest ways to protect your contracts against these kinds of attacks.

## Resource constraints

Tezos limits the resources available to the contracts. It bounds operations size so that nodes can broadcast the operations over the network in a reasonable time. It also places a limit on the computations the bakers need to perform to validate an operation – the **gas limit.** When you develop your contract, you need to bear these limits in mind.

Let's look at a seemingly innocent wallet contract that stores an event log:
```
type parameter =
  Fund
| Send of address * tez

type transaction =
  Incoming of address * tez
| Outgoing of address * tez

type storage = {
  owner : address;
  transactionLog : transaction list
}

let send (dst, amount_ : address * tez) =
  let callee : unit contract option =
    Tezos.get_contract_opt dst in
  match callee with
  | Some contract ->
      let op = Tezos.transaction () amount_ contract in
      Outgoing (dst, amount_), [op]
  | None ->
    (failwith "Could not send tokens" : transaction * operation list)

let receive (from, amount_ : address * tez) =
  Incoming (from, amount_), ([] : operation list)

let main (p, s : parameter * storage) =
  let tx, ops = match p with
  | Fund -> receive (Tezos.sender, Tezos.amount)
  | Send args ->
    let u = assert (Tezos.sender = s.owner && Tezos.amount = 0tez) in
    send args
  in ops, {s with transactionLog = tx :: s.transactionLog}
```

This contract:
1. Can receive funds sent to it via the `Fund` entrypoint.
2. Can send some tez via the `Send` entrypoint callable by the owner.
3. Stores a log of all the operations.

What can go wrong? To answer this question, we'll need to dive a bit into how Tezos processes transactions and what limits it places on them.

To guarantee that the nodes spend reasonable time processing transactions, Tezos requires that the execution consumes no more than a certain amount of _gas_ (in the current protocol, it's 1 040 000 gas units).

But in Tezos, the amount of gas consumed depends on the size of the storage! All non-lazy (i.e. non-BigMap) storage entries get fetched, deserialized, and typechecked upon each contract invocation. It means that:
1. Our contract will be more and more expensive to call with every transaction made.
2. Eventually, when the gas consumption is too high, every transaction will hit the upper bound, which will render the contract unusable.

To avoid it in this particular case, you should store the logs in a big map, e.g., indexed incrementally.

Generally, you need to think about whether the side effect of gas consumption can halt the execution prematurely. Here are the tips that can help you reduce the risk of potential gas exhaustion.
1. Limit the size of non-lazy storage:
   - Do not store data extendable by the users (e.g., event logs, a set of token holders) in non-lazy containers.
   - If using non-lazy containers is absolutely required, place an upper bound on the size of non-lazy containers.
   - Limit the maximum size of strings and byte strings.
   - Do not put untrusted lambdas in storage.
2. Ensure that your contract logic does not allow attackers to increase the interpretation cost, e.g., by forcing future transactions to run a huge loop.

## Transaction ordering
It is crucial to understand that all blockchains, including Tezos, are distributed systems where block producers – bakers in Tezos – are free to include, censor, and reorder transactions within a block. For most of the practical applications, this does not pose a threat. However, in some cases, especially in Decentralized Finance (DeFi) applications, bakers can use their power to gain economic benefit from reordering or censoring out user transactions.

A classic example of a system vulnerable to this kind of attacks is a decentralized exchange with an on-chain orderbook, like this one (let's assume just one asset pair for clarity):

```
type order = {
  price : nat;
  volume : nat
}

type storage = {
    bids : order list;
    asks : order list
}

type parameter =
  Buy of order
| Sell of order

let buy (order : order) = ...
let sell (order : order) = ...
let main (p, s : parameter * storage) = ...
```

A baker may notice some transaction, for example, a request to buy some big volume of asset. The baker may then _front-run_ this transaction and, anticipating the price going up, insert a _buy_ order at the current price before the trader's transaction. He can then benefit from the price change by selling the asset at a higher price.

In fact, the so-called _miner extracted value_ [poses a big risk](https://arxiv.org/pdf/1904.05234.pdf) to security of blockchains in general. You should avoid letting miners get rewards from transaction ordering. In this particular case, moving the order book off-chain would be a good option.

## Timestamps

Aside from transaction ordering, bakers can manipulate other variables you might want to rely on. A classic example of such a value is block timestamp (`Tezos.now`). Since Tezos is a distributed system, there is no way to make sure the block was produced _exactly_ at the specified timestamp. Other nodes can only check that the block timestamp is within the reasonable bounds: it is not set to a time _before_ the latest known block, and it is not too far away in the future.

Assuming an operation goes through _roughly about_ the specified timestamp is fine for most of the practical applications. However, in some cases, bakers can slightly modify the timestamp to gain economic benefit.

## Reentrancy and call injection

Tezos features a rather unconventional model of execution:
1. The contract state is updated _after_ the computations are completed.
2. The contracts cannot emit operations in the middle of execution.
3. Internal operations are _queued._

The first two points resemble the Checks-Effects-Interactions pattern popular in Solidity. In Ethereum, it is considered a best practice, and Tezos enforces this on the protocol level. Such restrictions help  prevent reentrancy attacks: if the state of your contract is updated _before_ someone makes a reentrant call, this call would be treated as a regular one and should do no harm.

Consider the following snippet in Solidity:
```
function withdraw(uint256 amount) {
  uint256 balance = balances[beneficiary];
  require(balance >= amount);
  uint256 new_balance = balance - amount;
  beneficiary.call.value(amount)();
  balances[beneficiary] = new_balance;
}
```

You may notice that the _effect_ of updating the storage happens after _interaction_ – transferring the `amount` to the beneficiary. This contract has a reentrancy vulnerability: the contract execution would get paused during the transfer, and the beneficiary can call `withdraw` again _before_ their balance is updated.

It is quite hard to repeat this attack on Tezos, where the contract storage is always updated _before_ any interactions:

```
type storage = {
    beneficiary : address;
    balances : (address, tez) map
}

type parameter = tez * (unit contract)

let withdraw (param, s : parameter * storage) =
  let amount_, beneficiary = param in
  let beneficiary_addr = Tezos.address beneficiary in
  let balance_ =
    match (Map.find_opt beneficiary_addr s.balances) with
    | Some v -> v
    | None -> 0tez
    in
  let new_balance =
    if (balance_ >= amount_)
    then balance_ - amount_
    else (failwith "Insufficient balance" : tez)
    in
  let op = 
    Tezos.transaction () amount_ beneficiary in
  let new_balances =
    Map.update beneficiary_addr (Some new_balance) s.balances
    in
  [op], { s with balances = new_balances }
```

Notice that the code flow is similar: we first check whether the beneficiary has enough balance, then forge an operation that sends the money, and finally we update the balances mapping. The difference is that in Tezos the operations are not executed immediately: we store the operation and later return it as a result of the entrypoint. Hence, the balances are updated by the time the operation is executed, so the reentrancy attack is mitigated.

However, in some cases reentrancy attacks are still possible, especially if contracts are supposed to "wait" for a callback in an indeterminate state. If you, for example, choose to store balances in a separate contract, your execution flow will need a lot more interactions than sending one internal operation:

| Current call | Treasury state after | Queued operations |
|--------------|----------------------|-------------------|
| `Treasury %withdraw`   | Waiting for balances | [`Balances %getBalance`] |
| `Balances %getBalance` | Waiting for balances | [`Treasury %withdrawContinuation`] |
| `Treasury %withdrawContinuation` | Sent | [Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| Send tez to `Beneficiary` | Sent | [`Balances %setNewBalance`] |
| `Balances %setNewBalance` | Sent | |

In this example, the Treasury contract uses a callback mechanism to get the sender balance. In an intermediate state between `%withdraw` and `%withdrawContinuation`, the balances request has already been sent but the funds have not been withdrawn yet, and the balances have not been updated. This opens up a possibility for a call injection attack.

For example, let's see what happens if an attacker tries to call `%withdraw` twice within a single transaction:

| Step | Current call | Queued operations |
|------|--------------|-------------------|
| 1 | `Evil %attack` | [`Treasury %withdraw`, `Treasury %withdraw`] |
| 2 | `Treasury %withdraw`  | [`Balances %getBalance`] |
| 3 | `Treasury %withdraw`  | [`Balances %getBalance`, `Balances %getBalance`] |
| 4 | `Balances %getBalance`| [`Balances %getBalance`, `Treasury %withdrawContinuation`] |
| 5 | `Balances %getBalance`| [`Treasury %withdrawContinuation`, `Treasury %withdrawContinuation`] |
| 6 | `Treasury %withdrawContinuation` | [`Treasury %withdrawContinuation`, Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| 7 | `Treasury %withdrawContinuation` | [Send tez to `Beneficiary`, `Balances %setNewBalance`, Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| 8 | Send tez to `Beneficiary` | [`Balances %setNewBalance`, Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| 9 | `Balances %setNewBalance` | [Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| 10 | Send tez to `Beneficiary` | [`Balances %setNewBalance`] |
| 11 | `Balances %setNewBalance` | |

The attacker successfully withdraws money twice using the fact that by the time the second `%withdraw` is called, the balance has not been updated yet.

## Transactions to untrusted contracts

When emitting a transaction to an untrusted contract, you can not assume that it will "play by the rules". Rather, you should always bear in mind that the callee may fail, causing the entire operation to fail, or emit other operations you don't expect.

Let's consider the following example:
```
type storage = {
  owner : address;
  beneficiaries : address list
}

let send_rewards (beneficiary_addr : address) =
  let maybe_contract : unit contract option =
    Tezos.get_contract_opt beneficiary_addr in
  let beneficiary =
    match maybe_contract with
    | Some contract -> contract
    | None -> (failwith "CONTRACT_NOT_FOUND" : unit contract) in
  Tezos.transaction () 5tez beneficiary

let main (p, s : unit * storage) =
  if Tezos.sender <> s.owner
  then (failwith "ACCESS_DENIED" : operation list * storage)
  else
    let ops = List.map send_rewards s.beneficiaries
    in ops, s
```

The contract emits a bunch of operations that transfer 5 tez to each of the beneficiaries listed in storage. The flaw here is that one of the receiver contracts may fail, preventing others from receiving the reward. This may be intentional censorship or a bug in the receiver contract – in either case, the contract gets stuck.

Instead of making a batch transfer, it is better to let beneficiaries withdraw their funds individually. This way, if the receiver contract fails, it would not affect other withdrawals.

## Incorrect authorization checks

When developing a contract, you may often want to restrict access to certain entrypoint. You need to somehow ensure that:
1. The request comes from an authorized entity
2. This entity can't be tricked into sending this request.

You may be tempted to use `Tezos.source` instruction – it returns the address of an implicit account who injected the operation – but this violates our second requirement. It is easy to ask the owner of this implicit account to make a seemingly innocent transfer to a malicious contract that, in turn, emits an operation to a restricted entrypoint. The attacker contract may disguise itself as some blockchain game or a DAO, but neither the caller would be aware of its side-effects nor the callee would notice the presence of the intermediary. You should **never** use `Tezos.source` for authorization purposes.

Checking whether `Tezos.sender` – the address of the immediate caller – is authorized to perform an operation is better: since the request comes directly from the authorized entity, we can be more certain this call is intended. Such an approach is a decent default choice if both conditions hold true:
1. The sender contract is well secured against emitting arbitrary operations. For instance, it must not contain ["view" entrypoints](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints) as defined in [TZIP-4](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md).
2. You only need to authorize an immediate caller and not the contracts somewhere up in the call chain.

If any of these conditions is not met, you need to use a more advanced technique called "tickets". Tickets are much like "contract signatures": a contract may issue a ticket that authorizes a certain action. A ticket holds the data of any type, and a number – ticket _amount_. A ticket can not be copied but it can be split. If you split a ticket of amount `N`, you'd get two tickets with amounts `M` and `K` such that `N = M + K`. You can also join two tickets if they have the same data and are issued by the same contract. In this case, you'd get a new ticket with the sum of the amounts.

To check whether an action is authorized, you need to see if the ticket meets the following conditions:
1. The ticket issuer has enough permissions to perform this action.
2. The ticket amount and data are correct (the definition of "correct" is application-specific, e.g., the amount may mean the number of tokens to spend or the number of _times_ the action can be executed).

We recommend using the sender-based authorization only in simple scenarios, e.g., when the contract has a single "owner" contract controlled by an implicit account. Otherwise, it's better to use ticket-based authorization.
