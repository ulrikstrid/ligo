(* Sock Collecting Contract

This contract implements a collectible virtual sock token (or v-sock for short).
You can think of it like baseball cards, but for socks. *)

(* Sock Type ID *)

type st_id = nat

type sock_type = {
  name: string;
  description_hash: string;
}

type sock_types = (st_id, sock_type) big_map

type socks = (address * st_id, nat) big_map

(* Drop Table

One of the trickier aspects of smart contract design is figuring out which parts
of the consensus need to be enforced on-chain and which parts of the consensus
can be softer. Naively, we can imagine 4 tiers of assurance:

Norm - No formal assurance, but everyone understands they should interpret something
a certain way. For example in a trading card game the actual rules are probably
not represented on-chain. Instead, clients and servers are expected to follow a
social norm of implementing the game rules as they're generally understood. The
on-chain representation only exists to keep consensus as to who owns what game
tokens.

Suggested Norm - No formal enforcement, but a clear suggestion of the norm people
should use to interpret something is stored in the contract or on-chain where it
is clearly privileged. Your trading card game might keep a link to the canonical
game rules somewhere in its contract storage.

Designated Authority - Consensus is assured by appointing an oracular party who
provides rulings or interpretations within a set of defined rules. If you
represented tournaments for your trading card game on Tezos, you might have rule
disputes and records of who wins a particular match recorded by human judges.
These judges would have a designated address from which they would provide
'oracular' off-chain knowledge to the contract.

Encoded Rule - All aspects of the consensus are represented on-chain and follow
deterministic rules in response to certain inputs or states. This would be if you
implemented the entire ruleset for your game on Tezos and didn't need human judges
to determine who wins a match.

So which of these approaches should we take for the sock drop table? Norm and
Suggested Norm are systems that can work, but if we're taking that approach we
don't really need a blockchain in the first place. That leaves us with a choice
between Designated Authority and Encoded Rule. Because blockchains need to be
deterministic, it's not really possible to implement randomness as a pure Encoded
Rule system. You need some kind of off-chain input to make things random. For example,
one idea is to have a secondary contract where multiple people contribute "random"
bytes to a list where they are then XOR'd together. The hope is that if enough
people contribute and they all hash-commit their inputs, it becomes impossible to
control the final result. This approach however has lots of problems. For one
thing it's vulnerable to sybil attacks where one person makes many identities
and coordinates them to manipulate the result of the XOR. This means that any
random number generator based on that design becomes increasingly complicated as
you try to design out the inherent flaw of sybil attacks.

Another approach which is pure Encoded Rule is to use a pseudo-random number
generator to output the collectibles, and then let people buy them at a defined
price. This is kind of lame though, taking the random element that people enjoy
out of collectible games.

Then finally there's the approach we'll use, which is to have a central authority
issue socks based on some off-chain process of random number generation. This might
intuitively seem to violate the premise of having our socks on the blockchain, but
it really doesn't. We want our v-sock *ownership* to be an Encoded Rule, but the
actual initial generation of v-socks is fine as a Designated Authority. Remember
in current Trading Card Games, a Designated Authority decidingt the rarity of game
tokens is the norm. The authors of current TCG's could tank the value of any given
card, or make a card more rare and increase its value arbitrarily.

*)

type storage = {
  sock_oracle: address;
  socks: socks;
  sock_types: sock_types;
  stock_price: tez;
}

(* This contract implements FA2, see:
   https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-12/tzip-12.md
*)

type transfer = {
  from_ : address;
  to_ : address;
  token_id : st_id;
  amount : nat;
}


type parameter =
| Add_Sock_Type of st_id * sock_type
| Update_Sock_Type of st_id * sock_type
| Add_Sock of st_id
| Transfer of transfer list

let add_sock_type (new_st, storage: (st_id * sock_type)  * storage) :
  operation list * storage =
  (* Sock Oracle check *)
  if Tezos.sender <> storage.sock_oracle
  then (failwith "You are not the sock oracle.": operation list * storage)
  else
  let st_id, sock_type = new_st in
  (* Make sure we're not overwriting an existing sock type *)
  match Big_map.find_opt st_id storage.sock_types with
  | Some st -> (failwith "A sock type with this ID already exists.":
                  operation list * storage)
  | None ->
    let updated_sock_types = Big_map.update st_id
                               (Some sock_type)
                               storage.sock_types
    in
    ([]: operation list), {storage with sock_types = updated_sock_types;}

let update_sock_type (st_update, storage: (st_id * sock_type) * storage) :
  operation list * storage =
  (* Sock Oracle check *)
  if Tezos.sender <> storage.sock_oracle
  then (failwith "You are not the sock oracle.": operation list * storage)
  else
  let st_id, sock_type = st_update in
  let updated_sock_types = Big_map.update st_id
                               (Some sock_type)
                               storage.sock_types
  in
  ([]: operation list), {storage with sock_types = updated_sock_types;}


let add_sock (st_id, storage: st_id * storage) :
  operation list * storage =
  (* Sock Oracle check *)
  if Tezos.sender <> storage.sock_oracle
  then (failwith "You are not the sock oracle.": operation list * storage)
  else
  let sid = (storage.sock_oracle, st_id) in
  let current_stock =
    match Big_map.find_opt sid storage.socks with
    | Some sc -> sc
    | None -> 0n
  in
  let updated_stock = Big_map.update sid
                      (Some (current_stock + 1n))
                      storage.socks
  in ([]: operation list), {storage with socks = updated_stock;}


let buy_stock (st_id, storage: st_id * storage ) :
  operation list * storage =
  let sid = (storage.sock_oracle, st_id) in
  let stock_count : nat =
    match Big_map.find_opt sid storage.socks with
    | Some sock_c -> sock_c
    | None -> (failwith "This sock is not in the oracle's stock (Tip: Wrong ID?).": nat)
  in
  (* Sock is stock check *)
  if stock_count < 1n
  then (failwith "This sock is not in the oracle's stock (Tip: Out Of Stock).":
         operation list * storage)
  else
  if Tezos.amount < storage.stock_price
  then (failwith "You paid too little for your sock.": operation list * storage)
  else
  let updated_socks = Big_map.update sid (Some (abs (stock_count - 1n))) storage.socks in
  (* We use the updated socks because the oracle might buy from itself *)
  let buyer_sock_count : nat =
    match Big_map.find_opt (Tezos.sender, st_id) updated_socks with
    | Some sc -> sc
    | None -> 0n
  in
  let updated_socks = Big_map.update
      (Tezos.sender, st_id)
      (Some (buyer_sock_count + 1n))
      updated_socks in
  ([]: operation list), {storage with socks = updated_socks;}

(* Helper function for the transfer operation *)
let t_process_one (storage, transfer_info: storage * transfer) : storage =
  let from_sid = (Tezos.sender, transfer_info.token_id) in
  let to_sid = (transfer_info.to_, transfer_info.token_id) in
  let from_sock_count =
    match Big_map.find_opt from_sid storage.socks with
    | Some sc -> sc
    | None -> (failwith "You don't own a sock of that type.": nat)
  in
  if (from_sock_count - (transfer_info.amount)) < 0
  then (failwith "Attempted to transfer more socks of that type than you have.":
          storage)
  else
  let to_sock_count =
    match Big_map.find_opt to_sid storage.socks with
    | Some sc -> sc
    | None -> 0n
  in
  let updated_socks =
    Big_map.update
      from_sid
      (Some (abs (from_sock_count - transfer_info.amount)))
      storage.socks
  in
  let updated_socks =
    Big_map.update
      to_sid
      (Some (to_sock_count + transfer_info.amount))
      updated_socks
  in {storage with socks = updated_socks;}

let transfer (transfers, s: transfer list * storage) : operation list * storage =
  ([]: operation list), List.fold t_process_one transfers s

let main (p,s: parameter * storage) : operation list * storage =
  match p with
  | Add_Sock_Type ast -> add_sock_type (ast,s)
  | Update_Sock_Type ust -> update_sock_type (ust,s)
  | Add_Sock a -> add_sock (a,s)
  | Transfer t -> transfer (t,s)
