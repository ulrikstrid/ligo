(* Sock Collecting Contract

This contract implements a collectible virtual sock token (or v-sock for short).
You can think of it like baseball cards, but for socks. *)

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
