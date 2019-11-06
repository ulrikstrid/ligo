(**

This contract represents a commitment to a particular hashed value.
The intended use is that in a contract between N parties, where each one has to
commit to a particular action before execution can continue, this contract can
be used as a mechanism to obscure what actions the other parties have chosen
while still committing them to a specific action.

That's a mouthful, so lets consider a hypothetical Rock-Paper-Scissors contract.
Rock-Paper-Scissors is a simple game for two players (N=2) where each player
picks from one of the three titular rock, paper, or scissors. Unless both
players pick the same option, one player loses and one player wins. That makes
it a useful illustration because the number of choices is small, there is only
one turn, and the rules are simple.

Rock beats scissors, paper beats rock, scissors beat paper. For an implementation
played over a blockchain, having to reveal your move upfront would defeat the
entire purpose of the game. The second player to move would always win, because
they can look at the first players choice and choose the winning move. This can
be prevented by having each player hash their move along with a salt and provide
these hashes to the network before the outcome is decided. That's what this contract
is for, it lets you get each party to _commit_ to a hash, and then _reveal_ their
submitted value. The contract will also handle the logic of deciding whether the
revealed value is a valid match for the commitment hash.

The actual use of the contract can also be explained with Rock-Paper-Scissors.
For that contract, the game would generate two instances of this contract; one
for each player. It would then submit an action hash (presumably from some client
external to the blockchain) from each player to their instance of this contract.
Once stored, this contract would call the caller with a specified entrypoint. The
caller then submits the revealed data from each client, and this contract tells
the caller whether it matches the commitment hash.

*)

type storage =
| Non_committed of unit
| Committed of bytes
| Revealed of unit
