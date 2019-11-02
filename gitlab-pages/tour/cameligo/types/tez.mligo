(**

It would be difficult to write a smart contract without some way to refer to
tokens. LIGO has a mutez token type. A `mutez` is a millionth of a Tezo. The
`tz` type is syntactic sugar if you would like to denominate in full tezos.
`mutez` is a bounded integer, so the `tz` will not lose track of things like a
float would.

*)

let add_tez : tez = 21mutez + 0.000021tz
let sub_tez : tez = 0.000021tz - 0.000020tz
let not_enough_tez : tez = 4611686018427.387903tz

let add_more_tez : tez = 100tz + 10tz + 1tz + 0.1tz + 0.01tz + 0.001tz
