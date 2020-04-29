(* people hash-commit to bytes they say will be random in round 1
   in round 2, they reveal
   in round 3, you xor all the revealed bytes
   if at least one actor is honest, the thing is truly random
   anyone can participate in round1
   if you need more randomness, you can keep hashing
   if you need new randomness, you can trigger a new game
*)

type commits = (address, bytes) map

type reveals = (address, nat) map

type parameter =
| Commit of bytes
| Reveal of nat
| Result of unit

type storage = {
  commits: commits;
  reveals: reveals;
  end_phase_one: timestamp;
  end_phase_two: timestamp;
  min_players: nat;
}

(* We use a 2 ** 256 nat for the random data space because it's not possible to
   XOR bytes in Michelson yet :( *)
let random_space : nat = 115792089237316195423570985008687907853269984665640564039457584007913129639936n


(* Phase 1 *)

let commit (p,s: bytes * storage) =
  let s =
    if s.end_phase_one > Current.time
    then {s with commits = Map.update Current.sender (Some p) s.commits }
    else (failwith "This game has passed the commit stage.": storage)
  in ([]: operation list), s

(* Phase 2 *)

let reveal (p,s: nat * storage) =
  if (Current.time > s.end_phase_one) && (Current.time < s.end_phase_two)
  then
  if p > random_space
  then (failwith "Your commitment was over the bound of 115792089237316195423570985008687907853269984665640564039457584007913129639936":
          operation list * storage)
  else
    let hashed : bytes =
      match Map.find_opt Current.sender s.commits with
      | Some hashed -> hashed
      | None -> (failwith "You didn't commit a hash in the 1st round.": bytes)
    in
    let updated_reveals : (address, nat) map =
      if (Crypto.sha256 (Bytes.pack p)) = hashed
      then Map.update Current.sender (Some p) s.reveals
      else (failwith "The submitted bytes don't match your commitment.":
              (address, nat) map)
    in
    let updated_storage : storage =
      {s with reveals = updated_reveals }
    in ([]: operation list), updated_storage
  else (failwith "This game is not in the reveal stage.": operation list * storage)

(* Phase 3 

XOR together the revealed bytes to create a final 'random' value that can be consumed.

*)
let folding_xor (accumulator, kv: nat * (address * nat)) : nat =
  Bitwise.lxor accumulator kv.1

let result (p,s: unit * storage) =
  if Current.time > s.end_phase_two
  then
    if Map.size s.reveals >= s.min_players
    then
      let xor_entries = Map.fold folding_xor s.reveals 0n in
      let result = Crypto.sha256 (Bytes.pack xor_entries) in
      let caller : bytes contract = Operation.get_contract Current.sender in
      let return_ops : operation = Operation.transaction result 0mutez caller in
      [return_ops], s
    else ((failwith "Minimum player threshold not met for this game."): operation list * storage)
  else ((failwith "The reveal stage has not finished yet."): (operation list * storage))

let main (p,s: parameter * storage) =
  match p with
  | Commit c -> commit (c, s)
  | Reveal r -> reveal (r, s)
  | Result -> result ((), s)
