(* people hash-commit to bytes they say will be random in round 1
   in round 2, they reveal
   in round 3, you xor all the revealed bytes
   if at least one actor is honest, the thing is truly random
   anyone can participate in round1
   if you need more randomness, you can keep hashing
   if you need new randomness, you can trigger a new game
*)

type commits = (address, bytes) map

type reveals = (address, bytes) map

type storage = {
  commits: commits;
  reveals: reveals;
  end_phase_one: timestamp;
  end_phase_two: timestamp;
  min_players: int;
}

(* Phase 1 *)

let commit (p,s: bytes * storage) =
  let s =
    if s.end_phase_one > Current.time
    then {commits = Map.update Current.sender p s.commits;
          reveals = reveals;
          end_phase_one = s.end_phase_one;
          end_phase_two = s.end_phase_two;
          min_players: s.min_players;
         }
    else (failwith "This game has passed the commit stage.": storage)
  in ([]: operation list), s

(* Phase 2 *)

let reveal (p,s: bytes * storage) =
  if (Current.time > s.end_phase_one) && (Current.time < s.end_phase_two)
  then
    let hashed : bytes =
      match Map.find_opt Current.sender s.commits with
      | Some hashed -> hashed
      | None -> (failwith "You didn't commit a hash in the 1st round.": bytes)
    in
    let updated_reveals : (address, bytes) map =
      if (Crypto.sha256 p) = hashed
      then Map.update Current.sender p s.reveals
      else (failwith "The submitted bytes don't match your commitment.":
              (address, bytes) map)
    in
    let updated_storage : storage =
      {commits = s.commits;
       reveals = updated_reveals;
       end_phase_one = s.end_phase_one;
       end_phase_two = s.end_phase_two;
       min_players = s.min_players;
      }
    in ([]: operation list), updated_storage
  else (failwith "This game is not in the reveal stage.": operation list * storage)

(* Phase 3 

XOR together the revealed bytes to create a final 'random' value that can be consumed.

*)
let result (p,s: bytes * storage) =
  if Current.time > s.end_phase_two
  then
    if Map.size s.reveals >= s.min_players
    then
      let folding_xor (accumulator, kv: bytes * (address * bytes)) : bytes =
        Bitwise.lxor accumulator kv.1
      (* TODO: Figure out how many bytes we want in revealed bytes. *)
      in Map.fold folding_xor s.reveals 0x00
      
    else (failwith "Minimum player threshold not met for this game.")
  else (failwith "The reveal stage has not finished yet.")
