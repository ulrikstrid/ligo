(* type definitions *)
type parameter is Back | Claim | Withdraw

type storage is
  record
    owner    : address;
    target   : tez;
    deadline : timestamp;
    backers  : map (address, tez);
    funded   : bool
  end

type return is list (operation) * storage

(* utility functions *)

function tezos_get_contract (var addr : address) : contract(unit) is
    case (Tezos.get_contract_opt (addr) : option (contract (unit))) of
      | Some (c) -> c
      | None -> (failwith("Could not get the sender contract."): (contract (unit)))
    end

function get_backer_balance (var addr : address ; var m : map(address,tez)) : tez is
    case Map.find_opt (addr,m) of
      | Some (t) -> t
      | None -> (failwith("Could not get the backer balance."): tez)
    end

(*
  add funds to the contract before the deadline is reached
*)
function back (var action : unit; var store : storage) : return is
  begin
    if Tezos.now > store.deadline then failwith ("Deadline passed.") else skip ;
    case store.backers[sender] of
      | None -> store.backers[sender] := Tezos.amount
      | Some (x) -> skip
    end
  end with ((nil : list (operation)), store)

(*
  refund if the deadline is reached the contract is not funded
*)
function claim (var action : unit; var store : storage) : return is
  begin
    if Tezos.now <= store.deadline                   then failwith ("Too early.")                 else skip ;
    if Tezos.balance >= store.target or store.funded then failwith ("Target reached: no refund.") else skip ;
    if not Map.mem(sender, store.backers)            then failwith ("Not a backer.")              else skip ;
    const asset : tez             = get_backer_balance (sender, store.backers) ;
    const dest  : contract (unit) = tezos_get_contract (sender);
    const op    : operation       = Tezos.transaction (Unit, asset, dest);
    remove sender from map store.backers
  end with (list [op], store)

(*
  withdraw the contract balance when the deadline is reached and the target reached
*)
function withdraw (var action : unit; var store : storage) : return is
  begin
    if Tezos.sender =/= store.owner then failwith ("Only owner can withdraw.") else skip ;
    if Tezos.now < store.deadline   then failwith ("Too early.")               else skip ;
    if Tezos.balance < store.target then failwith ("Below target.")            else skip ;
    const dest : contract (unit) = tezos_get_contract (Tezos.sender);
    const op   : operation       = Tezos.transaction (Unit, balance, dest) ;
    store.funded := True;
  end with (list [op], store)

function main (const action : parameter; const store : storage) : return is
  case action of
  | Back     -> back (Unit, store)
  | Claim    -> claim (Unit, store)
  | Withdraw -> withdraw (Unit, store)
  end

(* test inputs *)
const test_back : storage = record [
    owner = ("KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi" : address) ;
    target = 100tz ;
    deadline = ("2020-03-10T10:00:00Z" : timestamp) ;
    backers = (map [
      ("tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA" : address) -> 1tz ;
      ("tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc" : address) -> 2tz ;
// "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
    ] : map(address,tez));
    funded = False ;
]