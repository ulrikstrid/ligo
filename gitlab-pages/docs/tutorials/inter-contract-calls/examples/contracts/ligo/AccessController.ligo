(* We show this contract to demonstrate a possible flaw in
   inter-contract communications. Do not use it in production:
   it's vulnerable because it contains a view entrypoint.
   You should either remove this entrypoint or use tickets
   for authorization. See "Inter-contract invocations" and
   "Security" tutorials for the details
*)

type parameter is
| Call of unit -> operation
| IsWhitelisted of address * contract (bool)

type storage is record [senders_whitelist : set (address)]

function main (const p : parameter; const s : storage) is
block {
  const op =
      case p of
      | Call (op) ->
          if Set.mem (Tezos.sender, s.senders_whitelist)
          then op (Unit)
          else (failwith ("Sender is not whitelisted") : operation)
      | IsWhitelisted (addr_and_callback) ->
          block {
            const addr = addr_and_callback.0;
            const callback_contract = addr_and_callback.1;
            const whitelisted = Set.mem (addr, s.senders_whitelist)
          } with Tezos.transaction(whitelisted, 0mutez, callback_contract)
      end
} with (list [op], s)
