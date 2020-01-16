type storage = {
  owner: address;
  controller: address;
}

type auth =
| At_least_controller of address
| At_least_owner of address

let at_least_controller (parameter, storage: address * storage) : bool =
  if (parameter = storage.owner || parameter = storage.controller)
  then true
  else false

let at_least_owner (parameter, storage: address * storage) : bool =
  if parameter = storage.owner
  then true
  else false

let main (parameter, storage: auth * storage) : bool =
  match parameter with
  | At_least_controller addr -> at_least_controller (addr, storage)
  | At_least_owner addr -> at_least_owner (addr, storage)
