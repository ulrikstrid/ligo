type storage is record [
  owner: address;
  controller: address;
]

type auth is
| At_least_controller of address
| At_least_owner of address

function at_least_controller (const parameter: address; const storage: storage) : bool is
  begin
    var result: bool := False ;
    if (parameter = storage.owner) or (parameter = storage.controller)
    then result := True
    else result := False
  end with result

function at_least_owner (const parameter: address; const storage: storage) : bool is
  begin
    var result: bool := False ;
    if parameter = storage.owner
    then result := True
    else result := False
  end with result

function main (const parameter: auth; const storage: storage) : (list(operation) * bool) is
  begin
    var result : bool := False ;
    case parameter of
    | At_least_controller(addr) -> result := at_least_controller(addr, storage)
    | At_least_owner(addr) -> result := at_least_owner(addr, storage)
    end
  end with ((nil: list(operation)), result)
