type storage = {
  owner: address ,
  controller: address ,
};

type auth =
  | At_least_controller(address)
  | At_least_owner(address);



let at_least_controller = (ps: (address, storage)): bool =>
  let parameter, storage = ps;
  if (parameter == storage.owner || parameter == storage.controller) {
    true;
  }
  else {
    false;
  };

let at_least_owner = (ps: (address, storage)): bool =>
  let parameter, storage = ps;
  if (parameter == storage.owner) {
    true;
  }
  else {
    false;
  };

let main = (ps: (auth, storage)): (list(operation), bool) =>
  let parameter, storage = ps;
  let result: bool = 
    switch (parameter) {
    | At_least_controller(addr) => at_least_controller((addr, storage))
    | At_least_owner(addr) => at_least_owner((addr, storage))
    };
  (([]: list(operation)), result);
