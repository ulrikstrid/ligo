let main = (p : key_hash) : address => {
  let c : contract(unit) = Current.implicit_account(p) ;
  Current.address(c) ;
};

let shadow_builtin1 = (source: address) : address => {
  source
};

let shadow_builtin2 = (source: address) : address => {
  let res : address = source ;
  res
};