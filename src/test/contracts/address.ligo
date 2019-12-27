// function main (const c: contract(unit)) : address is address(c)

function main (const p : key_hash) : address is block {
  const c : contract(unit) = implicit_account(p) ;
} with address(c)

function shadow_builtin1(const source: address): address is source

function shadow_builtin2(const source: address): address is
block {
   const res : address = source ;
} with res
