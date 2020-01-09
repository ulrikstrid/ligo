function main (const p: address) : contract(unit) is 
  block {
    const c: contract(unit) = get_contract(p);
  } with c
