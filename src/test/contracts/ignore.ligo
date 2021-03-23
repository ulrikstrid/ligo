function main (const x : int; const s : int) is
  block {
    const y = Tezos.balance;
    const w = 42;
    Ligo.ignore (y);
    Ligo.ignore (x);
    Ligo.ignore (w);
    Ligo.ignore (s);
    } with ((list [] : list (operation)), 42)

