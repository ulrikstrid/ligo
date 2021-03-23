let main = ((x,_) : (int, int)) =>
  let y = Tezos.balance;
  let w = 42;
  { Ligo.ignore(y);
    Ligo.ignore(x);
    Ligo.ignore(w);
    (([] : list(operation)), 42) }
