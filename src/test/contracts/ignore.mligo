let main (x,_ : int * int) =
  let y = Tezos.balance in
  let w = 42 in
  begin
    Ligo.ignore y;
    Ligo.ignore x;
    Ligo.ignore w;
    (([] : operation list), 42)
  end
