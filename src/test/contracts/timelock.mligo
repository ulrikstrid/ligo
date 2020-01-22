type storage = {
  after: timestamp;
  execute: unit -> operation list;
}

let main (p,s: unit * storage) : operation list * storage =
  if Current.time > s.after
  then (s.execute (), s)
  else (([]: operation list), s)
