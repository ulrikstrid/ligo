let main (p : key_hash) =
  let c : unit contract = Current.implicit_account p in 
  Current.address c

let shadow_builtin1 (source: address) =
  source

let shadow_builtin2 (source: address) =
  let res : address = source in
  res
