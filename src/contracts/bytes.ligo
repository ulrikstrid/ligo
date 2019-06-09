type parameter is bytes
type storage_ is bytes * bytes

function main (const p : parameter ; const s : storage_) : (list(operation) * storage_) is
  block { skip }
  with ((nil : list(operation)),
        (0x626F6E_00_6A6F7572,
         p))
