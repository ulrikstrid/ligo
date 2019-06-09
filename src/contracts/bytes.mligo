type parameter = bytes
type storage = bytes * bytes

let%entry main (p : parameter) (s : storage) =
  (([] : operation list),
   (0x626f6e_00_6A6F7572,
    p))
