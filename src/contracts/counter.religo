type storage = int;

let%entry main = (p: int, storage) => ([]: list(operation), p + storage);
