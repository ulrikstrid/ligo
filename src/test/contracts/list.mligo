type storage = int * int list

type param = int list

let x : int list = []
let y : int list = [3; 4; 5]
let z : int list = 2::y

let main (p: param) storage =
  let storage =
    match p with
          [] -> storage
    | hd::tl -> storage.0 + hd, tl
  in ([] : operation list), storage

let fold_op (s: int list) : int =
  let aggregate = fun (prec: int) (cur: int) -> prec + cur
  in List.fold s 10 aggregate

let map_op (s: int list) : int list =
  List.map s (fun (cur: int) -> cur + 1)

let iter_op (s : int list) : unit =
  let do_nothing = fun (_: int) -> unit
  in List.iter s do_nothing
