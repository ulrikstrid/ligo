(**

The map type is two way mapping from keys of one type to values of another. It is
analogous to the hashmap type in C-like languages, python dictionaries, etc.

*)
type foobar = (int , int) map

let empty_map : foobar = Map.empty

let map1 : foobar = Map.literal
  [ (144 , 23) ; (51 , 23) ; (42 , 23) ; (120 , 23) ; (421 , 23) ]
let map2 : foobar = Map [ (23 , 0) ; (42 , 0) ]

(**

Map.update takes a key, optional value, and map. It updates the given key in the given
map with the optional value.

*)
let set_ (n : int) (m : foobar) : foobar =
    Map.update 23 (Some n) m

(**

Map.remove is like Map.update, but it removes a key/value pair instead of updating
it.

*)
let rm (m : foobar) : foobar = Map.remove 42 m

(**

Map.size lets us get the size of a map.

*)
let size_ (m : foobar) : nat = Map.size m

(**

Map.find retrieves the value associated with the given key inside its map
argument.

*)
let gf (m : foobar) : int = Map.find 23 m

let get (m : foobar) : int option = Map.find_opt 42 m
let get_ (m : foobar) : int option = Map.find_opt 42 m

(**

TODO: Describe these

*)

let iter_op (m : foobar) : unit =
    let assert_eq = fun (i : int) (j : int) -> assert(i=j) in
    Map.iter m assert_eq

let map_op (m : foobar) : foobar =
    let increment = fun (i : int) (j : int) -> j+1 in
    Map.map m increment

let fold_op (m : foobar) : foobar =
    let aggregate = fun (i : int) (j : (int * int)) -> i + j.(0) + j.(1) in
    Map.fold m 10 aggregate

let deep_op (m : foobar) : foobar =
  let coco = (0,m) in
  let coco = (0 , Map.remove 42 coco.(1)) in
  let coco = (0 , Map.update 32 (Some 16) coco.(1)) in
  coco.(1)
