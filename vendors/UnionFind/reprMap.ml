(* Map where the key is the repr *)

type ('item, 'a) reprMap = {
  merge: 'a -> 'a -> 'a;
  map: ('item, 'a) PolyMap.t
}

let create ~cmp ~merge = {
  merge;
  map = PolyMap.create ~cmp;
}

let alias ~other_repr ~new_repr (m : (_,_) reprMap) =
  { m with
    map = match PolyMap.find_opt other_repr m.map with
      | None -> m.map
      | Some other_value ->
        PolyMap.update
          new_repr
          (function
              None -> None
            | Some v -> Some (m.merge other_value v))
          m.map }

let empty m = { m with map = PolyMap.empty m.map }

let is_empty m = PolyMap.is_empty m.map

let add k v m = { m with map = PolyMap.add k v m.map }

(* No removal, should be monotonic aside from merges *)
(* let remove k m = { m with map = PolyMap.remove k m.map } *)

(* find throws an exception, disabling it *)
(* let find k m = PolyMap.find k m.map *)

let find_opt k m = PolyMap.find k m.map

let find_default k default m = PolyMap.find k default m.map

let has_key k m = PolyMap.has_key k m.map

let bindings m = PolyMap.bindings m.map