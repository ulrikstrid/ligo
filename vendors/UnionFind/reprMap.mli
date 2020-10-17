type ('item, 'a) reprMap = {
  merge : 'a -> 'a -> 'a;
  map : ('item, 'a) PolyMap.t;
}
val create :
  cmp:('a -> 'a -> int) -> merge:('b -> 'b -> 'b) -> ('a, 'b) reprMap
val alias :
  other_repr:'a -> new_repr:'a -> ('a, 'b) reprMap -> ('a, 'b) reprMap
val empty : ('a, 'b) reprMap -> ('a, 'b) reprMap
val is_empty : ('a, 'b) reprMap -> bool
val add : 'a -> 'b -> ('a, 'b) reprMap -> ('a, 'b) reprMap
val find_opt : 'a -> ('a, 'b) reprMap -> 'b
val find_default :
  'a -> ('a, ('b, 'c) PolyMap.t -> 'd) PolyMap.t -> ('b, 'c) reprMap -> 'd
val has_key : 'a -> ('a, 'b) reprMap -> bool
val bindings : ('a, 'b) reprMap -> ('a * 'b) list
