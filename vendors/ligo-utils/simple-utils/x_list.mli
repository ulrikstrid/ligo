(* stdlib list *)
val length : 'a list -> int
val compare_lengths : 'a list -> 'b list -> int
val compare_length_with : 'a list -> int -> int
val cons : 'a -> 'a list -> 'a list
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val nth : 'a list -> int -> 'a
val nth_opt : 'a list -> int -> 'a option
val rev : 'a list -> 'a list
val init : int -> (int -> 'a) -> 'a list
val append : 'a list -> 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val concat : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
val iter : ('a -> unit) -> 'a list -> unit
val iteri : (int -> 'a -> unit) -> 'a list -> unit
val map : ('a -> 'b) -> 'a list -> 'b list
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val rev_map : ('a -> 'b) -> 'a list -> 'b list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
val for_all : ('a -> bool) -> 'a list -> bool
val exists : ('a -> bool) -> 'a list -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(* val mem : 'a -> 'a list -> bool *)
(* val memq : 'a -> 'a list -> bool *)
val find : ('a -> bool) -> 'a list -> 'a
val find_opt : ('a -> bool) -> 'a list -> 'a option
val filter : ('a -> bool) -> 'a list -> 'a list
val find_all : ('a -> bool) -> 'a list -> 'a list
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
(* val assoc : 'a -> ('a * 'b) list -> 'b *)
(* val assoc_opt : 'a -> ('a * 'b) list -> 'b option *)
(* val assq : 'a -> ('a * 'b) list -> 'b *)
(* val assq_opt : 'a -> ('a * 'b) list -> 'b option *)
(* val mem_assoc : 'a -> ('a * 'b) list -> bool *)
(* val mem_assq : 'a -> ('a * 'b) list -> bool *)
(* val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list *)
(* val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list *)
val split : ('a * 'b) list -> 'a list * 'b list
val combine : 'a list -> 'b list -> ('a * 'b) list
val sort : ('a -> 'a -> int) -> 'a list -> 'a list
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
val to_seq : 'a list -> 'a Seq.t
val of_seq : 'a Seq.t -> 'a list


(* custom stuff *)
val remove : int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val fold_map_right :
  ('acc -> 'ele -> 'acc * 'ret) -> 'acc -> 'ele list -> 'ret list
val fold_map_acc :
  ('acc -> 'ele -> 'acc * 'ret) -> 'acc -> 'ele list -> 'acc * 'ret list
val fold_map :
  ('acc -> 'ele -> 'acc * 'ret) -> 'acc -> 'ele list -> 'ret list
val fold_right' : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val unopt : default:'a -> 'a option -> 'a
val remove_element : compare:('a -> 'b -> int) -> 'a -> 'b list -> 'b list
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val cons_iter : ('a -> unit) -> ('a -> unit) -> 'a list -> unit
val range : int -> int list
val find_map : ('a -> 'b option) -> 'a list -> 'b option
val find_index : ('a -> bool) -> 'a list -> int
val find_full : ('a -> bool) -> 'a list -> 'a * int
val assoc_i : compare:('a -> 'a -> int) -> 'a -> ('a * 'b) list -> 'b * int
val from : int -> 'a list -> 'a list
val until : int -> 'a list -> 'a list
val uncons_opt : 'a list -> ('a * 'a list) option
val rev_uncons_opt : 'a list -> ('a list * 'a) option
val hds : 'a list -> 'a list
val to_pair : 'a list -> ('a * 'a) option
val to_singleton : 'a list -> 'a option

val mem : compare:('a -> 'a -> int) -> 'a -> 'a list -> bool
val assoc : compare:('a -> 'a -> int) -> 'a -> ('a * 'b) list -> 'b
val assoc_opt : compare:('a -> 'a -> int) -> 'a -> ('a * 'b) list -> 'b option
val mem_assoc : compare:('a -> 'a -> int) -> 'a -> ('a * 'b) list -> bool
val remove_assoc : compare:('a -> 'a -> int) -> 'a -> ('a * 'b) list -> ('a * 'b) list

val compare : compare:('a -> 'a -> int) -> 'a list -> 'a list -> int

module Ne : sig
  type 'a t = 'a * 'a list
  val of_list : 'a list -> 'a * 'a list
  val to_list : 'a t -> 'a list
  val singleton : 'a -> 'a t
  val hd : 'a t -> 'a
  val cons : 'a -> 'a t -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b * 'b list
  val hd_map : ('a -> 'a) -> 'a t -> 'a t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b * 'b list
  val concat : 'a list t -> 'a list
  val rev : 'a t -> 'a * 'a list
  val find_map : ('a -> 'b option) -> 'a t -> 'b option
  val append : 'a t -> 'a t -> 'a t
end
