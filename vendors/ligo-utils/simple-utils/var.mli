type t
val equal : t -> t -> bool
val compare : t -> t -> int
(* generate a var not in the given list of vars *)
val fresh : ?name:string -> t list -> t
val of_name : string -> t
val name_of : t -> string
val pp : Format.formatter -> t -> unit
