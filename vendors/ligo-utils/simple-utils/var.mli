type t
val equal : t -> t -> bool
val compare : t -> t -> int
val fresh : ?name:string -> unit -> t
val of_name : string -> t
val name_of : t -> string
val pp : Format.formatter -> t -> unit
