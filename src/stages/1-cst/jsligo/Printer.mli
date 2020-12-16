(* Printing the CST *)

(* The type [state] captures the state that is threaded in the
    printing iterators in this module. *)

type state

val mk_state :
  offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

(* Printing tokens from the CST in a buffer *)

val print_tokens  : state -> CST.t -> unit

(* Pretty-printing *)

val pp_cst  : state -> CST.t -> unit
