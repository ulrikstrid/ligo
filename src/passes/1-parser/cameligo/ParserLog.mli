(** Printing the CST *)

(** The type [state] captures the state that is threaded in the
    printing iterators in this module.
*)
type state

val mk_state :
  offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

(** {1 Printing tokens from the CST in a buffer}

   Printing the tokens reconstructed from the CST. This is very useful
   for debugging, as the output of [print_token ast] can be textually
   compared to that of [Lexer.trace] (see module [LexerMain]). *)

val print_tokens  : state -> CST.t -> unit
val print_pattern : state -> CST.pattern -> unit
val print_expr    : state -> CST.expr -> unit

val tokens_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.t -> string
val pattern_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.pattern -> string
val expr_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.expr -> string

(** {1 Pretty-printing of CST nodes} *)

val pp_ast  : state -> CST.t -> unit
val pp_expr : state -> CST.expr -> unit
