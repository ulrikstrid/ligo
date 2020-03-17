open Trace
open Types


(*

module Errors : sig
  val different_literals_because_different_types : name -> literal -> literal -> unit -> error

  val different_literals : name -> literal -> literal -> unit -> error

  val error_uncomparable_literals : name -> literal -> literal -> unit -> error
end

val assert_literal_eq : ( literal * literal ) -> unit result
*)
module Errors : sig
  type 'a assert_eq_tracer = { a:expression ; b:expression ; error : 'a }
end

val assert_value_eq : ( expression * expression ) -> (unit ,
            [> `Simple_error of string
             | `Sugar_Not_equal of 'a Errors.assert_eq_tracer ]
            as 'a)
            result

val is_value_eq : ( expression * expression ) -> bool
