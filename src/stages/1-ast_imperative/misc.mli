open Trace
open Types

module Errors : sig
  type 'a assert_eq_tracer = { a:expression ; b:expression ; error : 'a }
end

val assert_value_eq :
  expression * expression ->
  (unit,
    [> `Imperative_Not_equal of 'a Errors.assert_eq_tracer
     | `Simple_error of string ]
  as 'a)
  result

val is_value_eq : ( expression * expression ) -> bool
