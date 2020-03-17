open Trace
open Types

module Errors : sig
  type 'a assert_eq_tracer = { a:expression ; b:expression ; error : 'a }
end

val assert_value_eq :
  ( expression * expression ) ->
  (unit ,
    [> `Core_Different_literals_types of literal * literal
     | `Core_Different_literals_value of literal * literal
     | `Core_Not_a_value of expression
     | `Core_Not_equal of 'a Errors.assert_eq_tracer
     | `Core_Uncomparable of expression * expression
     | `Core_Uncomparable_literals of literal * literal
     | `Simple_error of string ]
    as 'a) result

val is_value_eq : ( expression * expression ) -> bool
