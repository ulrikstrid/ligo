open Trace
val run : unit Alcotest.test_case -> unit
val wrap : (unit -> unit result) -> (unit -> unit)