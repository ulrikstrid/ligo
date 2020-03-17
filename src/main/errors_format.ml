(* open Trace *)
open Display

let error_ppformat ~display_format f a =
  match display_format with
  | Human_readable | Dev -> (
    match a with 
    | `Different_literals_types i ->
      let i' = Format.asprintf " error : %i" i in 
      Format.pp_print_string f i'
  )