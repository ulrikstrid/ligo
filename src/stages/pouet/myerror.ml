open Pervasives
open Display

let error_ppformat ~display_format f a =
  match display_format with
  | Human_readable | Dev -> (
    match a with 
    | `Bar_error1 i | `Bar_error2 i ->
      let i' = Format.asprintf " error : %i" i in 
      Format.pp_print_string f i'
    | `Foo_error1 i | `Foo_error2 i ->
      let i' = Format.asprintf " error : %i" i in 
      Format.pp_print_string f i'
  )

let error_jsonformat a =
  match a with 
  | `Bar_error1 i | `Bar_error2 i ->
    `Assoc [("bar_error", `Int i)]
  | `Foo_error1 i | `Foo_error2 i ->
    `Assoc [("foo_error", `Int i)]

let error_format : 'a Display.format = {
  pp = error_ppformat;
  to_json = error_jsonformat;
}

let bind_result =
  fun x ~f ->
  match x with
  | Ok x -> f x
  | Error err -> Error err

module Let_syntax = struct
  let bind = bind_result
  module Open_on_rhs_bind = struct end
end

let bind_format : 'a Display.format -> 'b Display.format -> ('b,'a) result Display.format =
  fun error_format value_format ->
    let pp ~display_format f a = match a with
      | Error e -> error_format.pp ~display_format f e
      | Ok v -> value_format.pp ~display_format f v in
    let to_json a = match a with
      | Error e -> error_format.to_json  e
      | Ok v -> value_format.to_json v in
    { pp ; to_json }
