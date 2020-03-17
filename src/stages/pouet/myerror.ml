open Pervasives
open Display

type nonrec ('value,'error) result = ('value, 'error) result

type error = [
  | `Simple_error of string
  | `Traced_error of (error * error)
] 


let rec error_ppformat ~display_format f a =
  match display_format with
  | Human_readable | Dev -> (
    match a with 
    | `Bar_error1 i | `Bar_error2 i ->
      let i' = Format.asprintf " error : %i" i in 
      Format.pp_print_string f i'
    | `Foo_error1 i | `Foo_error2 i ->
      let i' = Format.asprintf " error : %i" i in 
      Format.pp_print_string f i'
    | `Simple_error s ->
      let i' = Format.asprintf " simple error : %s" s in 
      Format.pp_print_string f i'
    | `Traced_error (top,prev) ->
      let ()  = error_ppformat ~display_format Format.str_formatter top in
      let top' = Format.flush_str_formatter () in
      let () = error_ppformat ~display_format Format.str_formatter prev in
      let prev' = Format.flush_str_formatter () in
      Format.pp_print_string f @@ Format.asprintf " traced error, top : %s | other : %s" top' prev'
  )

let rec error_jsonformat a =
  match a with 
  | `Bar_error1 i | `Bar_error2 i ->
    `Assoc [("bar_error", `Int i)]
  | `Foo_error1 i | `Foo_error2 i ->
    `Assoc [("foo_error", `Int i)]
  | `Simple_error i ->
    `Assoc [("simple_error", `String i)]
  | `Traced_error (top,prev) ->
    `Assoc [
      ("top_err", (error_jsonformat top));
      ("prev_err", (error_jsonformat prev));
    ]


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

let simple_error str = `Simple_error str

let bind_format : 'a Display.format -> 'b Display.format -> ('b,'a) result Display.format =
  fun error_format value_format ->
    let pp ~display_format f a = match a with
      | Error e -> error_format.pp ~display_format f e
      | Ok v -> value_format.pp ~display_format f v in
    let to_json a = match a with
      | Error e -> error_format.to_json e
      | Ok v -> value_format.to_json v in
    { pp ; to_json }

let trace top_err = function
  | Ok _ as o -> o
  | Error err -> Error (`Traced_error (top_err,err))