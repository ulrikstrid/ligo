open Trace
open Display

let error_ppformat : display_format:string display_format ->
  Format.formatter -> [> error ] -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with 
    | `Simple_error _ -> ()
    | `Sys_error _ -> ()
    | `Different_literals_types i ->
      let i' = Format.asprintf " error : %i" i in 
      Format.pp_print_string f i'
  )

let error_jsonformat : [> error] -> J.t = fun a ->
  match a with
  | `Simple_error s -> `Assoc [("simple_error", `String s)] 
  | `Sys_error _ -> `Assoc [("sys_error", `String "todo")]

let error_format : [> error] Display.format = {
  pp = error_ppformat;
  to_json = error_jsonformat;
}