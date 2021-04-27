open Simple_utils.Display
open Ast_core

let stage = "self_ast_core"

type self_ast_core_error = [
  | `Self_ast_core_shadowing of expression_variable
  | `Self_ast_core_capturing of expression_variable list
]

let shadowing v = `Self_ast_core_shadowing v
let capturing vs = `Self_ast_core_capturing vs

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_ast_core_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_ast_core_shadowing v ->
      Format.fprintf f
        "@[<hv>%a@ Cannot be shadowed. @]"
        Snippet.pp v.location
    | `Self_ast_core_capturing v ->
       let l = List.map (fun (v : expression_variable) -> v.location) v in
       Format.fprintf f
         "@[<hv>%a@ Cannot be captured. @]"
         (PP_helpers.list_sep_d Snippet.pp) l
  )

let error_jsonformat : self_ast_core_error -> json = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Self_ast_core_shadowing v ->
    let message = `String "Cannot be shadowed" in
    let loc = `String (Format.asprintf "%a" Location.pp v.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Self_ast_core_capturing v ->
    let message = `String "Cannot be captured" in
    let v0 = List.nth v 0 in
    let loc = `String (Format.asprintf "%a" Location.pp v0.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
