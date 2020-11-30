(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Errors *)

type message = string Region.reg

type lexing_error = [
  `Lexer_generic of message
]

let stage = "lexing"

let generic reg = `Lexer_generic reg

(* Formatting *)

let error_ppformat :
      display_format:(string Display.display_format) ->
      Format.formatter ->
      lexing_error ->
      unit =
  fun ~display_format f a ->
  match display_format with
    Human_readable | Dev ->
      match a with
        `Lexer_generic Region.{value; region} ->
          Snippet.pp_lift f region;
          Format.pp_print_string f value

let error_jsonformat : lexing_error -> Yojson.Safe.t =
  fun error ->
    let json_error ~stage ~content =
      `Assoc [
         ("status", `String "error");
         ("stage",  `String stage);
         ("content", content)] in
    match error with
     `Lexer_generic Region.{value; region} ->
        let loc = Location.lift @@ region in
        let content =
          `Assoc [
             ("message",  `String value);
             ("location", Location.to_yojson loc)]
        in json_error ~stage ~content
