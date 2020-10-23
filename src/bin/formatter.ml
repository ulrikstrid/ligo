open Display

let declarations_ppformat ~display_format f (source_file,decls) =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%s declarations:\n" source_file ;
    List.iter (fun decl -> Format.fprintf f "%s\n" decl) decls

let declarations_jsonformat (source_file,decls) : json =
  let json_decl = List.map (fun decl -> `String decl) decls in
  `Assoc [ ("source_file", `String source_file) ; ("declarations", `List json_decl) ]

let declarations_format : 'a format = {
  pp = declarations_ppformat;
  to_json = declarations_jsonformat;
}

let changelog_ppformat ~display_format f changelog =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%s" changelog

let changelog_jsonformat changelog : json =
  `String changelog

let changelog_format : 'a format = {
  pp = changelog_ppformat;
  to_json = changelog_jsonformat;
}

let contract_size_ppformat ~display_format f contract_size =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%d bytes" contract_size

let contract_size_jsonformat contract_size : json =
  `Int contract_size

let contract_size_format : 'a format = {
  pp = contract_size_ppformat;
  to_json = contract_size_jsonformat;
}

module Michelson_formatter = struct
  open Tezos_utils.Michelson

  let pp_hex ppf michelson =
    let hex = Proto_alpha_utils.Memory_proto_alpha.to_hex michelson in
    Format.fprintf ppf "%a" Hex.pp hex

  type michelson_format = [
    | `Text
    | `Json
    | `Hex
  ]

  let michelson_ppformat michelson_format ~display_format f a =
    let mich_pp = fun michelson_format ->  match michelson_format with
      | `Text -> pp
      | `Json -> pp_json
      | `Hex -> pp_hex in
    match display_format with
    | Display.Human_readable | Dev -> (
       let m = Format.asprintf "%a\n" (mich_pp michelson_format) a in
       Format.pp_print_string f m
    )

  let michelson_jsonformat michelson_format a : Display.json = match michelson_format with
    | `Text ->
      let code_as_str = Format.asprintf "%a" pp a in
      `Assoc [("text_code" , `String code_as_str)]
    | `Hex -> 
      let code_as_hex = Format.asprintf "%a" pp_hex a in
      `Assoc [("hex_code" , `String code_as_hex)]
    | `Json ->
      (* Ideally , would like to do that :
      Michelson.get_json a *)
      let code_as_str = Format.asprintf "%a" pp_json a in
      `Assoc [("json_code" , `String code_as_str)]

  let michelson_format : michelson_format -> 'a Display.format = fun mf -> {
    pp = michelson_ppformat mf;
    to_json = michelson_jsonformat mf;
  }

  let source_map_ppformat ~display_format f source_map =
    match display_format with
    | Human_readable | Dev ->
      Format.fprintf f "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f "@ ")
           (fun f (t, s) ->
              Format.fprintf f "%d: %a"
                t
                Location.pp s))
        source_map

  let source_map_jsonformat source_map : json =
    `List (List.map (fun (t, s) -> `Tuple [`Int t; Location.to_yojson s]) source_map)

  let source_map_format : (int * Location.t) list Display.format = {
    pp = source_map_ppformat;
    to_json = source_map_jsonformat;
  }

  open Sourcemaps
  module Json_builder : Sourcemap.Json_writer_intf with type t = json = struct
    type t = json
    let of_string s = `String s
    let of_obj kvs = `Assoc kvs
    let of_array xs = `List xs
    let of_number hmm = `Intlit hmm
    let null = `Null
  end
  module J = Sourcemap.Make_json_writer(Json_builder)

  let real_source_map_format : Sourcemap.t Display.format = {
    pp =
      (fun ~display_format f map ->
         ignore display_format;
         Format.fprintf f "%a" Yojson.Safe.pp (J.json_of_sourcemap map) );
    to_json = J.json_of_sourcemap ;
  }

  let wip_concrete_locations_format : (int * Tezos_micheline.Micheline_parser.location) list Display.format = {
    pp =
      (fun ~display_format f clocs ->
         ignore display_format;
         Format.fprintf f "@[<v>%a@]"
           (Format.pp_print_list
              ~pp_sep:(fun f () -> Format.fprintf f "@ ")
              (fun f (loc, Tezos_micheline.Micheline_parser.{ start; stop }) ->
                 Format.fprintf f "%d %d:%d-%d:%d"
                   loc
                   start.line start.column
                   stop.line stop.column))
           clocs);
    to_json = (fun _ -> `Null) ;
  }
end
