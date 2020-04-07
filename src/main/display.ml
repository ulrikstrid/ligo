type json = Yojson.Basic.t

type 'a display_format =
  | Human_readable : string display_format
  | Dev : string display_format
  | Json : json display_format

type 'b consommateur = { run : 'a . 'a display_format -> 'b }
type planque = { tt : 'b . 'b consommateur -> 'b }

let human_readable = { tt = fun f -> (f.run Human_readable) }
let dev = { tt = fun f -> (f.run Dev) }
let json = { tt = fun f -> (f.run Json) }

type 'a pp = display_format:(string display_format) -> Format.formatter -> 'a -> unit
type 'a format = {
    pp : 'a pp ;
    to_json : 'a -> json ;
}

type 'a with_format = {
    value : 'a ;
    format : 'a format ;
}

type displayable = Displayable : 'a with_format -> displayable

let convert : type output . display_format:(output display_format) -> displayable -> output =
  fun ~display_format (Displayable { value ; format }) ->
  match display_format with
  | Json -> format.to_json value
  | Dev -> Format.asprintf "%a" (format.pp ~display_format) value
  | Human_readable -> Format.asprintf "%a" (format.pp ~display_format) value

let to_json : displayable -> json = convert ~display_format:Json

let bind_format :
  'value format -> 'error format -> ('value,'error) result format =
  fun value_format error_format ->
    let pp ~display_format f a = match a with
      | Error e -> error_format.pp ~display_format f e
      | Ok v -> value_format.pp ~display_format f v in
    let to_json a = match a with
      | Error e -> error_format.to_json e
      | Ok v -> value_format.to_json v in
    { pp ; to_json }

(* TODO : move michelson format things out of here *)
type michelson_format = [
  | `Text
  | `Json
  | `Hex
]

let michelson_ppformat michelson_format ~display_format f (a,_) =
  let mich_pp = fun michelson_format ->  match michelson_format with
    | `Text -> Michelson.pp
    | `Json -> Michelson.pp_json
    | `Hex -> Michelson.pp_hex in
  match display_format with
  | Human_readable | Dev -> (
     let m = Format.asprintf "%a\n" (mich_pp michelson_format) a in
     Format.pp_print_string f m
  )

let michelson_jsonformat michelson_format (a,_) : json = match michelson_format with
  | `Text ->
    let code_as_str = Format.asprintf "%a" Michelson.pp a in
    `Assoc [("text_code" , `String code_as_str)]
  | `Hex -> 
    let code_as_hex = Format.asprintf "%a" Michelson.pp_hex a in
    `Assoc [("hex_code" , `String code_as_hex)]
  | `Json ->
    (* Ideally , would like to do that :
    Michelson.get_json a *)
    let code_as_str = Format.asprintf "%a" Michelson.pp_json a in
    `Assoc [("json_code" , `String code_as_str)]


let michelson : michelson_format -> 'a format = fun mf -> {
  pp = michelson_ppformat mf;
  to_json = michelson_jsonformat mf;
}