module F = Format
module J = Yojson.Basic

type json = J.t

type 'a display_format =
| Human_readable : string display_format
| Dev : string display_format
| Json : json display_format

type 'a pp = display_format:(string display_format) -> F.formatter -> 'a -> unit
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
  | Dev -> F.asprintf "%a" (format.pp ~display_format) value
  | Human_readable -> F.asprintf "%a" (format.pp ~display_format) value

let to_json : displayable -> json = convert ~display_format:Json