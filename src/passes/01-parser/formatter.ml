open Display

let ppx_ppformat ~display_format f (buf,_) =
  match display_format with
  | Human_readable | Dev -> Format.fprintf f "%s" (Buffer.contents buf)

let ppx_jsonformat (buf,_) : json =
  let s = Format.asprintf "%s" (Buffer.contents buf) in
  `String s

let ppx_format : 'a format = {
  pp = ppx_ppformat;
  to_json = ppx_jsonformat;
}