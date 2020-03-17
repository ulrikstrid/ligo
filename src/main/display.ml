type json = Yojson.Basic.t

type 'a display_format =
  | Human_readable : string display_format
  | Dev : string display_format
  | Json : json display_format

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

(* let rec error_pp ?(dev = false) out (e : error) =
  let open JSON_string_utils in
  let message =
    let opt = e |> member "message" |> string in
    match opt with
    | Some msg -> ": " ^ msg
    | None -> "" in
  let error_code =
    let error_code = e |> member "error_code" in
    match error_code with
    | `Null -> ""
    | _ -> " (" ^ (J.to_string error_code) ^ ")" in
  let title =
    let opt = e |> member "title" |> string in
    Option.unopt ~default:"" opt in
  let data =
    let data = e |> member "data" in
    match data with
    | `Null -> ""
    | _ -> " " ^ (J.to_string data) ^ "\n" in
  let infos =
    let infos = e |> member "infos" in
    match infos with
    | `List lst -> lst
    | `Null -> []
    | x -> [ x ] in
  let children =
    let infos = e |> member "children" in
    match infos with
    | `List lst -> lst
    | `Null -> []
    | x -> [ x ] in
  let location =
    let opt = e |> member "data" |> member "location" |> string in
    let aux cur prec =
      match prec with
      | None -> cur |> member "data" |> member "location" |> string
      | Some s -> Some s
    in
    match List.fold_right aux infos opt with
    | None -> ""
    | Some s -> s ^ ". "
  in
  let print x = Format.fprintf out x in
  if not dev then (
    print "%s%s%s%s%s" location title error_code message data
  ) else (
    print "%s%s%s.\n%s%s\n%a\n%a\n" title error_code message data location
      (Format.pp_print_list (error_pp ~dev)) infos
      (Format.pp_print_list (error_pp ~dev)) children
  ) *)



