type virtual_location = string

type t =
  | File of Region.t (* file_location *)
  | Virtual of virtual_location

let pp = fun ppf t ->
  match t with
  | Virtual s -> Format.fprintf ppf "%s" s
  | File f -> Format.fprintf ppf "%s" (f#to_string `Point)

let virtual_location s = Virtual s
let dummy = virtual_location "dummy"
let generated = virtual_location "generated"

type 'a wrap = {
  wrap_content : 'a ;
  location : t ;
}

let wrap ?(loc = generated) wrap_content = { wrap_content ; location = loc }
let get_location x = x.location
let unwrap { wrap_content ; _ } = wrap_content
let map f x = { x with wrap_content = f x.wrap_content }

let lift_region : 'a Region.reg -> 'a wrap = fun x ->
  wrap ~loc:(File x.region) x.value
let lift : Region.region -> t = fun x -> File x
let pp_lift = fun ppf r -> pp ppf @@ lift r

let r_split : 'a Region.reg -> ('a * t) = fun x -> x.value , File x.region
