type i1 = < i1 : unit >
type i2 = < i2 : unit >
type i3 = < i3 : unit >

type t_available_indexers = < i1; i2; i3; >
let available_indexers : t_available_indexers = object
  method i1 = ()
  method i2 = ()
  method i3 = ()
end

type 'req h = {
  f : 'req -> unit
}

module H1 = struct
  type req = < i1; i2 >
  let f : <req;..> -> unit = fun _ -> ()
end
let h1 = { f = H1.f }

module H2 = struct
  type req = < i2; i3 >
  let f : <req;..> -> unit = fun _ -> ()
end
let h2 = { f = H2.f }


let hs : t_available_indexers h list = [ h1; h2 ]



module H = functor (Req : sig class t : object end end) -> struct module type S = sig
  val f : <Req.t;..> -> unit
end end

module H1 = struct
  type req = < i1; i2 >
  let f : < req;..> -> unit = fun _ -> ()
end

module R2 = struct class t = available_indexers end
module H2 : H(R2).S = struct
  type req = < i2; i3 >
  let f : <req;..> -> unit = fun _ -> ()
end

(* module type HH = H with type +'self req := < available_indexers; .. > as 'self *)
module ZZ = struct type t = available_indexers end
let hs : (module H(ZZ).S) list = [ (module H1); (module H2) ]

let _ = match hs with
  | hd :: _ ->
    let module Hd = (val hd) in
    Hd.f available_indexers
  | _ -> failwith "pvdegelgvdlkg"
