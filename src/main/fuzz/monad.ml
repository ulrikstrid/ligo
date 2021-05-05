open QCheck
open Ast_imperative

module Let_syntax = struct
  let bind m ~f = Gen.(m >>= f)
  module Open_on_rhs_bind = struct end
end

type 'a monad = 'a Gen.t
let ok x = Gen.return x
let (>>?)  x f = Gen.(x >>= f)

let bind_lmap (l:_ label_map) =
  let open LMap in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux l (ok empty)

let bind_location (x:_ Location.wrap) =
  x.wrap_content >>? fun wrap_content ->
  ok { x with wrap_content }

let bind_map_lmap f map = bind_lmap (LMap.map f map)

let rec bind_list = function
      [] -> ok []
| hd::tl -> hd >>? fun hd -> bind_list tl >>? fun tl -> ok @@ hd :: tl

let bind_map_list f lst = bind_list (List.map f lst)

let bind_map_option f = function
    None -> ok None
  | Some s -> f s >>? fun x -> ok (Some x)

let bind_map_location f x = bind_location (Location.map f x)

let bind_and (a, b) =
  a >>? fun a ->
  b >>? fun b ->
  ok (a, b)

let bind_and3 (a, b, c) =
  a >>? fun a ->
  b >>? fun b ->
  c >>? fun c ->
  ok (a, b, c)

let bind_pair = bind_and

let bind_map_pair f (a, b) =
  bind_pair (f a, f b)
