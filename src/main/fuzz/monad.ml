open QCheck

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val get_one : ?n:int -> 'a t -> 'a
  val oneof : 'a t list -> 'a t
  val mutate_int : int -> int t
  val mutate_nat : int -> int t
  val mutate_string : string -> string t
  
  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    
    module Open_on_rhs : sig
      val return : 'a -> 'a t
    end
  end
end

module Rnd : Monad = struct
  type 'a t = 'a Gen.t
  
  let return x = Gen.return x
  let bind x ~f = Gen.(x >>= f)
  let map x ~f = Gen.map f x
  let get_one ?n x =
    let rand = match n with
      | None -> Random.State.make_self_init()
      | Some seed ->
         let curr = Random.get_state () in
         Random.init seed;
         let rand = Random.get_state () in
         Random.set_state curr;
         rand in
    Gen.generate1 ~rand x
  let oneof l = Gen.oneof l
  let mutate_int _ = Gen.oneof [Gen.small_int; Gen.int]
  let mutate_nat _ = Gen.big_nat
  let mutate_string _ = Gen.small_string ~gen:Gen.printable
  
  module Let_syntax = struct
    let return = return
    let bind = bind
    let map = map
    
    module Open_on_rhs = struct
      let return = return
    end
  end
end

module Lst : Monad = struct
  type 'a t = 'a list
  
  let return x = [x]
  let bind x ~f = List.concat (List.map f x)
  let map x ~f = List.map f x
  let get_one ?(n = 0) l = List.nth l n
  let oneof l = List.concat l
  let mutate_int n = [n]
  let mutate_nat n = [n]
  let mutate_string s = [s]
  
  module Let_syntax = struct
    let return = return
    let bind = bind
    let map = map
    
    module Open_on_rhs = struct
      let return = return
    end
  end
end

module Monad_context (M : Monad) = struct
  include M

  let bind_location (x:_ Location.wrap) =
    let%bind wrap_content = x.wrap_content in
    return { x with wrap_content }

  open Ast_imperative
  let bind_lmap (l:_ label_map) =
    let open LMap in
    let aux k v prev =
      let%bind prev' = prev in
      let%bind v' = v in
      return @@ add k v' prev' in
    fold aux l (return empty)
  
  let bind_map_lmap f map = bind_lmap (LMap.map f map)
  
  let rec bind_list = function
      [] -> return []
    | hd::tl -> let%bind hd = hd in
                let%bind tl = bind_list tl in
                return @@ hd :: tl
  
  let bind_map_list f lst = bind_list (List.map f lst)
  
  let bind_map_option f = function
      None -> return None
    | Some s -> let%bind x = f s in
                return (Some x)
  
  let bind_map_location f x = bind_location (Location.map f x)
  
  let bind_and (a, b) =
    let%bind a = a in
    let%bind b = b in
    return (a, b)
  
  let bind_and3 (a, b, c) =
    let%bind a = a in
    let%bind b = b in
    let%bind c = c in
    return (a, b, c)
  
  let bind_pair = bind_and
  
  let bind_map_pair f (a, b) =
    bind_pair (f a, f b)
end
