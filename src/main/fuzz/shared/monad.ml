open QCheck

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val get_one : ?n:int -> 'a t -> 'a
  val get_list : ?n:int -> 'a t -> 'a list
  val oneof : 'a t list -> 'a t
  val mutate_int : int -> int t
  val mutate_nat : int -> int t
  val mutate_string : string -> string t
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
end

module Rnd : Monad = struct
  type 'a t = 'a Gen.t
  
  let return x = Gen.return x
  let (let*) x f = Gen.(x >>= f)
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
  let get_list ?(n = 100) l =
    Gen.generate ~n l
  let oneof l = Gen.oneof l
  let mutate_int z = Gen.oneof [Gen.return z; Gen.small_int]
  let mutate_nat n = Gen.oneof [Gen.return n; Gen.big_nat]
  let mutate_string s = Gen.oneof [Gen.return s; Gen.small_string ~gen:Gen.printable]
end

module Lst : Monad = struct
  type 'a t = 'a list
  
  let return x = [x]
  let (let*) x f = List.concat (List.map f x)
  let get_one ?(n = 0) l = List.nth l n
  let get_list ?n l =
    let n = Option.unopt ~default:(List.length l) n in
    List.take n l
  let oneof l = List.concat l
  let mutate_int n = [n]
  let mutate_nat n = [n]
  let mutate_string s = [s]
end

module Monad_context (M : Monad) = struct
  include M

  let bind_location (x:_ Location.wrap) =
    let* wrap_content = x.wrap_content in
    return { x with wrap_content }

  let rec bind_list = function
      [] -> return []
    | hd::tl -> let* hd = hd in
                let* tl = bind_list tl in
                return @@ hd :: tl
  
  let bind_map_list f lst = bind_list (List.map f lst)
  
  let bind_map_option f = function
      None -> return None
    | Some s -> let* x = f s in
                return (Some x)
  
  let bind_map_location f x = bind_location (Location.map f x)
  
  let bind_and (a, b) =
    let* a = a in
    let* b = b in
    return (a, b)
  
  let bind_and3 (a, b, c) =
    let* a = a in
    let* b = b in
    let* c = c in
    return (a, b, c)
  
  let bind_pair = bind_and
  
  let bind_map_pair f (a, b) =
    bind_pair (f a, f b)

  let bind_fold_list f init lst =
    let aux x y =
      let* x = x in f x y
    in List.fold_left aux (return init) lst

  let bind_fold_ne_list f init lst =
    let aux x y =
      let* x = x in f x y
    in Simple_utils.List.Ne.fold_left aux (return init) lst

  let bind_ne_list (hd, tl) =
    let* hd = hd in
    let* tl = bind_list tl in
    return @@ (hd, tl)

  let bind_map_ne_list : _ -> 'a Simple_utils.List.Ne.t -> 'b Simple_utils.List.Ne.t t =
    fun f lst -> bind_ne_list (Simple_utils.List.Ne.map f lst)

  let map f x =
    let* x = x in
    return (f x)
end
