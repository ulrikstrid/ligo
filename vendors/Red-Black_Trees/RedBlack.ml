(* Red-black trees according to the following classic paper:

   Chris Okasaki, Red-Black Trees in a Functional
   Setting. J. Funct. Program. 9(4): 471-477 (1999)
*)

type colour = NegBlack | Red | Black | DoubleBlack

type 'a t =
  L
| BBL (*Double black leaf*)
| T of colour * 'a t * 'a * 'a t

let empty = L

let is_empty m = (m = empty)

let blacken = function
  L   -> L
| BBL -> L
| T (_, left, root, right) -> T (Black, left, root, right)

let redden = function
  L | BBL -> failwith "Can't redden a leaf"
| T (_, left, root, right) -> T (Red, left, root, right)

let rec pp f ppf = function
  L   -> Format.fprintf ppf "L (Black)"
| BBL -> Format.fprintf ppf "BBL (Black-Black)"
| T (c, l, root, r) ->
    Format.fprintf ppf "Int (%s,%a,%a,%a)"
    (match c with NegBlack -> "-Black" | Red -> "Red" | Black -> "Black" | DoubleBlack -> "Black-Black")
    (pp f) l
    f root
    (pp f) r

let blackInc = function
  NegBlack -> Red
| Red      -> Black
| Black    -> DoubleBlack
| DoubleBlack -> failwith "can't increase doubleBlack"

let blackIncNode = function
  T (c,l,v,r) -> T (blackInc c,l,v,r)
| L   -> BBL
| BBL -> failwith "can't increase a black-black leaft"

let blackDec = function
  DoubleBlack -> Black
| Black       -> Red
| Red         -> NegBlack
| NegBlack    -> failwith "can't decrease -Black"

let blackDecNode = function
  T (c,l,v,r) -> T (blackDec c,l,v,r)
| L -> failwith "can't decrease a black leaft"
| BBL -> L

let is_double_black = function
  BBL | T (DoubleBlack, _,_,_) -> true
| _ -> false

let is_legal tree =
  let rec max_black_height = function
    T (c, l, _, r) -> 
      let max = max (max_black_height l) (max_black_height r) in
      if c == Black then max + 1 else max
  | L -> 1
  | BBL -> failwith "Tree malformed"
  and min_black_height = function
    T (c, l, _, r) -> 
      let min = min (min_black_height l) (min_black_height r) in
      if c == Black then min + 1 else min
  | L -> 1
  | BBL -> failwith "Tree malformed"
  and is_balanced tree = 
    let max = max_black_height tree in
    let min = min_black_height tree in
    Format.printf "Max : %d, min : %d\n%!" max min;
    max = min
  and no_red_red = function
    T (Black, l, _, r) 
  | T (Red, (T (Black,_,_,_)| L as l), _, (T(Black,_,_,_) | L as r))
    -> no_red_red l && no_red_red r
  | L -> true
  | _ -> false
  in 
  if not @@ no_red_red tree then failwith "Red_red";
  if not @@ is_balanced tree then failwith "Unbalanced tree";
  true

let max = function
  | L | BBL -> None
  | T (_,_,x,l) ->
  let rec max x = function
    | L | BBL -> Some x
    | T (_,_,x,l) -> max x l
  in max x l

let count tree =
  let count = 0 in
  let rec aux count = function
    | L -> count
    | BBL -> failwith "impossible"
    | T (_, l,_,r) -> aux (aux (count + 1) l) r
  in aux count tree

let blackDepth tree =
  let rec aux count = function
    | L -> count + 1
    | BBL -> failwith "impossible"
    | T (NegBlack   , l, _, _) -> aux (count - 1) l
    | T (Red        , l, _, _) -> aux count l
    | T (Black      , l, _, _) -> aux (count + 1) l
    | T (DoubleBlack, l, _, _) -> aux (count + 2) l
  in aux 0 tree

exception Not_equal
let rec checkDepth = function 
  | L -> 1
  | BBL -> 2
  | T (colour, l, _, r) ->
      let lcount = checkDepth l in
      let rcount = checkDepth r in
      if lcount <> rcount then raise Not_equal
      else match colour with
        NegBlack    -> lcount - 1
      | Red         -> lcount
      | Black       -> lcount + 1
      | DoubleBlack -> lcount + 2

let checkDepth_opt tree =
  try Some (checkDepth tree) with 
    Not_equal -> None


let rec balance_node = function (* n -> n *)
  | T (Black | DoubleBlack as k, T (Red, T (Red, a, x, b), y, c), z ,d)
  | T (Black | DoubleBlack as k, T (Red, a, x, T (Red, b, y, c)), z, d)
  | T (Black | DoubleBlack as k, a, x, T (Red, T (Red, b, y, c), z, d))
  | T (Black | DoubleBlack as k, a, x, T (Red, b, y, T (Red, c, z, d)))
    -> T (blackDec k, T (Black, a, x, b), y, T (Black, c, z, d))
  
  | T (DoubleBlack, a, x, T (NegBlack, T (Black, b, y, c), z, (T (Black, _, _, _) | L as d)))
    -> T (Black, T (Black, a, x, b), y, balance Black c z (redden d))
  
  | T (DoubleBlack, T (NegBlack, (T (Black, _, _, _) | L as a), x, T (Black, b, y, c)), z, d)
    -> T (Black, balance Black (redden a) x b, y, T (Black, c, z, d))
  | t -> t

and balance c l v r =
  balance_node @@ T (c, l, v, r)

(* Okasaki's balance function
let balance colour left(*n*) root right(*n*) =
  match colour, left, root, right with
    Black, T (Red, T (Red, a, x, b), y, c), z, d
  | Black, T (Red, a, x, T (Red, b, y, c)), z, d
  | Black, a, x, T (Red, T (Red, b, y, c), z, d)
  | Black, a, x, T (Red, b, y, T (Red, c, z, d)) ->
      T (Red, T (Black, a, x, b), y, T (Black, c, z, d)) (*n+1*)
  | _ ->
      T (colour, left, root, right) (*n+1 if color black*)
*)

type choice = Old | New

let choose ~old ~new' = function
  Old -> old
| New -> new'

exception Physical_equality

let add ?debug:_ ~cmp choice elt tree =
  let rec insert = function
    L -> T (Red, L, elt, L)  (* A leaf *)
  | BBL -> failwith "impossible"
  | T (colour, left, root, right) ->
      let diff = cmp elt root in
      if diff = 0 then
        let root' = choose ~new':elt ~old:root choice
        in if root == root' then raise Physical_equality
           else T (colour, left, root', right)
      else if diff < 0 then 
        balance colour (insert left) root right
      else 
        balance colour left root (insert right)
  in 
  let tree = try blacken (insert tree) with
    Physical_equality -> tree
  in
  tree


let remove : type a b . ?debug:(Format.formatter -> b -> unit) -> cmp:(a -> b -> int) -> a -> b t -> b t = 
fun ?debug:_ ~cmp x tree ->
  let rec del = function
    | L | BBL -> raise Not_found 
    | T (k, l, y ,r) as nod -> 
        let c = cmp x y in
        if c < 0 then bubble k (del l) y r
        else if c > 0 then bubble k l y (del r)
        else remove nod (*n-1 if c = B, n if c = R*)
  and remove = function (* n -> n *)
  (* Remove a leaf *)
  | T (Red  , L, _, L) -> L
  | T (Black, L, _, L) -> BBL
  (* Only one child the parent or child is red *)
  | T (Red  , child, _, L)
  | T (Red  , L, _, child) -> child
  | T (Black, T (Red, l, v, r), _, L)
  | T (Black, L, _, T (Red, l, v, r)) -> T (Black, l, v, r)
  (* Only one black child*)
  | T (Black, (T(Black,_,_,_) as child), _, L)
  | T (Black, L, _, (T (Black,_,_,_) as child)) -> blackIncNode child
  (* Two sub-trees*)
  | T (c, l, _, r) ->
    let v = Option.get @@ max l in
    let l' = remove_max l in
    bubble c l' v r
  | L | BBL -> failwith "impossible"
  
  and bubble c l v r =
    if is_double_black l || is_double_black r
      then balance (blackInc c) (blackDecNode l) v (blackDecNode r)
    else T (c, l, v, r)
  and remove_max = 
  function
    T (_,_,_,L) as n -> remove n
  | T (c,l,v,r) -> bubble c l v @@ remove_max r
  | L | BBL -> failwith "impossible"
  in blacken @@ del tree


let rec find ~cmp elt = function
  L -> (
    raise Not_found)
| BBL -> failwith "impossible"
| T (_, left, root, right) ->
    let diff = cmp elt root in
    if diff = 0 then (root)
    else if diff < 0 
      then (find ~cmp elt left)
      else (find ~cmp elt right)

let find_opt ~cmp elt tree =
  try Some (find ~cmp elt tree) with Not_found -> None

(* Inorder iterators *)

let rec iter f = function
  L | BBL -> ()
| T (_, left, root, right) -> iter f left; f root; iter f right

let rec inorder acc = function
  L | BBL -> acc
| T (_, left, root, right) -> inorder (root :: inorder acc right) left

let elements t = inorder [] t

let union ~cmp choice (tree_a : 'a t) tree_b =
  List.fold_left
    (fun acc elt -> add ~cmp choice elt acc)
    tree_b
    (elements tree_a)
    
let rec fold_inc f ~init = function
  L | BBL -> init
| T (_, left, root, right) ->
    fold_inc f ~init:(f ~elt:root ~acc:(fold_inc f ~init left)) right

let rec fold_dec f ~init = function
  L | BBL -> init
| T (_, left, root, right) ->
    fold_dec f ~init:(f ~elt:root ~acc:(fold_dec f ~init right)) left
