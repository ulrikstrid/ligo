(* Red-black trees according to the following classic paper:

   Chris Okasaki, Red-Black Trees in a Functional
   Setting. J. Funct. Program. 9(4): 471-477 (1999)
*)

type colour = Red | Black

type 'a t =
  Ext
| Int of colour * 'a t * 'a * 'a t

let rec pp f ppf = function
  Ext -> Format.fprintf ppf "Ext (Black)"
| Int (c, l, root, r) ->
    Format.fprintf ppf "Int (%s,%a,%a,%a)"
    (match c with Red -> "Red" | Black -> "Black")
    (pp f) l
    f root
    (pp f) r

let empty = Ext

let is_empty m = (m = empty)

let blacken = function
  Ext -> Ext
| Int (_, left, root, right) -> Int (Black, left, root, right)


(* Okasaki's balance function *) 
let balance colour left root right =
  match colour, left, root, right with
    Black, Int (Red, Int (Red, a, x, b), y, c), z, d
  | Black, Int (Red, a, x, Int (Red, b, y, c)), z, d
  | Black, a, x, Int (Red, Int (Red, b, y, c), z, d)
  | Black, a, x, Int (Red, b, y, Int (Red, c, z, d)) ->
      Int (Red, Int (Black, a, x, b), y, Int (Black, c, z, d))
  | _ ->
      Int (colour, left, root, right)

type choice = Old | New

let choose ~old ~new' = function
  Old -> old
| New -> new'

exception Physical_equality

let add ?debug:_ ~cmp choice elt tree =
  let rec insert = function
    Ext -> Int (Red, Ext, elt, Ext)  (* A leaf *)
  | Int (colour, left, root, right) ->
      let diff = cmp elt root in
      if diff = 0 then
        let root' = choose ~new':elt ~old:root choice
        in if root == root' then raise Physical_equality
           else Int (colour, left, root', right)
      else if diff < 0 then 
        balance colour (insert left) root right
      else 
        balance colour left root (insert right)
  in 
  try blacken (insert tree) with
    Physical_equality -> tree

let remove : type a b . ?debug:(Format.formatter -> b -> unit) -> cmp:(a -> b -> int) -> a -> b t -> b t = 
fun ?debug:_ ~cmp elt tree ->
  let rec del x = function
    | Ext -> raise Not_found 
    | Int (_, l, y ,r) as t -> 
        (* (match debug with Some (debug) -> Format.printf "Foud node to remove %a\n%!" (pp debug) t | None -> ()); *)
        let c = cmp x y in
        if c < 0 then delL x t
        else if c > 0 then delR x t
        else fuse (l,r)

  and delL x = function
    | Int (Black, t1, y, t2) -> balL @@ Int (Black, (del x t1), y, t2)
    | Int (Red, t1, y, t2) -> Int (Red, (del x t1), y, t2)
    | Ext -> failwith "Impossible"
  and balL = function
    | Int (Black, Int (Red, t1, x, t2), y, t3) -> Int (Red, Int (Black, t1, x, t2), y, t3)
    | Int (Black, t1, y, Int (Black, t2, z, t3)) -> balance Black t1 y @@ Int (Red, t2, z, t3)
    | Int (Black, t1, y, Int (Red, Int (Black, t2, u, t3), z, Int(Black, l, value, r)))
      -> Int (Red, Int (Black, t1, y, t2), u, (balance Black t3 z @@ Int (Red, l, value, r)))
    | t -> t
  and delR x = function
    | Int (Black, t1, y, t2) -> balR @@ Int (Black, t1, y, (del x t2))
    | Int (Red,   t1, y, t2) -> Int (Red, t1, y, (del x t2))
    | Ext -> failwith "Impossible"
  and balR = function
    | Int (Black, t1, y, Int(Red, t2, x, t3)) -> Int (Red, t1, y, Int (Black, t2, x, t3))
    | Int (Black, Int (Black, t1, z, t2), y, t3) -> balance Black (Int (Red, t1, z, t2)) y t3
    | Int (Black, Int (Red, Int (Black, l, value, r), z, Int (Black, t2, u, t3)), y, t4)
      -> Int (Red, (balance Black (Int (Red, l, value, r)) z t2), u, Int (Black, t3, y, t4))
    | t -> t
    
  and fuse = function
    | Ext, t | t, Ext -> t
    | Int (Black, _, _, _) as t1, Int (Red, t3, y, t4) -> Int (Red, (fuse (t1, t3)), y, t4)
    | Int (Red, t1, x, t2), (Int (Black, _, _, _) as t3) -> Int (Red, t1, x, (fuse (t2, t3)))
    | Int (Red, t1, x, t2), Int (Red, t3, y, t4) -> (
      match fuse (t2, t3) with 
        | Int (Red, s1, z, s2) -> Int (Red, Int (Red, t1, x, s1), z, Int (Red, s2, y, t4))
        | (Int (Black, _, _, _) | Ext) as s -> Int (Red, t1, x, Int (Red, s, y, t4))
    )
    | Int (Black, t1, x, t2), Int (Black, t3, y, t4) ->
      match fuse (t2, t3) with
        | Int (Red, s1, z, s2) -> Int (Red, Int (Black, t1, x, s1), z, Int (Black, s2, y, t4))
        | (Int (Black, _, _, _) | Ext) as s -> balL @@ Int (Black, t1, x, Int (Black, s, y, t4))
  in
  blacken @@ del elt tree

let rec find ~cmp elt = function
  Ext -> (
    raise Not_found)
| Int (_, left, root, right) ->
    let diff = cmp elt root in
    if diff = 0 then (root)
    else if diff < 0 
      then (find ~cmp elt left)
      else (find ~cmp elt right)

let find_opt ~cmp elt tree =
  try Some (find ~cmp elt tree) with Not_found -> None

(* Inorder iterators *)

let rec iter f = function
                         Ext -> ()
| Int (_, left, root, right) -> iter f left; f root; iter f right

let rec inorder acc = function
                         Ext -> acc
| Int (_, left, root, right) -> inorder (root :: inorder acc right) left

let elements t = inorder [] t

let union ~cmp choice (tree_a : 'a t) tree_b =
  List.fold_left
    (fun acc elt -> add ~cmp choice elt acc)
    tree_b
    (elements tree_a)
    
let rec fold_inc f ~init = function
                         Ext -> init
| Int (_, left, root, right) ->
    fold_inc f ~init:(f ~elt:root ~acc:(fold_inc f ~init left)) right

let rec fold_dec f ~init = function
                         Ext -> init
| Int (_, left, root, right) ->
    fold_dec f ~init:(f ~elt:root ~acc:(fold_dec f ~init right)) left
