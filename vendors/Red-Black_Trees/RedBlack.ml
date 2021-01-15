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


exception Not_a_node
let empty = Ext

let is_empty m = (m = empty)

let blacken = function
  Ext -> Ext
| Int (_, left, root, right) -> Int (Black, left, root, right)

let paint red = function
  Ext -> raise Not_a_node
| Int (_, left, root, right) -> Int (red, left, root, right)


(* Standard Balance function *)
let balance colour left root right =
  match colour, left, root, right with
  (* Case 3: P is red and U is red *)
    Black, Int (Red, Int (Red, a, x, b), y, c), z, (Int (Red,_,_,_) as d) ->
      Int (Red, Int (Black, Int (Red, a, x, b), y, c), z, blacken d)
  | Black, Int (Red, a, x, Int (Red, b, y, c)), z, (Int (Red,_,_,_) as d) ->
      Int (Red, Int (Black, a, x, Int (Red, b, y, c)), z, blacken d)
  | Black, (Int (Red,_,_,_) as a), x, Int (Red, Int (Red, b, y, c), z, d) ->
      Int (Red, blacken a, x, Int (Black, Int (Red, b, y, c), z, d))
  | Black, (Int (Red,_,_,_) as a), x, Int (Red, b, y, Int (Red, c, z, d)) ->
      Int (Red, blacken a, x, Int (Black, b, y, Int (Red, c, z, d)))
  (* Case 4: P is red and U is black*)
  | Black, Int (Red, Int (Red, a, x, b), y, c), z, (Int (Black,_,_,_) | Ext as d) ->
      Int (Black, Int (Red, a, x, b), y, Int (Red,c, z, d))
  | Black, Int (Red, a, x, Int (Red, b, y, c)), z, (Int (Black,_,_,_) | Ext as d) ->
      Int (Black, Int (Red, a, x, b), y, Int (Red,c, z, d))
  | Black, (Int (Black,_,_,_) | Ext as a), x, Int (Red, Int (Red, b, y, c), z, d) ->
      Int (Black, Int (Red, a, x, b), y, Int (Red,c, z, d))
  | Black, (Int (Black,_,_,_) | Ext as a), x, Int (Red, b, y, Int (Red, c, z, d)) ->
      Int (Black, Int (Red, a, x, b), y, Int (Red,c, z, d))
  | _ ->
      Int (colour, left, root, right)

(* Alternative blance function 
let balance colour left root right =
  match colour, left, root, right with
    Black, Int (Red, Int (Red, a, x, b), y, c), z, d
  | Black, Int (Red, a, x, Int (Red, b, y, c)), z, d
  | Black, a, x, Int (Red, Int (Red, b, y, c), z, d)
  | Black, a, x, Int (Red, b, y, Int (Red, c, z, d)) ->
      Int (Red, Int (Black, a, x, b), y, Int (Black, c, z, d))
  | _ ->
      Int (colour, left, root, right)
*)

type choice = Old | New

let choose ~old ~new' = function
  Old -> old
| New -> new'

exception Physical_equality

let add ?debug ~cmp choice elt tree =
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
  (match debug with Some (debug) -> Format.printf "Adding to tree:%a\n%!" (pp debug) tree | None -> ());
  let tree = try blacken (insert tree) with
       Physical_equality -> tree
  in (match debug with Some (debug) -> Format.printf "New tree:%a\n%!" (pp debug) tree | None -> ()); tree

let remove : type a b . ?debug:(Format.formatter -> b -> unit) -> cmp:(a -> b -> int) -> a -> b t -> b t = fun ?debug ~cmp elt tree ->
  (* The removal in a reb black tree is similar to the removal in a binary tree :
     In order to delet a node, it value is replace by the inorder sucessor in the tree, 
     which is the left most value of it's right substree. Then this node is remove 
     from the tree and replace by its right subtree. 
     Following this operation, the color property has to restore in this subtree
     *)
  (* TODO: restoring the color property. *)
  (* Search for the inorder successor *)  
  let insert_left ~left node parent =
    match parent with
      Ext -> node
    | Int (colour,l,root,r) ->
      if left then
        Int (colour,node,root,r)
      else
        Int (colour,l,root,node)
      in

  let insert_right ~right parent node =
    match parent with
      Ext -> node
    | Int (colour,l,root,r) ->
      if right then
        Int (colour,l,root,node)
      else
        Int (colour,node,root,r)
      in

    let get_colour n = match n with Ext -> Black
                      | Int (colour,_,_,_) -> colour in
    let get_left n = match n with Ext -> failwith "Get_left not a node"
                      | Int (_,left,_,_) -> left in
    let get_right n = match n with Ext -> failwith "Get_right not a node"
                      | Int (_,_,_,right) -> right in
    let get_child ~right = 
      if right then get_right else get_left in

  (* return the updated parent or current if current is root*)
  let rec bst_remove : bool -> b t -> b t -> b t = fun left parent -> function
    | Ext -> failwith "unknown error"
    | Int (colour,l, root, r) as current -> (
       (match debug with Some (debug) -> Format.printf "Foud node to remove %a\nwith parent: %a\n%!" (pp debug) current (pp debug) parent | None -> ());
        ignore root; (* Deletion *)
        match l, r with 
        (* No child, just delete *)
        | Ext, Ext -> (
            (match debug with Some (_debug) -> Format.printf "Removing the leaf\n" | None -> ());
            let res = bst_remove_leftmost ~left parent colour Ext in
            (match debug with Some (debug) -> Format.printf "Returns with parent :%a\n%!" (pp debug) res | None -> ()); 
            res
          )
         (* If this is the highr value, there is only one child, so move the tree up,
          then restore color property *)
        | Int (_lcolor, _lleft, _lroot, _lright), Ext ->
          (match debug with Some (_debug) -> Format.printf "No inorder successor\n" | None -> ());
          bst_remove_leftmost ~left:(false) parent colour l
         (* The inorder value is the right child*)
        | _, Int (_,Ext,_,_) ->
          (match debug with Some (_debug) -> Format.printf "Inorder succesor is right child\n" | None -> ());
          (match debug with Some (debug) -> Format.printf "Left child %a\n" (pp debug) l | None -> ());
          let new_current = bst_remove_leftmost ~left:(false) parent colour r in
          (match debug with Some (debug) -> Format.printf "New_current %a\n" (pp debug) new_current | None -> ());
          let new_current = match parent with Ext -> new_current | _ -> get_right new_current in
          let new_current = insert_left ~left:(true) l new_current in
          insert_right ~right:true parent new_current
         (* Get the next value, then put it at the place of the element you are removing and remove this element *)
        | _, Int (_rcolor,rleft,_rroot,_rright) ->
          (match debug with Some (_debug) -> Format.printf "Search for inorder successor\n" | None -> ());
          let new_root, new_right = bst_find_leftmost r rleft in
          let new_current = Int (colour,l,new_root,new_right) in
          insert_left ~left new_current parent
    )
  
  (* remove the node and restore the color property *)
  (* return the parent of the node *)
  and bst_find_leftmost : b t -> b t -> b * b t = fun parent -> function
    | Ext -> failwith "unknow error"
    | Int  (colour, left, root, right) as current ->
      match left with
      | Int _ -> 
        (* Continue searching*) 
        let root, new_current =  bst_find_leftmost current left in
        root, insert_left ~left:true new_current @@ parent
      | Ext ->
        let parent = bst_remove_leftmost ~left:true parent colour right in
        root, parent

  (* this function return the parent of the node modified except if this is the root *)
  and bst_remove_leftmost ~left parent old_colour new_node (* N *) =
    let right = left in
    match old_colour,get_colour new_node with
    | Red, Red -> 
      failwith "Bad tree : a red node has to be followed by two black node"
    (* If the old one is red an the new is Black, move it up *)
    | Red, Black -> (
      (match debug with Some (debug) -> Format.printf "Old is red and New is %a\n" (pp debug) new_node | None -> ());
      (match debug with Some (debug) -> Format.printf "Parent is : %a\n" (pp debug) parent | None -> ());
      let res = insert_left ~left new_node parent in
      (match debug with Some (debug) ->  Format.printf "Returns with parent :%a\n%!" (pp debug) res | None -> ());
      res)
    (* If the old node is black and the new is red, repaint the node *)
    | Black, Red -> 
      (match debug with Some (debug) -> Format.printf "Old is black and New is %a\n" (pp debug) new_node | None -> ());
      (match debug with Some (debug) -> Format.printf "Parent is : %a\n" (pp debug) parent | None -> ());
      let new_parent = insert_left ~left (blacken @@ new_node) parent in
      (match debug with Some (debug) ->  Format.printf "Returns with parent :%a\n%!" (pp debug) new_parent | None -> ());
      new_parent
    (* if both nodes are black, things get complicated *)
    | Black, Black -> 
      (match debug with Some (debug) -> Format.printf "Old is black and New is %a\n" (pp debug) new_node | None -> ());
      match parent (* P *) with
        (* Case 1 : N is the new root *)
        Ext -> 
          (match debug with Some (_debug) -> Format.printf "Case 1\n" | None -> ());
          blacken new_node
        (* other casese*)
      | _ ->
      let s  = get_child ~right parent in (* This is the sibling of N *)
      let ss = get_child ~right:(not right) s in (* This is the child of S on the same side of N to P  (if N is left Ss is left)*)
      let sc = get_child ~right s in (* This is the child of S on the conter side of N to P (if N is left Sc is right) *)
      match get_colour parent,get_colour s,get_colour ss, get_colour sc with
        (* Case 2 : pright is red, parent has to be black *)
      | Black, Red, Black, Black->
          (match debug with Some (_debug) -> Format.printf "Case 2\n" | None -> ());
        let parent = paint Red @@ insert_left ~left new_node (insert_right ~right parent ss) in (
          (match debug with Some (debug) -> Format.printf "Parent :\n %a\n%!" (pp debug) parent | None -> ());

          (* Proceed with case 4, 5 or 6, the 1,2,3 are not possible as the parent is now red*)
          let new_parent = bst_remove_leftmost ~left parent old_colour new_node in

          (match debug with Some (debug) -> Format.printf "New Parent :\n %a\n%!" (pp debug) new_parent | None -> ());
          blacken @@ insert_left ~left new_parent (insert_right ~right s sc)
        )

        (* Case 3 : pright is black and its children are black*)
      | Black, Black, Black, Black ->
          (match debug with Some (_debug) -> Format.printf "Case 3\n" | None -> ());
          paint Black @@ insert_left ~left new_node @@ insert_right ~right parent @@ paint Red s

        (* Case 4 : prigt children's are black but parent is red*)
      | Red, Black, Black, Black ->
          (match debug with Some (_debug) -> Format.printf "Case 4\n" | None -> ());
          blacken @@ insert_left ~left new_node @@ insert_right ~right parent @@ paint Red s

        (* Case 5 : pright is black, prl is red but prr is black *)
      | colour, Black, Red, Black ->
          (* since ss is red it as two children sss and ssc*)
          (match debug with Some (_debug) -> Format.printf "Case 5\n" | None -> ());
          let ssc,sss = get_child ~right ss, get_child ~right:(not left) ss in

          (* Rotate right at s and change color to make new sc red*)
          (* Int (colour,new_node,get_root parent, blacken @@ insert_left ~left sss @@ insert_right ~right ss @@ paint Red @@insert_left ~left ssc @@ insert_right ~right s sc ) *)

          (*then apply case 6*)
          paint colour 
            @@ insert_left ~left (paint Black @@ insert_left ~left new_node @@ insert_right ~right parent sss) 
            @@ insert_right ~right ss
            @@ paint Black @@ insert_left ~left ssc @@ insert_right ~right s sc

        (* Case 6 : pright is black, prr is red*)
      | colour, Black, _, Red ->
          (match debug with Some (_debug) -> Format.printf "Case 6\n" | None -> ());

          paint colour
            @@ insert_left ~left(paint Black @@ insert_left ~left new_node @@ insert_right ~right parent ss)
            @@ insert_right ~right s
            @@ paint Black @@ sc
      
        (* Wrong trees *)
      | Red, Red, _, _  ->
        failwith "Bad tree : a red node has to be followed by two black node"
      | Black, Red, Black, Red ->
        failwith "Bad tree : a red node has to be followed by two black node"
      | Black, Red, Red, _ ->
        failwith "Bad tree : a red node has to be followed by two black node"
      
      
  in
  (* Search the node to remove *)
  let rec bst_delete : bool -> a -> b t -> b t -> b t = fun l elt parent -> function
    | Ext -> raise Not_found
    | Int (_, left, root, right) as current ->
      let c = cmp elt root in
      if      c = 0 then bst_remove l parent current
      else if c < 0 then (
        (match debug with Some (debug) -> Format.printf "Going to the left in subtree :\n %a\nnew parent: %a\n%!" (pp debug) left (pp debug) current | None -> ());
        let new_parent = insert_left ~left:l (bst_delete true elt current left) parent in
        (match debug with Some (debug) -> Format.printf "Returning with tree: %a\n%!" (pp debug) new_parent | None -> ());
        new_parent
      )
      else (
        (match debug with Some (debug) -> Format.printf "Going to the right in subtree :\n %a\nnew parent: %a\n%!" (pp debug) right (pp debug) current | None -> ());
        let new_parent = insert_right ~right:(not l) parent @@ bst_delete false elt current right in
        (match debug with Some (debug) -> Format.printf "Returning with tree: %a\n%!" (pp debug) new_parent | None -> ());
        new_parent
      )
  in
  (match debug with Some (debug) -> Format.printf "\nTree to remove from : %a\n%!" (pp debug) tree | None -> ());
  let tree = try bst_delete true elt Ext tree
  with Not_found -> tree
  in (
  (match debug with Some (debug) -> Format.printf "Tree after remove : %a\n%!" (pp debug) tree | None -> ());
  tree)

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
