open Trace
open Test_helpers

let redblack () =
  let open RedBlackTrees in
  Random.self_init ();
  let tree = RedBlack.empty in
  let rec aux lst e = 
    if e > 10 then
      lst
    else 
      let lst = (Random.int 100)::lst in
      aux lst @@ e+1
  in
  let lst = aux [] 1 in
  (*Add test*)
  let tree = List.fold_left (
    fun tree e ->
      RedBlack.add ~debug:(fun ppf i -> Format.fprintf ppf "%i" i) ~cmp:(-) RedBlack.New e tree
  ) tree lst in
  (* Removal test *)
  let _ = List.fold_left (
    fun tree e ->
      RedBlack.remove ~debug:(fun ppf i -> Format.fprintf ppf "%i" i) ~cmp:(-) e tree
  ) tree lst in
  ok ()


let main = test_suite "Vendors" [
    test "RedblackTree1" redblack ;
    test "RedblackTree2" redblack ;
    test "RedblackTree3" redblack ;
    test "RedblackTree4" redblack ;
    test "RedblackTree5" redblack ;
    test "RedblackTree6" redblack ;
    test "RedblackTree7" redblack ;
    test "RedblackTree8" redblack ;
    test "RedblackTree9" redblack ;
]
