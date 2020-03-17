open Trace
open Types

open Stage_common.Helpers
module Errors = struct
  type 'a assert_eq_tracer = { a:expression ; b:expression ; error : 'a }

  let different_literals_type  a b = `Core_Different_literals_types (a,b)
  let different_literals_value a b = `Core_Different_literals_value (a,b)
  let uncomparable_literals a b    = `Core_Uncomparable_literals (a,b)

  let uncomparable a b             = `Core_Uncomparable (a,b)
  let not_a_value a                = `Core_Not_a_value a
  let not_equal_tracer a b error   = `Core_Not_equal { a ; b ; error }

  (* let different_literals_because_different_types name a b () =
    let title () = "literals have different types: " ^ name in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.literal a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.literal b )
    ] in
    error ~data title message () *)

  (* let different_literals name a b () =
    let title () = name ^ " are different" in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.literal a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.literal b )
    ] in
    error ~data title message () *)

  (* let error_uncomparable_literals name a b () =
    let title () = name ^ " are not comparable" in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.literal a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.literal b )
    ] in
    error ~data title message () *)
end
open Errors

let assert_literal_eq (a, b : literal * literal) : (unit,_) result =
  match (a, b) with
  | Literal_bool a, Literal_bool b when a = b -> ok ()
  | Literal_bool _, Literal_bool _ -> fail @@ different_literals_value a b
  | Literal_bool _, _ -> fail @@ different_literals_type a b
  | Literal_int a, Literal_int b when a = b -> ok ()
  | Literal_int _, Literal_int _ -> fail @@ different_literals_value a b
  | Literal_int _, _ -> fail @@ different_literals_type a b
  | Literal_nat a, Literal_nat b when a = b -> ok ()
  | Literal_nat _, Literal_nat _ -> fail @@ different_literals_value a b
  | Literal_nat _, _ -> fail @@ different_literals_type a b
  | Literal_timestamp a, Literal_timestamp b when a = b -> ok ()
  | Literal_timestamp _, Literal_timestamp _ -> fail @@ different_literals_value a b
  | Literal_timestamp _, _ -> fail @@ different_literals_type  a b
  | Literal_mutez a, Literal_mutez b when a = b -> ok ()
  | Literal_mutez _, Literal_mutez _ -> fail @@ different_literals_value a b
  | Literal_mutez _, _ -> fail @@ different_literals_type  a b
  | Literal_string a, Literal_string b when a = b -> ok ()
  | Literal_string _, Literal_string _ -> fail @@ different_literals_value a b
  | Literal_string _, _ -> fail @@ different_literals_type  a b
  | Literal_bytes a, Literal_bytes b when a = b -> ok ()
  | Literal_bytes _, Literal_bytes _ -> fail @@ different_literals_value a b
  | Literal_bytes _, _ -> fail @@ different_literals_type  a b
  | Literal_void, Literal_void -> ok ()
  | Literal_void, _ -> fail @@ different_literals_type  a b
  | Literal_unit, Literal_unit -> ok ()
  | Literal_unit, _ -> fail @@ different_literals_type  a b
  | Literal_address a, Literal_address b when a = b -> ok ()
  | Literal_address _, Literal_address _ -> fail @@ different_literals_value a b
  | Literal_address _, _ -> fail @@ different_literals_type  a b
  | Literal_operation _, Literal_operation _ -> fail @@ uncomparable_literals a b
  | Literal_operation _, _ -> fail @@ different_literals_type  a b
  | Literal_signature a, Literal_signature b when a = b -> ok ()
  | Literal_signature _, Literal_signature _ -> fail @@ different_literals_value a b
  | Literal_signature _, _ -> fail @@ different_literals_type  a b
  | Literal_key a, Literal_key b when a = b -> ok ()
  | Literal_key _, Literal_key _ -> fail @@ different_literals_value a b
  | Literal_key _, _ -> fail @@ different_literals_type  a b
  | Literal_key_hash a, Literal_key_hash b when a = b -> ok ()
  | Literal_key_hash _, Literal_key_hash _ -> fail @@ different_literals_value a b
  | Literal_key_hash _, _ -> fail @@ different_literals_type  a b
  | Literal_chain_id a, Literal_chain_id b when a = b -> ok ()
  | Literal_chain_id _, Literal_chain_id _ -> fail @@ different_literals_value a b
  | Literal_chain_id _, _ -> fail @@ different_literals_type a b

let rec assert_value_eq (a, b: (expression * expression )) : (unit,_) result =
  (* Format.printf "in assert_value_eq %a %a\n%!" PP.expression a PP.expression b; *)
  (* let error_content () =
    Format.asprintf "\n@[<v>- %a@;- %a]" PP.expression a PP.expression b
  in *)
  trace (not_equal_tracer a b) @@
  match (a.expression_content , b.expression_content) with
  | E_literal a , E_literal b ->
    assert_literal_eq (a, b)
  | E_constant (ca) , E_constant (cb) when ca.cons_name = cb.cons_name -> (
      let%bind lst =
        generic_try (simple_error "constants with different number of elements")
          (fun () -> List.combine ca.arguments cb.arguments) in
      let%bind _all = bind_list @@ List.map assert_value_eq lst in
      ok ()
    )
  | E_constructor (ca), E_constructor (cb) when ca.constructor = cb.constructor -> (
      let%bind _eq = assert_value_eq (ca.element, cb.element) in
      ok ()
    )
  | E_record sma, E_record smb -> (
      let aux _ a b =
        match a, b with
        | Some a, Some b -> Some (assert_value_eq (a, b))
        | _ -> Some (simple_fail "different record keys")
      in
      let%bind _all = bind_lmap @@ LMap.merge aux sma smb in
      ok ()
    )
  | E_record_update ura, E_record_update urb ->
    let _ = 
      generic_try (simple_error "Updating different record") @@ 
      fun () -> assert_value_eq (ura.record, urb.record) in
    let aux (Label a,Label b) =
      assert (String.equal a b)
    in
    let () = aux (ura.path, urb.path) in
    let%bind () = assert_value_eq (ura.update,urb.update) in
    ok ()
  | (E_map lsta, E_map lstb | E_big_map lsta, E_big_map lstb) -> (
      let%bind lst = generic_try (simple_error "maps of different lengths")
          (fun () ->
             let lsta' = List.sort compare lsta in
             let lstb' = List.sort compare lstb in
             List.combine lsta' lstb') in
      let aux = fun ((ka, va), (kb, vb)) ->
        let%bind _ = assert_value_eq (ka, kb) in
        let%bind _ = assert_value_eq (va, vb) in
        ok () in
      let%bind _all = bind_map_list aux lst in
      ok ()
    )
  | E_list lsta, E_list lstb -> (
      let%bind lst =
        generic_try (simple_error "list of different lengths")
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_map_list assert_value_eq lst in
      ok ()
    )
  | E_set lsta, E_set lstb -> (
      let lsta' = List.sort (compare) lsta in
      let lstb' = List.sort (compare) lstb in
      let%bind lst =
        generic_try (simple_error "set of different lengths")
          (fun () -> List.combine lsta' lstb') in
      let%bind _all = bind_map_list assert_value_eq lst in
      ok ()
    )
  | (E_ascription a ,  _b') -> assert_value_eq (a.anno_expr , b)
  | (_a' , E_ascription b) -> assert_value_eq (a , b.anno_expr)

  | (E_variable _, _) | (E_lambda _, _)
  | (E_application _, _) | (E_let_in _, _)
  | (E_recursive _,_) | (E_record_accessor _, _)
  | (E_look_up _, _) | (E_matching _, _)
   -> fail @@ not_a_value a

  | E_literal _ , _
  | E_constant _ , E_constant _
  | E_constant _ , _
  | E_constructor _, E_constructor _
  | E_record _, _
  | E_constructor _, _
  | E_record_update _, _
  | E_list _, _
  | E_set _, _
  | (E_map _ | E_big_map _), _ ->
      fail @@ uncomparable a b

let is_value_eq (a , b) = to_bool @@ assert_value_eq (a , b)
