open Trace
open Ast_typed.Types
open Database_plugins.All_plugins
open Db_index_tests_common

open Test_vars
open GroupedByVariable
let repr : type_variable -> type_variable = fun tv ->
  match tv with
  | tv when Var.equal tv tva -> tva
  | tv when Var.equal tv tvb -> tva
  | _ -> tv

let assert_ctor_equal ~(expected:type_constraint_simpl list) ~(actual:constraints) =
  (*order of lists do not matter*)
  let aux : type_constraint_simpl -> (unit,_) result =
    fun expected ->
      match expected with
      | SC_Constructor expected' -> (
        let opt = List.find_opt (fun a -> Ast_typed.Compare.c_constructor_simpl a expected' = 0) actual.constructor in
        match opt with
        | Some _ -> ok ()
        | None -> fail (test_err "ctor must be equal")
      ) 
      | _ -> fail (test_err "expecting constructors only")
  in
  bind_iter_list aux expected

let assert_row_equal ~(expected:type_constraint_simpl list) ~(actual:constraints) =
  (*order of lists do not matter*)
  let aux : type_constraint_simpl -> (unit,_) result =
    fun expected ->
      match expected with
      | SC_Row expected' -> (
        let opt = List.find_opt (fun a -> Ast_typed.Compare.c_row_simpl a expected' = 0) actual.row in
        match opt with
        | Some _ -> ok ()
        | None -> fail (test_err "rows must be equal")
      ) 
      | _ -> fail (test_err "expecting rows only")
  in
  bind_iter_list aux expected


let assert_poly_equal ~(expected:type_constraint_simpl list) ~(actual:constraints) =
  (*order of lists do not matter*)
  let aux : type_constraint_simpl -> (unit,_) result =
    fun expected ->
      match expected with
      | SC_Poly expected' -> (
        let opt = List.find_opt (fun a -> Ast_typed.Compare.c_poly_simpl a expected' = 0) actual.poly in
        match opt with
        | Some _ -> ok ()
        | None -> fail (test_err "polys must be equal")
      ) 
      | _ -> fail (test_err "expecting polys only")
  in
  bind_iter_list aux expected

let assert_const_equal ~(expected:type_constraint_simpl list) ~(actual:constraints) =
  (*order of lists do not matter*)
  let aux : type_constraint_simpl -> (unit,_) result =
    fun expected ->
      match expected with
      | SC_Constructor expected' -> (
        let opt = List.find_opt (fun a -> Ast_typed.Compare.c_constructor_simpl a expected' = 0) actual.constructor in
        match opt with
        | Some _ -> ok ()
        | None -> fail (test_err "ctor must be equal")
      ) 
      | SC_Row expected' -> (
        let opt = List.find_opt (fun a -> Ast_typed.Compare.c_row_simpl a expected' = 0) actual.row in
        match opt with
        | Some _ -> ok ()
        | None -> fail (test_err "rows must be equal")
      ) 
      | SC_Poly expected' -> (
        let opt = List.find_opt (fun a -> Ast_typed.Compare.c_poly_simpl a expected' = 0) actual.poly in
        match opt with
        | Some _ -> ok ()
        | None -> fail (test_err "polys must be equal")
      ) 
      | _ -> fail (test_err "expecting ctors, rows or polys")
  in
  bind_iter_list aux expected

module Grouped_by_variable_tests = struct
  include Test_vars
  module Plugin_under_test = GroupedByVariable
  include Plugin_under_test
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv
  let same_state sa sb =
    let sa = bindings sa in
    let sb = bindings sb in
    let%bind () = tst_assert "Length sa = Length sb" (List.length sa = List.length sb) in
    bind_list_iter
      (fun ((tva,constraintsa) , (tvb,constraintsb)) ->
         let%bind () = tst_assert "" (Ast_typed.Compare.type_variable tva tvb = 0) in
         let { constructor = ca ; poly = pa; row = ra } = constraintsa in
         let { constructor = cb ; poly = pb; row = rb } = constraintsb in
         let%bind () = tst_assert "" (List.compare ~compare:Ast_typed.Compare.c_constructor_simpl ca cb = 0) in
         let%bind () = tst_assert "" (List.compare ~compare:Ast_typed.Compare.c_poly_simpl pa pb = 0) in
         let%bind () = tst_assert "" (List.compare ~compare:Ast_typed.Compare.c_row_simpl ra rb = 0) in
         ok ()
      )
      (List.combine sa sb)
end

let previous_test () =
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  let constraints_nb (l:constraints) (expected:int) =
    List.(
      length l.constructor = expected &&
      length l.poly = 0 &&
      length l.row = 0
    ) in

  let state = create_state ~cmp:Ast_typed.Compare.type_variable in
  let clist = [ sc_a ; sc_b ; sc_c ] in
  let state = List.fold_left (fun acc el -> add_constraint repr acc el) state clist in
  (* 
    check that :
    - a is associated with sc_a and sc_b
    - c is associated wit sc_c
    - b has no associated constraint (repr(b) = a)
  *)
  let gbv = bindings state in
  let%bind () = tst_assert "state' = { a -> ... ; c -> ... }" (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert "two constraints related to tva" (constraints_nb cs 2) in
          let%bind () = assert_ctor_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert "one constraint related to tvc" (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_c] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err "b should not be in the state")
        | _ -> fail @@ test_err "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  (* =============================================================================================
     TODO: THIS TEST IS DISABLED BECAUSE REMOVAL IS NOT IMPLEMENTED FOR CONSTRUCTOR CONSTRAINTS IN
     grouped_by_variable
     ============================================================================================= *)
  let%bind () = if false then
      (* remove sc_a from state *)
      let%bind state = trace Main_errors.typer_tracer @@ remove_constraint repr state sc_a in
      (* same check as above except sc_a should be deleted from tva's constraints *)
      let gbv = bindings state in
      let%bind () = tst_assert "state' = { a -> ... ; c -> ... }" (List.length gbv = 2) in
      let%bind () =
        let aux : (type_variable * constraints) -> (unit,_) result =
          fun (tv, cs) ->
            match tv with
            | a when Var.equal a tva ->
              let%bind () = tst_assert "two constraints related to tva" (constraints_nb cs 1) in
              let%bind () = assert_ctor_equal ~expected:[sc_b] ~actual:cs in
              ok ()
            | c when Var.equal c tvc ->
              let%bind () = tst_assert "one constraint related to tvc" (constraints_nb cs 1) in
              let%bind () = assert_ctor_equal ~expected:[sc_c] ~actual:cs in
              ok ()
            | b when Var.equal b tvb -> fail (test_err "b should not be in the state")
            | _ -> fail @@ test_err "new variable discovered (impossible)"
        in
        bind_iter_list aux gbv
      in
      ok ()
    else
      ok ()
  in
  (*
  let d = merge_aliases (*??*) in
  let e = get_constraints_by_lhs tv a in
  ignore (a,b,c,d,e) ; *)
  ok ()

let constraints_nb (l:constraints) (expected:int) =
  List.(
    length l.constructor = expected &&
    length l.poly = 0 &&
    length l.row = 0
  )
let sc_a : type_constraint_simpl = constructor 4 None tva C_unit []
let sc_b : type_constraint_simpl = constructor 5 None tvb C_unit []
let sc_c : type_constraint_simpl = constructor 6 None tvc C_unit []
(* Test independant add + remove + merge pour chaque type de contraintes:
   test add ctor constraint + add other ctor constraint + merge ctor constraint + add third ctor constraint
   *)
let ctor_add_and_merge () =
  let msg = "ctor_add_and_merge:" in

  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  (* Add contraint sc_a *)
  let state' = add_constraint (fun a -> a) state sc_a in

  (* Test one; state is { a -> [sc_a]} *)
  let test = "Test 1:" in
  let gbv = bindings state' in
  let%bind () = tst_assert (msg^test^"state' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg ^ test ^ "c should not be in the state")
        | _ -> fail @@ test_err "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  
  (* Add constraint sc_b *)
  let state'' = add_constraint (fun a -> a) state' sc_b in
  (* Test two; state is { a -> [sc_a]; b -> [sc_b]} *)
  let test = "Test 2:" in
  let gbv = bindings state'' in
  let%bind () = tst_assert (msg^test^"state'' = { a -> ... ; b -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb ->
          let%bind () = tst_assert (msg^test^"one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg^test^"new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Merge constaint sc_a sc_b *)
  let merge_tvb_in_tva  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvb in
    let new_repr = tva in
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state''' = merge_aliases merge_tvb_in_tva state'' in
  (* Test three; state is { a -> [sc_a;sc_b]} *)
  let test = "Test 3:" in
  let gbv = bindings state''' in
  let%bind () = tst_assert (msg^test^"state''' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_ctor_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg^test^"b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Add constraint sc_c *)
  let state'''' = add_constraint repr state''' sc_c in
  (* Test four; state is { a -> [sc_a;sc_b]; c -> [sc_c]} *)
  let test = "Test 4:" in
  let gbv = bindings state'''' in
  let%bind () = tst_assert (msg ^ test ^ "state'''' = { a -> ... ; c -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_ctor_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg ^ test ^ "one constraints related to tvc") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_c] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  ok ()

(*
   test add ctor constraint + add other ctor constraint + remove ctor constraint + add third ctor constraint
*)
let ctor_add_and_remove () =
  let msg = "ctor_add_and_remove:" in

  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  (* Add contraint sc_a *)
  let state' = add_constraint (fun a -> a) state sc_a in

  (* Test one; state is { a -> [sc_a]} *)
  let test = "Test 1:" in
  let gbv = bindings state' in
  let%bind () = tst_assert (msg^test^"state' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg ^ test ^ "c should not be in the state")
        | _ -> fail @@ test_err "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  
  (* Add constraint sc_b *)
  let state'' = add_constraint (fun a -> a) state' sc_b in
  (* Test two; state is { a -> [sc_a]; b -> [sc_b]} *)
  let test = "Test 2:" in
  let gbv = bindings state'' in
  let%bind () = tst_assert (msg^test^"state'' = { a -> ... ; b -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb ->
          let%bind () = tst_assert (msg^test^"one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg^test^"new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Remove constaint sc_b *)
  let%bind state''' = trace Main_errors.typer_tracer @@ remove_constraint (fun a -> a) state'' sc_b in
  (* Test three; state is { a -> [sc_a]} *)
  let test = "Test 3:" in
  let gbv = bindings state''' in
  let%bind () = tst_assert (msg^test^"state''' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg^test^"b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Add constraint sc_c *)
  let state'''' = add_constraint (fun a -> a) state''' sc_c in
  (* Test four; state is { a -> [sc_a]; c -> [sc_b]} *)
  let test = "Test 4:" in
  let gbv = bindings state'''' in
  let%bind () = tst_assert (msg ^ test ^ "state'''' = { a -> ... ; c -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_ctor_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg ^ test ^ "one constraints related to tvc") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_c] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  ok ()
(*
   test add row constraint + add other row constraint + merge row constraint + add third row constraint
*)
let constraints_nb (l:constraints) (expected:int) =
  List.(
    length l.constructor = 0 &&
    length l.poly = 0 &&
    length l.row = expected
  )
let sc_a : type_constraint_simpl = row 7 tva
let sc_b : type_constraint_simpl = row 8 tvb
let sc_c : type_constraint_simpl = row 9 tvc

let row_add_and_merge () =
  let msg = "row_add_and_merge:" in

  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  (* Add contraint sc_a *)
  let state' = add_constraint (fun a -> a) state sc_a in

  (* Test 1 : state is { a -> [sc_a]}*)
  let test = "Test 1:" in
  let gbv = bindings state' in
  let%bind () = tst_assert (msg^test^"state' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg ^ test ^ "c should not be in the state")
        | _ -> fail @@ test_err "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  
  (* Add constraint sc_b *)
  let state'' = add_constraint (fun a -> a) state' sc_b in
  (* Test 2 : state is { a -> [sc_a]; b -> [sc_b]}*)
  let test = "Test 2:" in
  let gbv = bindings state'' in
  let%bind () = tst_assert (msg^test^"state'' = { a -> ... ; b -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb ->
          let%bind () = tst_assert (msg^test^"one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg^test^"new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Merge constaint sc_a sc_b *)
  let merge_tvb_in_tva  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvb in
    let new_repr = tva in
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state''' = merge_aliases merge_tvb_in_tva state'' in
  (* Test 3 : state is { a -> [sc_a;sc_b]}*)
  let test = "Test 3:" in
  let gbv = bindings state''' in
  let%bind () = tst_assert (msg^test^"state''' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_row_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg^test^"b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Add constraint sc_c *)
  let state'''' = add_constraint repr state''' sc_c in
  (* Test 4 : state is { a -> [sc_a;sc_b]; c -> [sc_c]}*)
  let test = "Test 4:" in
  let gbv = bindings state'''' in
  let%bind () = tst_assert (msg ^ test ^ "state'''' = { a -> ... ; c -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_row_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg ^ test ^ "one constraints related to tvc") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_c] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  ok ()

(*
   test add row constraint + add other row constraint + remove row constraint + add third row constraint
*)
let row_add_and_remove () =
  let msg = "row_add_and_remove:" in

  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  (* Add contraint sc_a *)
  let state' = add_constraint (fun a -> a) state sc_a in

  (* Test 1 : state is { a -> [sc_a]}*)
  let test = "Test 1:" in
  let gbv = bindings state' in
  let%bind () = tst_assert (msg^test^"state' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg ^ test ^ "c should not be in the state")
        | _ -> fail @@ test_err "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  
  (* Add constraint sc_b *)
  let state'' = add_constraint (fun a -> a) state' sc_b in
  (* Test 2 : state is { a -> [sc_a]; b -> [sc_b]} *)
  let test = "Test 2:" in
  let gbv = bindings state'' in
  let%bind () = tst_assert (msg^test^"state'' = { a -> ... ; b -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb ->
          let%bind () = tst_assert (msg^test^"one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg^test^"new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Remove constaint sc_a sc_b *)
  let%bind state''' = trace Main_errors.typer_tracer @@ remove_constraint (fun a -> a) state'' sc_b in
  (* Test 3 : state is { a -> [sc_a]} *)
  let test = "Test 3:" in
  let gbv = bindings state''' in
  let%bind () = tst_assert (msg^test^"state''' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg^test^"b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Add constraint sc_c *)
  let state'''' = add_constraint (fun a -> a) state''' sc_c in
  (* Test 4 : state is { a -> [sc_a]; c -> [sc_c]} *)
  let test = "Test 4:" in
  let gbv = bindings state'''' in
  let%bind () = tst_assert (msg ^ test ^ "state'''' = { a -> ... ; c -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_row_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg ^ test ^ "one constraints related to tvc") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_c] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  ok ()
(*
   test add typeclass constraint + add other typeclass constraint + merge typeclass constraint + add third typeclass constraint
   test add typeclass constraint + add other typeclass constraint + remove typeclass constraint + add third typeclass constraint
  typeclass not added yet
*)

(*
   test add poly constraint + add other poly constraint + merge poly constraint + add third poly constraint
*)
let constraints_nb (l:constraints) (expected:int) =
  List.(
    length l.constructor = 0 &&
    length l.poly = expected &&
    length l.row = 0
  )
let p_forall : p_forall = {
  binder = Var.of_name "binder";
  constraints = [];
  body = Location.wrap @@ P_variable (Var.of_name "binder");
}
let sc_a : type_constraint_simpl = poly tva p_forall
let sc_b : type_constraint_simpl = poly tvb p_forall
let sc_c : type_constraint_simpl = poly tvc p_forall

let poly_add_and_merge () =
  let msg = "poly_add_and_merge:" in

  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  (* Add contraint sc_a *)
  let state' = add_constraint (fun a -> a) state sc_a in

  (* Test 1 : state is { a -> [sc_a]} *)
  let test = "Test 1:" in
  let gbv = bindings state' in
  let%bind () = tst_assert (msg^test^"state' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg ^ test ^ "c should not be in the state")
        | _ -> fail @@ test_err "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  
  (* Add constraint sc_b *)
  let state'' = add_constraint (fun a -> a) state' sc_b in
  (* Test 2 : state is { a -> [sc_a]; b -> [sc_b]} *)
  let test = "Test 2:" in
  let gbv = bindings state'' in
  let%bind () = tst_assert (msg^test^"state'' = { a -> ... ; b -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb ->
          let%bind () = tst_assert (msg^test^"one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg^test^"new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Merge constaint sc_a sc_b *)
  let merge_tvb_in_tva  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvb in
    let new_repr = tva in
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state''' = merge_aliases merge_tvb_in_tva state'' in
  (* Test 3: state is { a -> [sc_a;sc_b]} *)
  let test = "Test 3:" in
  let gbv = bindings state''' in
  let%bind () = tst_assert (msg^test^"state''' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_poly_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg^test^"b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Add constraint sc_c *)
  let state'''' = add_constraint repr state''' sc_c in
  (* Test 4: state is { a -> [sc_a;sc_b]; c -> [sc_c]} *)
  let test = "Test 4:" in
  let gbv = bindings state'''' in
  let%bind () = tst_assert (msg ^ test ^ "state'''' = { a -> ... ; c -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_poly_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg ^ test ^ "one constraints related to tvc") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_c] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  ok ()

(*
   test add poly constraint + add other poly constraint + remove poly constraint + add third poly constraint
*)
let poly_add_and_remove () =
  let msg = "poly_add_and_remove:" in

  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  (* Add contraint sc_a *)
  let state' = add_constraint (fun a -> a) state sc_a in

  (* Test 1: state is { a -> [sc_a]} *)
  let test = "Test 1:" in
  let gbv = bindings state' in
  let%bind () = tst_assert (msg^test^"state' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg ^ test ^ "c should not be in the state")
        | _ -> fail @@ test_err "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  
  (* Add constraint sc_b *)
  let state'' = add_constraint (fun a -> a) state' sc_b in
  (* Test 2: state is { a -> [sc_a]; b -> [sc_b]} *)
  let test = "Test 2:" in
  let gbv = bindings state'' in
  let%bind () = tst_assert (msg^test^"state'' = { a -> ... ; b -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb ->
          let%bind () = tst_assert (msg^test^"one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg^test^"new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Remove constaint sc_b *)
  let%bind state''' = trace Main_errors.typer_tracer @@ remove_constraint (fun a -> a) state'' sc_b in
  (* Test 3: state is { a -> [sc_a]} *)
  let test = "Test 3:" in
  let gbv = bindings state''' in
  let%bind () = tst_assert (msg^test^"state''' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg^test^"b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Add constraint sc_c *)
  let state'''' = add_constraint (fun a -> a) state''' sc_c in
  (* Test 4: state is { a -> [sc_a]; c -> [sc_c]} *)
  let test = "Test 4:" in
  let gbv = bindings state'''' in
  let%bind () = tst_assert (msg ^ test ^ "state'''' = { a -> ... ; c -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_poly_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg ^ test ^ "one constraints related to tvc") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_c] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  ok ()

(* Test mixtes + remove + merge *)

let constraints_nb (l:constraints) (expected:int) =
  List.(
    length l.constructor +
    length l.poly +
    length l.row = expected
  )
let mixte () =
  let msg = "mixte:" in

  let state = create_state ~cmp:Ast_typed.Compare.type_variable in


  (* add ctor *)
  let sc_a : type_constraint_simpl = constructor 10 None tva C_unit [] in
  let state' = add_constraint (fun a -> a) state sc_a in

  (* Test 1: state is { a -> [sc_a]} *)
  let test = "Test 1:" in
  let gbv = bindings state' in
  let%bind () = tst_assert (msg^test^"state' = { a -> ... }") (List.length gbv = 1) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg ^ test ^ "b should not be in the state")
        | c when Var.equal c tvc -> fail (test_err @@ msg ^ test ^ "c should not be in the state")
        | _ -> fail @@ test_err "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  
  (* Add row *)
  let sc_b : type_constraint_simpl = row 11 tvb in
  let state'' = add_constraint (fun a -> a) state' sc_b in
  (* Test 2: state is { a -> [sc_a]; b -> [sc_b]} *)
  let test = "Test 2:" in
  let gbv = bindings state'' in
  let%bind () = tst_assert (msg^test^"state'' = { a -> ... ; b -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb ->
          let%bind () = tst_assert (msg^test^"one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc -> fail (test_err @@ msg^test^"c should not be in the state")
        | _ -> fail @@ test_err @@ msg^test^"new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Add poly*)
  let sc_c : type_constraint_simpl = poly tvc p_forall in
  let state''' = add_constraint (fun a -> a) state'' sc_c in
  (* Test 3: state is { a -> [sc_a]; b -> [sc_b]; c -> [sc_c]} *)
  let test = "Test 3:" in
  let gbv = bindings state''' in
  let%bind () = tst_assert (msg^test^"state''' = { a -> ... ; b -> ... ; c -> ... }") (List.length gbv = 3) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg^test^"one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb ->
          let%bind () = tst_assert (msg^test^"one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg^test^"one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_poly_equal ~expected:[sc_c] ~actual:cs in
          ok ()
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Add constraint sc_c2 *)
  let sc_c2 = constructor 12 None tvc C_unit [] in
  let state'''' = add_constraint (fun a -> a) state''' sc_c2 in
  (* Test 4: state is { a -> [sc_a]; b -> [sc_b]; c -> [sc_c;sc_c2]} *)
  let test = "Test 4:" in
  let gbv = bindings state'''' in
  let%bind () = tst_assert (msg ^ test ^ "state'''' = { a -> ... ; b -> ... ; c -> ... }") (List.length gbv = 3) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^ "one constraints related to tva") (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_a] ~actual:cs in
          ok ()
        | b when Var.equal b tvb ->
          let%bind () = tst_assert (msg ^ test ^ "one constraints related to tvb") (constraints_nb cs 1) in
          let%bind () = assert_row_equal ~expected:[sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tvc") (constraints_nb cs 2) in
          let%bind () = assert_const_equal ~expected:[sc_c;sc_c2] ~actual:cs in
          ok ()
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  (* Merge tvb in tva *)
  let merge_tvb_in_tva  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvb in
    let new_repr = tva in
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state''''' = merge_aliases merge_tvb_in_tva state'''' in
  (* Test 5: state is { a -> [sc_a;sc_b]; c -> [sc_c;sc_c2]} *)
  let test = "Test 5:" in
  let gbv = bindings state''''' in
  let%bind () = tst_assert (msg^test^"state''' = { a -> ... ; c -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tva") (constraints_nb cs 2) in
          let%bind () = assert_const_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg^test^"b should not be in the state")
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tvc") (constraints_nb cs 2) in
          let%bind () = assert_const_equal ~expected:[sc_c;sc_c2] ~actual:cs in
          ok ()
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in

  let sc_b2 = row 13 tvb in
  let state'''''' = add_constraint repr state''''' sc_b2 in
  (* Test 6: state is { a -> [sc_a;sc_b;sc_b2]; c -> [sc_c;sc_c2]} *)
  let test = "Test 6:" in
  let gbv = bindings state'''''' in
  let%bind () = tst_assert (msg^test^"state''' = { a -> ... ; c -> ... }") (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert ( msg ^ test ^ "three constraints related to tva") (constraints_nb cs 3) in
          let%bind () = assert_const_equal ~expected:[sc_a;sc_b;sc_b2] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err @@ msg^test^"b should not be in the state")
        | c when Var.equal c tvc ->
          let%bind () = tst_assert (msg ^ test ^ "two constraints related to tvc") (constraints_nb cs 2) in
          let%bind () = assert_const_equal ~expected:[sc_c;sc_c2] ~actual:cs in
          ok ()
        | _ -> fail @@ test_err @@ msg ^ test ^ "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  ok ()

let grouped_by_variable () =
  let%bind () = previous_test () in
  let%bind () = ctor_add_and_merge () in
  (* let%bind () = ctor_add_and_remove () in *)
  let%bind () = row_add_and_merge () in
  (* let%bind () = row_add_and_remove () in *)
  let%bind () = poly_add_and_merge () in
  (* let%bind () = poly_add_and_remove () in *)
  let%bind () = mixte () in
  ok ()
