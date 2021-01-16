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


let merge_in_state ~demoted_repr ~new_repr state =
  let updater = {
    map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
    set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
  } in
  merge_aliases updater state

let merge_in_repr ~demoted_repr ~new_repr repr =
  fun tv -> match repr tv with
      tv when Var.equal tv demoted_repr -> new_repr
    | other -> other

let merge ~demoted_repr ~new_repr repr state =
  if (not (Var.equal (repr demoted_repr) demoted_repr)) or
     (not (Var.equal (repr new_repr) demoted_repr))
  then failwith "Internal error: bad test: the demoted_repr and \
                 new_repr should already be representants when merge \
                 is called."
  else ();
  ((merge_in_repr ~demoted_repr ~new_repr repr),
   (merge_in_state ~demoted_repr ~new_repr state))

(* can't be defined easily in PolySet.ml because it doesn't have access to List.compare ~cmp  *)
let polyset_compare a b =
  let ab = List.compare ~compare:(PolySet.get_compare a) (PolySet.elements a) (PolySet.elements b) in
  let ba = List.compare ~compare:(PolySet.get_compare b) (PolySet.elements a) (PolySet.elements b) in
  if ab != ba
  then failwith "Internal error: bad test: sets being compared have different comparison functions!"
  else ab

module Grouped_by_variable_tests = struct
  include Test_vars
  module Plugin_under_test = GroupedByVariable
  include Plugin_under_test
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv

  let cmp x y =
    List.compare ~compare:(Pair.compare Var.compare polyset_compare)
      (List.filter (fun (_,s) -> not (PolySet.is_empty s)) x)
      (List.filter (fun (_,s) -> not (PolySet.is_empty s)) y)
  let same_state (expected : _ t_for_tests) (actual : _ t_for_tests) =
    let%bind () = tst_assert "lists of ctors must be equal" (cmp expected.constructor actual.constructor = 0) in
    let%bind () = tst_assert "lists of rows must be equal"  (cmp expected.row actual.row = 0) in
    let%bind () = tst_assert "lists of polys must be equal" (cmp expected.poly actual.poly = 0) in
    ok ()
end

open Grouped_by_variable_tests

type nonrec t_for_tests = type_variable GroupedByVariable.t_for_tests

let empty_ctors = PolySet.create ~cmp:Ast_typed.Compare.c_constructor_simpl
let empty_rows  = PolySet.create ~cmp:Ast_typed.Compare.c_row_simpl
let empty_polys = PolySet.create ~cmp:Ast_typed.Compare.c_poly_simpl
let only_ctors  = List.map (function Ast_typed.Types.SC_Constructor c -> c | _ -> failwith "bad expeted in test: should be a constructor")
let only_rows   = List.map (function Ast_typed.Types.SC_Row         c -> c | _ -> failwith "bad expeted in test: should be a constructor")
let only_polys  = List.map (function Ast_typed.Types.SC_Poly        c -> c | _ -> failwith "bad expeted in test: should be a constructor")
let to_ctor_sets = List.map (fun (v,cs) -> (v, (PolySet.add_list (only_ctors cs) empty_ctors).set))
let to_row_sets  = List.map (fun (v,cs) -> (v, (PolySet.add_list (only_rows  cs) empty_rows).set))
let to_poly_sets = List.map (fun (v,cs) -> (v, (PolySet.add_list (only_polys cs) empty_polys).set))

(* Uncomment these if necessary *)
(* let assert_ctor_equal ~(expected:(type_variable * c_constructor_simpl list) list) ~(actual:t_for_tests) =
 *   same_state { constructor = to_ctor_sets expected ; row = [] ; poly = [] } actual
 * 
 * let assert_row_equal ~(expected:(type_variable * c_row_simpl list) list) ~(actual:t_for_tests) =
 *   same_state { constructor = [] ; row = to_row_sets expected ; poly = [] } actual
 * 
 * let assert_row_equal ~(expected:(type_variable * c_poly_simpl list) list) ~(actual:t_for_tests) =
 *   same_state { constructor = [] ; row = [] ; poly = to_poly_sets expected } actual *)

let assert_states_equal
    ~(expected_ctors:(type_variable * type_constraint_simpl list) list)
    ~(expected_rows:(type_variable * type_constraint_simpl list) list)
    ~(expected_polys:(type_variable * type_constraint_simpl list) list)
    ~(actual:type_variable t) =
  same_state
    {
      constructor = to_ctor_sets expected_ctors ;
      row         = to_row_sets  expected_rows  ;
      poly        = to_poly_sets expected_polys ;
    }
    (GroupedByVariable.bindings actual)

let first_test () =
  (* create constraints and add them to the state *)
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in
  let state = add_constraint repr state sc_a in
  let state = add_constraint repr state sc_b in
  let state = add_constraint repr state sc_c in
  (* 
    check that :
    - a is associated with sc_a and sc_b
    - c is associated wit sc_c
    - b has no associated constraint (because repr(b) = a)
  *)
  assert_states_equal
    ~expected_ctors:[(tva, [sc_a ; sc_b]) ; (tvc, [sc_c])]
    ~expected_rows:[]
    ~expected_polys:[]
    ~actual:state

let second_test () =
  (* create constraints and add them to the state *)
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in
  let state = add_constraint repr state sc_a in
  let state = add_constraint repr state sc_b in
  let state = add_constraint repr state sc_c in
  (* 
    check that :
    - a is associated with sc_a and sc_b
    - c is associated wit sc_c
    - b has no associated constraint (because repr(b) = a)
  *)
  let%bind () = assert_states_equal
      ~expected_ctors:[(tva, [sc_a ; sc_b]) ; (tvc, [sc_c])]
      ~expected_rows:[]
      ~expected_polys:[]
      ~actual:state in

  (* remove sc_a from state *)
  let%bind state = trace Main_errors.typer_tracer @@ remove_constraint repr state sc_a in
  (* same check as above except sc_a should be deleted from tva's constraints *)
  let%bind () = assert_states_equal
      ~expected_ctors:[(tva, [sc_b]) ; (tvc, [sc_c])]
      ~expected_rows:[]
      ~expected_polys:[]
      ~actual:state in

  (* merge variable c into a *)
  let repr, state = merge ~demoted_repr:tva ~new_repr:tvc repr state in
  (* same check as above except sc_c should now be in a's constraints *)
  let%bind () = assert_states_equal
      ~expected_ctors:[(tva, [sc_b; sc_c])]
      ~expected_rows:[]
      ~expected_polys:[]
      ~actual:state in

  (* create constraint and add it to the state *)
  let sc_d : type_constraint_simpl = constructor 4 None tvd C_unit [] in
  let state = add_constraint repr state sc_d in
  (* same check as above except sc_d should be added to d's constraints (was empty / absent before) *)
  let%bind () = assert_states_equal
      ~expected_ctors:[(tva, [sc_b; sc_c]) ; (tvd, [sc_d])]
      ~expected_rows:[]
      ~expected_polys:[]
      ~actual:state in

  (* create constraint and add it to the state *)
  let sc_a2 : type_constraint_simpl = constructor 5 None tva C_unit [] in
  let state = add_constraint repr state sc_a2 in
  (* same check as above except sc_d should be added to a's constraints *)
  let%bind () = assert_states_equal
      ~expected_ctors:[(tva, [sc_a2; sc_b; sc_c]) ; (tvd, [sc_d])]
      ~expected_rows:[]
      ~expected_polys:[]
      ~actual:state in

  (* create constraint and add it to the state *)
  let sc_b2 : type_constraint_simpl = constructor 6 None tvb C_unit [] in
  let state = add_constraint repr state sc_b2 in
  (* same check as above except sc_d should be added to a's constraints *)
  let%bind () = assert_states_equal
      ~expected_ctors:[(tva, [sc_a2; sc_b; sc_b2; sc_c]) ; (tvd, [sc_d])]
      ~expected_rows:[]
      ~expected_polys:[]
      ~actual:state in

  ok ()














let _ =
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    "

  (* =============================================================================================
     TODO: THIS TEST IS DISABLED BECAUSE REMOVAL IS NOT IMPLEMENTED FOR CONSTRUCTOR CONSTRAINTS IN
     grouped_by_variable
     ============================================================================================= *)

let constraints_nb (l:t_for_tests) (expected:int) =
  List.(
    length l.constructor = expected &&
    length l.poly = 0 &&
    length l.row = 0
  )
let sc_a : type_constraint_simpl = constructor tva C_unit []
let sc_b : type_constraint_simpl = constructor tvb C_unit []
let sc_c : type_constraint_simpl = constructor tvc C_unit []
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
let sc_a : type_constraint_simpl = row tva
let sc_b : type_constraint_simpl = row tvb
let sc_c : type_constraint_simpl = row tvc

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
  let sc_a : type_constraint_simpl = constructor tva C_unit [] in
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
  let sc_b : type_constraint_simpl = row tvb in
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
  let sc_c2 = constructor tvc C_unit [] in
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

  let sc_b2 = row tvb in
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
