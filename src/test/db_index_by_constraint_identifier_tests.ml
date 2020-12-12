open Trace

(* module Core = Typesystem.Core *)
open Ast_typed.Types
(* open Ast_typed.Reasons *)
open Ast_typed.Combinators
open Database_plugins.All_plugins
(* module Assignments                   = Assignments
module GroupedByVariable             = GroupedByVariable
module CycleDetectionTopologicalSort = CycleDetectionTopologicalSort
module ByConstraintIdentifier        = ByConstraintIdentifier
module RefinedTypeclasses            = RefinedTypeclasses
module TypeclassesConstraining       = TypeclassesConstraining *)

open Db_index_tests_common

module By_constraint_identifier_tests = struct
  include Test_vars
  include ByConstraintIdentifier
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv
  let same_state sa sb =
    (* This index is not implemented yet, its state is just a unit value *)
    let sa = PolyMap.bindings @@ get_state_for_tests sa in
    let%bind () = tst_assert "Length sa = Length sb" (List.length sa = List.length sb) in
    bind_list_iter
      (fun ((cida,tca) , (cidb,tcb)) ->
        let%bind () = tst_assert "constructor id =" (Ast_typed.Compare.constraint_identifier cida cidb = 0) in
        let%bind () = tst_assert "c_typeclass_simpl =" (Ast_typed.Compare.c_typeclass_simpl tca tcb = 0) in
        ok ()
      )
      (List.combine sa sb)
end

let tval ?(loc = Location.generated) tag args = 
  {
    Location.
    wrap_content = P_constant { p_ctor_tag = tag ; p_ctor_args = args; };
    location = loc
  }

let tval_int = tval C_int []
let tval_unit = tval C_unit []
let tval_map_int_unit = tval C_map [tval C_int []; tval C_unit []]

let by_constraint_identifier () =
  let open By_constraint_identifier_tests in
  (* create empty state *)
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in
  (* assert state = {} *)
  let%bind () = same_state state [] in

  (* add `tva = unit()' to the state *)
  let ctor_a = make_c_constructor_simpl tva C_unit [] in
  let state' = add_constraint repr state (SC_Constructor ctor_a) in                                           
  (* assert state' = {} because only typeclass constraints have an ID for now. *)
  let%bind () = same_state state' [] in

  (* add ([tvb;tvc] ∈ { [int;unit] , [unit;int] , [map(int,unit);map(int,unit)] } ) to the state *)
  let tc_allowed_bc : type_value list list = [
    [ tval_int ; tval_unit ] ;
    [ tval_unit ; tval_int ] ;
    [ tval_map_int_unit; tval_map_int_unit ] ;
  ] in
  let tc_bc = make_c_typeclass_simpl 1 None [tvb;tvc] tc_allowed_bc in
  let state'' = add_constraint repr state' (SC_Typeclass tc_bc) in
  (* assert state'' = … *)
  let%bind () = same_state state'' [
      (ConstraintIdentifier 1L, tc_bc)
    ]
  in

  
  (* add ([tvb] ∈ { [int] , [unit] } ) to the state *)
  let tc_allowed_b : type_value list list = [
    [ tval_int ] ;
    [ tval_unit ] ;
  ] in
  let tc_b = make_c_typeclass_simpl 2 (Some 1) [tvb;tvc] tc_allowed_b in
  let state''' = add_constraint repr state'' (SC_Typeclass tc_b) in
  (* assert state''' = … *)
  let%bind () = same_state state''' [
      (ConstraintIdentifier 1L, tc_bc) ;
      (ConstraintIdentifier 2L, tc_b)
    ]
  in

  (* merging tvc to tva *)
  let merge_keys  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvc in
    let new_repr = tva in
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state'''' = merge_aliases merge_keys state''' in
  (* assert that c has been merged to a in state'''' *)
  (* state'''' = same as above, because this indexer does not store any type variable. *)
  let%bind () = same_state state'''' [
      (ConstraintIdentifier 1L, tc_bc) ;
      (ConstraintIdentifier 2L, tc_b)
    ]
  in
  ok ()
  
