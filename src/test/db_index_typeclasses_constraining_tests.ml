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

module Typeclasses_constraining_tests = struct
  include Test_vars
  include TypeclassesConstraining
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv
  let same_state sa sb =
    (* This index is not implemented yet, its state is just a unit value *)
    let sa = UnionFind.ReprMap.bindings @@ (get_state_for_tests sa) in
    let%bind () = tst_assert "Length saf = Length sbf" (List.length sa = List.length sb) in
    bind_list_iter
      (fun (a , b) ->
        let%bind () = tst_assert "type variable =" (Ast_typed.Compare.type_variable (fst a) (fst b) = 0) in
        let%bind () = tst_assert "c_typeclass_simpl set =" (List.compare ~compare:Ast_typed.Compare.c_typeclass_simpl (PolySet.elements (snd a)) (PolySet.elements (snd b)) = 0) in
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

let typeclasses_constraining () =
  let open Typeclasses_constraining_tests in
  let set l = (PolySet.add_list l @@ PolySet.create ~cmp:Ast_typed.Compare.c_typeclass_simpl).set in
  (* create empty state *)
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in
  (* assert state = {} *)
  let%bind () = same_state state [] in

  (* add `tva = unit()' to the state *)
  let ctor_a = make_c_constructor_simpl tva C_unit [] in
  let state' = add_constraint repr state (SC_Constructor ctor_a) in                                           
  (* assert state' = {} because only typeclass constraints are indexed by this indexer. *)
  let%bind () = same_state state' [] in

  (* add ([tvb;tvc] ∈ { [int;unit] , [unit;int] , [map(int,unit);map(int,unit)] } ) to the state *)
  let tc_allowed_bc : type_value list list = [
    [ tval_int ; tval_unit ] ;
    [ tval_unit ; tval_int ] ;
    [ tval_map_int_unit; tval_map_int_unit ] ;
  ] in
  let tc_bc = make_c_typeclass_simpl 1 None [tvb;tvc] tc_allowed_bc in
  let state'' = add_constraint repr state' (SC_Typeclass tc_bc) in
  (* assert state'' = [], [] because there is no refined typeclass yet *)
  let%bind () = same_state state'' [
      (tvb, set[tc_bc]);
      (tvc, set[tc_bc]);
    ]
  in

  (* add ([tvb] ∈ { [int] , [unit] }) as a refinement of tc_bc to the state *)
  let tc_allowed_b : type_value list list = [
    [ tval_int ] ;
    [ tval_unit ] ;
  ] in
  let tc_b = make_c_typeclass_simpl 2 (Some 1) [tvb] tc_allowed_b in
  let state''' = add_constraint repr state'' (SC_Typeclass tc_b) in
  (* assert state''' = … *)
  let%bind () = same_state state''' [
      (tvb, set[tc_bc;tc_b]);  (* TODO: should the old one still be there? *)
      (tvc, set[tc_bc]);        (* TODO: should the old one still be there? *)
    ]
  in

  (* merging tvb to tva *)
  let merge_keys  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvb in
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
      (tva, set[tc_bc;tc_b]);  (* TODO: should the old one still be there? *)
      (tvc, set[tc_bc]);        (* TODO: should the old one still be there? *)
    ] in
  ok ()
  