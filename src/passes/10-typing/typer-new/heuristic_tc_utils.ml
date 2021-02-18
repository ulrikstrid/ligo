[@@@warning "-32"]
module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

open Trace
open Typer_common.Errors
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet

module Utils = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types
  type type_variable = Type_variable.t

  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  (* open All_plugins *)

  let get_cells (tc : c_typeclass_simpl) =
    List.flatten tc.tc

  let map_cells (f : type_value -> type_value) (tc : c_typeclass_simpl) =
    { tc with tc = List.map (List.map f) tc.tc }

  type column = (type_variable * type_value list)
  type columns = column list
  let loop3 : 'e 'x 'a 'b 'c . ('x -> ('a * 'b * 'c, 'e) result) -> ('a * 'b * 'c) -> (('a -> 'a -> 'a) * ('b -> 'b -> 'b) * ('c -> 'c -> 'c)) -> 'x list -> (('a * 'b * 'c), 'e) result =
    fun f (a0, b0, c0) (a,b,c) xs ->
    let%bind r = bind_map_list f xs in
    let (as_, bs, cs) = List.split3 r in
    ok (List.fold_left a a0 as_, List.fold_left b b0 bs, List.fold_left c c0 cs)

   let rec transpose_list_of_lists (matrix : type_value list list) =
      match matrix with
        [] -> []
      | (_::_)::_ -> (List.map List.hd matrix) :: transpose_list_of_lists (List.map List.tl matrix)
      | []::_ -> assert (List.for_all List.is_empty matrix); []

  type 'a all_equal = Empty | All_equal_to of 'a | Different
  let all_equal cmp = function
    | [] -> Empty
    | hd :: tl -> if List.for_all (fun x -> cmp x hd = 0) tl then All_equal_to hd else Different  
   
  let update_columns3 : (columns -> (column Rope.SimpleRope.t * 'b * 'c, _) result) -> c_typeclass_simpl -> (c_typeclass_simpl * 'b * 'c, _) result =
    fun f tc ->
    let rec transpose (headers : type_variable list) (matrix : type_value list list) =
      match headers with
        [] -> []
      | hd::tl -> (hd, (List.map List.hd matrix)) :: transpose tl (List.map List.tl matrix) in
    let rec transpose_back (columns : columns) : (type_variable list * type_value list list) =
      match columns with
        [] -> [], []
      | (_header, _::_)::_ ->
        let (headers, matrix) = transpose_back @@ List.map (fun (header, cells) -> header, List.tl cells) columns in
        headers, (List.map (fun (_header,cells) -> List.hd cells) columns :: matrix)
      | _ -> assert (List.for_all List.is_empty @@ List.map snd columns); (List.map fst columns), []
    in
    (*let transpose_back cs = let (hs, m) = transpose_back cs in (hs, List.rev m) in*)
    let%bind updated, b, c = f @@ transpose tc.args tc.tc in
    let headers', matrix' = transpose_back @@ Rope.SimpleRope.list_of_rope updated in
    ok ({ tc with args = headers'; tc = matrix' }, b, c)
    

  let filter_lines (f : _ -> ([`headers] * type_variable list * [`line] * type_value list) -> (bool*_, _) result) (tc_org : c_typeclass_simpl) =
    let%bind (updated,_) =
      bind_fold_list (fun (acc,tc) line ->
          let%bind b,tc = f tc (`headers, tc_org.args, `line, line) in
          if b then ok (line :: acc,tc) else ok (acc,tc)) ([],tc_org) tc_org.tc
    in
    ok { tc_org with tc = List.rev updated }

  (* Check that the typeclass is a rectangular matrix, with one column
    per argument. *)
  let check_typeclass_rectangular ({ reason_typeclass_simpl=_; tc; args } as tcs : c_typeclass_simpl) =
    let nargs = List.length args in
    if (List.for_all (fun allowed -> List.length allowed = nargs) tc)
    then ok tcs
    else fail typeclass_not_a_rectangular_matrix

  (* Check that the transposed typeclass is a rectangular matrix, with
    one row per argument. *)
  let check_typeclass_transposed_rectangular (tc : (type_variable * type_value list) list) =
    match tc with
      [] -> ok tc
    | (_, hd) :: tl ->
      let hdlen = List.length hd in
      if List.for_all (fun (_, l) -> List.length l = hdlen) tl
      then ok tc
      else fail typeclass_not_a_rectangular_matrix

  let get_tag_and_args_of_constant (tv : type_value) =
    match tv.wrap_content with
    | P_constant { p_ctor_tag; p_ctor_args } -> ok (p_ctor_tag, p_ctor_args)
    | P_row { p_row_tag; p_row_args } -> ignore (p_row_tag, p_row_args); failwith "TODO: return p_row_tag, p_row_args similarly to P_constant"
    | P_forall _ ->
      (* In this case we would need to do specialization.
        For now we just leave as-is and don't deduce anything *)
      failwith "Unsuported"
    | P_variable _ ->
      (* In this case we  *)
      failwith "TODO : P_variable"
    | P_apply _ ->
      (* In this case we would need to do β-reduction, if
        possible, or invoke another heuristic.
        For now we just leave as-is and don't deduce anything *)
      failwith "TODO"
    | P_abs _ ->
      failwith "TODO"
    | P_constraint _ ->
      failwith "TODO"
  
let all_equal' : (type_variable) -> type_value list -> (constructor_or_row * type_variable list * type_value list list) all_equal =
  fun repr_x -> function
      [] -> Empty
    | hd :: tl ->
      match hd.wrap_content with
        P_forall _ -> failwith "forall in typeclass cells is not supported"
      | P_variable _ -> Different
      | P_apply _ -> Different
      | P_abs _ -> Different
      | P_constraint _ -> failwith "kinding error: constraints have kind Constraint, but the cells of a typeclass can only contain types (i.e. kind *)"
      | P_constant { p_ctor_tag=hd_ctor_tag; p_ctor_args=hd_ctor_args } ->
        if List.for_all (function { Location.wrap_content = P_constant { p_ctor_tag; p_ctor_args=_ }} -> Compare.constant_tag p_ctor_tag hd_ctor_tag = 0 | _ -> false) tl
        then
          let fresh_vars = List.map (fun _arg -> Core.fresh_type_variable ()) hd_ctor_args in
          let deduced : c_constructor_simpl = {
            id_constructor_simpl = ConstraintIdentifier.fresh ();
            original_id = None;
            reason_constr_simpl = "inferred because it is the only remaining possibility at this point according to the typeclass [TODO:link to the typeclass here]" ;
            tv = repr_x;
            c_tag = hd_ctor_tag;
            tv_list = fresh_vars
          } in
          All_equal_to (`Constructor deduced, fresh_vars, List.map (function { Location.wrap_content = P_constant { p_ctor_tag=_; p_ctor_args }} -> p_ctor_args | _ -> failwith "impossible") (hd :: tl))
        else Different
      | P_row { p_row_tag=hd_row_tag; p_row_args=hd_row_args } ->
        let hd_labels = LMap.keys hd_row_args in
        if List.for_all (function { Location.wrap_content = P_row { p_row_tag; p_row_args=_ }} ->
                           Compare.row_tag p_row_tag hd_row_tag = 0
                           && List.compare ~compare:Compare.label hd_labels (LMap.keys hd_row_args) = 0
                         | _ -> false)
                        tl
        then
          let deduced : c_row_simpl = {
            id_row_simpl = ConstraintIdentifier.fresh ();
            original_id = None;
            reason_row_simpl = "inferred because it is the only remaining possibility at this point according to the typeclass [TODO:link to the typeclass here]" ;
            tv = repr_x;
            r_tag = hd_row_tag;
            tv_map = LMap.map (fun { associated_value=_; michelson_annotation; decl_pos } -> ({ associated_variable=Core.fresh_type_variable (); michelson_annotation; decl_pos} : row_variable)) hd_row_args
          } in
          let fresh_vars = List.map (fun x -> x.associated_variable) @@ LMap.values deduced.tv_map in
          All_equal_to (`Row deduced, fresh_vars, List.map (function { Location.wrap_content = P_row { p_row_tag=_; p_row_args }} -> List.map (fun x -> x.associated_value) @@ LMap.values p_row_args | _ -> failwith "impossible") (hd :: tl))
        else Different
      

(* input:
     x ? [ map3( nat , unit , float ) ; map3( bytes , mutez , float ) ]
   output:
     true,
     [ x = map( m , n , o ) ; o = float ( ) ],
     [ m ? [ nat  ; bytes ]
       n ? [ unit ; mutez ] ]
   input:
     x ? [ record( a = nat , b = unit , c = float ) ; record( a = bytes , b = mutez , c = float ) ]
   output:
     true,
     [ x = record( a=m , b=n , c=o ) ; o = float ( ) ],
     [ m ? [ nat  ; bytes ]
       n ? [ unit ; mutez ] ] *)
let rec replace_var_and_possibilities_1
    (repr:type_variable -> type_variable)
    ((x : type_variable), (possibilities_for_x : type_value list))
    : (column Rope.SimpleRope.t * _ * bool, _) result =
  let open Rope.SimpleRope in
  (*let%bind tags_and_args = bind_map_list get_tag_and_args_of_constant possibilities_for_x in
  let tags_of_constructors, arguments_of_constructors = List.split tags_and_args in*)
  match all_equal' (repr x) possibilities_for_x  with
  | Different ->
    (* The "changed" boolean return indicates whether any update was done.
       It is used to detect when the variable doesn't need any further cleanup. *)
    ok (singleton (x, possibilities_for_x), empty, false)            (* Leave as-is, don't deduce anything *)
  | Empty ->
    (* TODO: keep track of the constraints used to refine the
       typeclass so far. *)
    (* fail @@ typeclass_error
     *   "original expected by typeclass"
     *   "<actual> partially guessed so far (needs a recursive substitution)" *)
    (* TODO: possible bug: if there is nothing left because everything was inferred, we shouldn't fail and just continue with an empty TC… can this happen? *)
    fail @@ corner_case "type error: the typeclass does not allow any type for \
                         the variable %a:PP_variable:x at this point"
  | All_equal_to (deduced, fresh_vars, arguments_of_constructors) ->
      (* discard the identical tags, splice their arguments instead, and deduce the x = tag(…) constraint *)

      let%bind (rec_cleaned, rec_deduced, _rec_changed) =
        replace_var_and_possibilities_rec repr (List.combine fresh_vars (transpose_list_of_lists arguments_of_constructors))
      in
      (* The "changed" boolean return indicates whether any update was done.
         It is used to prevent removal + update of the typeclass if it wasn't modified. *)
      ok (rec_cleaned, pair (singleton deduced) rec_deduced, true)

  and replace_var_and_possibilities_rec repr matrix =
    let open Rope.SimpleRope in
    (loop3 (replace_var_and_possibilities_1 repr) (empty, empty, false) (pair, pair, (||))) matrix

type deduce_and_clean_result = {
  deduced : constructor_or_row list ;
  cleaned : c_typeclass_simpl ;
  changed : bool
}

let deduce_and_clean : (_ -> _) -> c_typeclass_simpl -> (deduce_and_clean_result, _) result = fun repr tcs ->
  let open Rope.SimpleRope in
  Format.printf "In deduce_and_clean for : %a\n%!" PP.c_typeclass_simpl_short tcs;
  (* ex.   [ x                             ; z      ]
       ∈ [ [ map3( nat   , unit  , float ) ; int    ] ;
           [ map3( bytes , mutez , float ) ; string ] ] *)
  let%bind (cleaned, deduced, changed) = update_columns3 (replace_var_and_possibilities_rec repr) tcs in
  
  (* ex. cleaned:
           [ fresh_x_1 ; fresh_x_2 ; y      ]
       ∈ [ [ nat       ; unit      ; int    ]
           [ bytes     ; mutez     ; string ] ]
         deduced:
         [ x         = map3  ( fresh_x_1 , fresh_x_2 , fresh_x_3 ) ;
           fresh_x_3 = float (                                   ) ; ] *)
  ok { deduced = list_of_rope deduced ; cleaned ; changed }

 let wrapped_deduce_and_clean repr tc ~(original:c_typeclass_simpl) =
  let%bind {deduced; cleaned; changed} = deduce_and_clean repr tc in
  let cleaned = SC_Typeclass { cleaned with original_id = Some original.id_typeclass_simpl; id_typeclass_simpl = ConstraintIdentifier.fresh ()} in
  let aux : constructor_or_row -> type_constraint_simpl = function
      `Constructor x -> SC_Constructor x
    | `Row x -> SC_Row x in
  let deduced = List.map aux deduced in
  ok (deduced, cleaned,changed)

end
