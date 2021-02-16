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

  let filter_lines (f : ([`headers] * type_variable list * [`line] * type_value list) -> (bool, _) result) (tc : c_typeclass_simpl) =
    let%bind updated =
      bind_fold_list (fun acc line ->
          let%bind b = f (`headers, tc.args, `line, line) in
          if b then ok (line :: acc) else ok acc) [] tc.tc
    in
    ok { tc with tc = List.rev updated }

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


  (* transpose ([x;z] ∈ [ [map(nat,unit) ; int    ; ] ;
                          [map(unit,nat) ; string ; ] ;
                          [map(int,int)  ; unit   ; ] ; ])
    will return [ x ? [ map(nat,unit) ; map(unit,nat) ; map(int,int) ; ] ;
                  z ? [ int           ; string        ; unit         ; ] ; ] *)
  let transpose : c_typeclass_simpl -> ((type_variable * type_value list) list, _) result =
    fun { reason_typeclass_simpl = _; tc; args } ->
    bind_fold_list
      (fun accs allowed_tuple ->
        List.map2 (fun (var, acc) allowed_type -> (var, allowed_type :: acc)) accs allowed_tuple
          ~ok ~fail:(fun _ _ -> fail @@ internal_error __LOC__ "typeclass is not represented by a rectangular matrix"))
      (List.map (fun var -> var, []) args)
      tc
    >>|? List.map (fun (var, acc) -> (var, List.rev acc))
    >>? check_typeclass_transposed_rectangular

  (* transpose_back [ x ? [ map(nat,unit) ; map(unit,nat) ; map(int,int) ; ] ;
                      z ? [ int           ; string        ; unit         ; ] ; ]
    will return ([x;z] ∈ [ [map(nat,unit) ; int    ; ] ;
                            [map(unit,nat) ; string ; ] ;
                            [map(int,int)  ; unit   ; ] ; ]) *)
  let transpose_back : _ -> _ -> (type_variable * type_value list) list -> (c_typeclass_simpl, _) result =
    fun (reason_typeclass_simpl, original_id) id_typeclass_simpl tcs ->
    let%bind tc =
      match tcs with
      | [] -> ok []
      | (_, hd_allowed_types) :: _ ->
        bind_fold_list
          (fun allowed_tuples allowed_types ->
            List.map2 (fun allowed_tuple allowed_type -> allowed_type :: allowed_tuple) allowed_tuples allowed_types
              ~ok ~fail:(fun _ _ -> fail @@ internal_error __LOC__ "transposed typeclass is not represented by a rectangular matrix"))
          (List.map (fun _ -> []) hd_allowed_types)
          (List.map snd tcs)
        >>|? List.map (fun allowed_typle -> List.rev allowed_typle)
    in
    let args = List.map fst tcs in
    check_typeclass_rectangular @@
    { tc_bound = [](*TODO*); tc_constraints = [](*TODO*); reason_typeclass_simpl; original_id; id_typeclass_simpl; tc; args }

  type 'a all_equal = Empty | All_equal_to of 'a | Different
  let all_equal cmp = function
    | [] -> Empty
    | hd :: tl -> if List.for_all (fun x -> cmp x hd = 0) tl then All_equal_to hd else Different
  
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
let replace_var_and_possibilities_1 (repr:type_variable -> type_variable) ((x : type_variable) , (possibilities_for_x : type_value list)) =
  let%bind tags_and_args = bind_map_list get_tag_and_args_of_constant possibilities_for_x in
  let tags_of_constructors, arguments_of_constructors = List.split @@ tags_and_args in
  match all_equal Compare.constant_tag tags_of_constructors with
  | Different ->
    (* The "changed" boolean return indicates whether any update was done.
       It is used to detect when the variable doesn't need any further cleanup. *)
    ok ( false, [ (x, possibilities_for_x) ], [] )            (* Leave as-is, don't deduce anything *)
  | Empty ->
    (* TODO: keep track of the constraints used to refine the
       typeclass so far. *)
    (* fail @@ typeclass_error
     *   "original expected by typeclass"
     *   "<actual> partially guessed so far (needs a recursive substitution)" *)
    failwith "type error: the typeclass does not allow any type for \
              the variable %a:PP_variable:x at this point"
  | All_equal_to c_tag ->
    match arguments_of_constructors with
    | [] -> failwith "the typeclass does not allow any possibilities \
                      for the variable %a:PP_variable:x at this point"
    | (arguments_of_first_constructor :: _) as arguments_of_constructors ->
      let fresh_vars = List.map (fun _arg -> Core.fresh_type_variable ()) arguments_of_first_constructor in
      let deduced : c_constructor_simpl = {
        id_constructor_simpl = ConstraintIdentifier 0L;
        original_id = None;
        reason_constr_simpl = "inferred because it is the only remaining possibility at this point according to the typeclass [TODO:link to the typeclass here]" ;
        tv = (repr x);
        c_tag ;
        tv_list = fresh_vars
      } in
      (* discard the identical tags, splice their arguments instead, and deduce the x = tag(…) constraint *)
      let sub_part_of_typeclass = {
        tc_bound = [](*TODO*); tc_constraints = [](*TODO*);
        reason_typeclass_simpl = Format.asprintf
            "sub-part of a typeclass: expansion of the possible \
             arguments for the constructor associated with %a"
            PP.type_variable (repr x);
        original_id = None;     (* TODO this and the is_mandatory_constraint are not actually used, should use a different type without these fields. *)
        id_typeclass_simpl = ConstraintIdentifier (-1L) ; (* TODO: this and the reason_typeclass_simpl should simply not be used here *)
        args = fresh_vars ;
        tc = arguments_of_constructors ;
      } in
      let%bind possibilities_alist = transpose sub_part_of_typeclass in
      (* The "changed" boolean return indicates whether any update was done.
         It is used to detect when the variable doesn't need any further cleanup. *)
      ok (true, possibilities_alist, [deduced])

let rec replace_var_and_possibilities_rec repr ((x : type_variable) , (possibilities_for_x : type_value list)) =
  let open Rope.SimpleRope in
  let%bind (changed1, possibilities_alist, deduced) = replace_var_and_possibilities_1 repr (x, possibilities_for_x) in
  if changed1 then
    (* the initial var_and_possibilities has been changed, recursively
       replace in the resulting vars and their possibilities, and
       aggregate the deduced constraints. *)
    let%bind (_changed, vp, more_deduced) = replace_vars_and_possibilities_list repr possibilities_alist in
    ok (true, vp, pair (rope_of_list deduced) more_deduced)
  else
    ok (changed1, rope_of_list possibilities_alist, rope_of_list deduced)

and replace_vars_and_possibilities_list repr possibilities_alist =
  let open Rope.SimpleRope in
  bind_fold_list
    (fun (changed_so_far, vps, ds) x ->
       let%bind (changed, vp, d) = replace_var_and_possibilities_rec repr x in
       ok (changed_so_far || changed, pair vps vp, pair ds d))
    (false, empty, empty)
    possibilities_alist

let replace_vars_and_possibilities repr possibilities_alist =
  let open Rope.SimpleRope in
  let%bind (_changed, possibilities_alist, deduced) = replace_vars_and_possibilities_list repr possibilities_alist in
  ok (list_of_rope possibilities_alist, list_of_rope deduced)

type deduce_and_clean_result = {
  deduced : c_constructor_simpl list ;
  cleaned : c_typeclass_simpl ;
}

let deduce_and_clean : (_ -> _) -> c_typeclass_simpl -> (deduce_and_clean_result, _) result = fun repr tcs ->
  Format.printf "In deduce_and_clean for : %a\n%!" PP.c_typeclass_simpl_short tcs;
  (* ex.   [ x                             ; z      ]
       ∈ [ [ map3( nat   , unit  , float ) ; int    ] ;
           [ map3( bytes , mutez , float ) ; string ] ] *)
  let%bind possibilities_alist = transpose tcs in
  (* ex. [ x ? [ map3( nat , unit , float ) ; map3( bytes , mutez , float ) ; ] ;
           z ? [ int                        ; string                        ; ] ; ] *)
  let%bind (vars_and_possibilities, deduced) = replace_vars_and_possibilities repr possibilities_alist in
  (* ex. possibilities_alist:
         [   fresh_x_1 ? [ nat   ; bytes  ] ;
             fresh_x_2 ? [ unit  ; mutez  ] ;
             y         ? [ int   ; string ]     ]
         deduced:
         [ x         = map3  ( fresh_x_1 , fresh_x_2 , fresh_x_3 ) ;
           fresh_x_3 = float (                                   ) ; ] *)
  let%bind cleaned = transpose_back (tcs.reason_typeclass_simpl, tcs.original_id) tcs.id_typeclass_simpl vars_and_possibilities in
  ok { deduced ; cleaned }

let wrapped_deduce_and_clean repr tc ~(original:c_typeclass_simpl) =
  let open Type_variable_abstraction.Reasons in
  let%bind {deduced; cleaned} = deduce_and_clean repr tc in
    (* TODO: this is because we cannot return a simplified constraint,
     and instead need to retun a constraint as it would appear if it
     came from the module (generated by the ill-named module
     "Wrap"). type_constraint_simpl is more or less a subset of
     type_constraint, but some parts have been shuffled
     around. Hopefully this can be sorted out so that we don't need a
     dummy value for the srcloc and maybe even so that we don't need a
     conversion (one may dream). *)
  let tc_args = List.map (fun x -> wrap (Todo "no idea") @@ P_variable (repr x)) cleaned.args in
  let cleaned : type_constraint = {
      reason = cleaned.reason_typeclass_simpl;
      c = C_typeclass {
          tc_bound = [](*TODO*); tc_constraints = [](*TODO*);
        tc_args ;
        typeclass = cleaned.tc;
        original_id = original.original_id;
      }
    }
  in
  let aux (x : c_constructor_simpl) : type_constraint = {
    reason = "inferred: only possible type for that variable in the typeclass";
    c = C_equation {
      aval = wrap (Todo "?") @@ P_variable (repr x.tv) ;
      bval = wrap (Todo "? generated") @@
              P_constant {
                p_ctor_tag  = x.c_tag ;
                p_ctor_args = List.map
                  (fun v -> wrap (Todo "? probably generated") @@ P_variable (repr v))
                  x.tv_list ;
              }
      }
    }
  in
  let deduced : type_constraint list = List.map aux deduced in
  ok (deduced, cleaned)

end
