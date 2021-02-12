(* selector / propagation rule for restricting a type class
     (α₁, α₂, …) ∈ { (τ₁₁, τ₁₂, …) , … }
   to the possible cases, given a second hypothesis of the form
     αᵢ = κ(β₁, β₂, …)
      or
     αᵢ = ρ(ℓᵢ : βᵢ, …)
   It restricts the number of possible cases and replaces αᵢ in
   tuple of constrained variables so that the βⱼ are constrained
   instead.

   This rule can deduce a new assignment for other variables
   constrained by the typeclass if every possible type for that
   variable uses the same type constructor. *)

(* TODO: have a heuristic that restricts typeclass constraints with
   repeated or aliased type variables in the arguments, i.e. of the
   form […;x;…;y;…] ∈ […] where x and y are identical or aliased. *)

open Trace
open Typer_common.Errors
open Simple_utils

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins
  module type S = sig
    val grouped_by_variable : Type_variable.t  Grouped_by_variable.t
    val assignments : Type_variable.t Assignments.t
    val typeclasses_constraining : Type_variable.t Typeclasses_constraining.t
  end
end

module M = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types
  open Type_variable_abstraction.Reasons

  module Utils = Heuristic_tc_fundep_utils.Utils(Type_variable)(Type_variable_abstraction)
  open Utils
  open Utils.All_plugins

  type flds = (module INDEXES(Type_variable)(Type_variable_abstraction).S)

  type selector_output = {
    tc : c_typeclass_simpl ;
    c :  constructor_or_row ;
  }

  let heuristic_name = "tc_fundep"
  
(* ***********************************************************************
 * Selector
 * *********************************************************************** *)

(* selector:
 *   find in db "αᵢ = κ(β…)" and "(…,αᵢ,…) ∈ ∃δ…, c… => [ (…,τᵢⱼ,…) … ]"
 *   find in db "αᵢ = Ξ(ℓ:β…)" and "(…,αᵢ,…) ∈ ∃δ…, c… => [ (τ…) … ]" *)
  
(* Find typeclass constraints in the dbs which constrain c_or_r.tv *)
let selector_by_variable : (type_variable -> type_variable) -> flds -> constructor_or_row -> type_variable -> selector_output list =
  fun repr (module Indexes) c_or_r tv ->
  let typeclasses = (Typeclasses_constraining.get_typeclasses_constraining_list (repr tv) Indexes.typeclasses_constraining) in
  List.map (fun tc -> { tc ; c = c_or_r }) typeclasses

(* Find constructor constraints α = κ(β …) and and row constraints
   α = Ξ(ℓ:β …) where α is one of the variables constrained by the
   typeclass constraint tcs. *)
let selector_by_tc : (type_variable -> type_variable) -> flds -> c_typeclass_simpl -> selector_output list =
  fun repr (module Indexes) tc ->
  let aux tv =
    (* Since we are only refining the typeclass one type expression
       node at a time, we only need the top-level assignment for
       that variable, e.g. α = κ(βᵢ, …). We can therefore look
       directly in the assignments. *)
    match Assignments.find_opt (repr tv) Indexes.assignments with
    | Some cr -> [{ tc ; c = cr }]
    | None   -> [] in
  List.flatten @@ List.map aux tc.args

let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  fun repr type_constraint_simpl indexes ->
  match type_constraint_simpl with
    SC_Constructor c  -> selector_by_variable repr indexes (`Constructor c) c.tv
  | SC_Row r          -> selector_by_variable repr indexes (`Row         r) r.tv
  | SC_Alias        _  -> [] (* TODO: this case should go away since aliases are handled by the solver structure *)
  | SC_Poly         _  -> []
  | SC_Access_label _  -> []
  | SC_Typeclass   tc -> selector_by_tc repr indexes tc

(* selector:
 *   find in db γᵢ = κ(β…)  and ∃δ…, c… => (…,αᵢ,…) ∈ [ (…,P_variable γᵢ,…) … ]
 *   find in db γᵢ = Ξ(ℓ:β…) and ∃δ…, c… => (…,αᵢ,…) ∈ [ (…,P_variable γᵢ,…) … ] *)

(* When (αᵢ, …) ∈ { (τ, …) , … } and β = κ(δ …) are in the db,
   aliasing α and β should check if they are non-empty, and in that
   case produce a selector_output for all pairs. This will involve a
   lookup to see if α is constrained by a typeclass
   (typeclasses_constraining indexer). Add to this the logic for
   refined_typeclass vs. typeclass. *)

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  let a_tcs = (Typeclasses_constraining.get_typeclasses_constraining_list a Indexes.typeclasses_constraining) in
  let b_tcs = (Typeclasses_constraining.get_typeclasses_constraining_list b Indexes.typeclasses_constraining) in
  let a_lhs_constructors = Grouped_by_variable.get_constructors_by_lhs a Indexes.grouped_by_variable in
  let b_lhs_constructors = Grouped_by_variable.get_constructors_by_lhs b Indexes.grouped_by_variable in
  let a_lhs_rows = Grouped_by_variable.get_rows_by_lhs a Indexes.grouped_by_variable in
  let b_lhs_rows = Grouped_by_variable.get_rows_by_lhs b Indexes.grouped_by_variable in
  let a_ctors = MultiSet.map_elements (fun a -> `Constructor a) a_lhs_constructors in
  let a_rows  = MultiSet.map_elements (fun a -> `Row a        ) a_lhs_rows         in
  let b_ctors = MultiSet.map_elements (fun a -> `Constructor a) b_lhs_constructors in
  let b_rows  = MultiSet.map_elements (fun a -> `Row a        ) b_lhs_rows         in
  List.flatten @@
  List.map
    (fun tc ->
       List.map
         (fun c ->
            { tc ; c })
         (a_ctors @ b_ctors @ a_rows @ b_rows ))
    (a_tcs @ b_tcs)

let get_referenced_constraints ({ tc; c } : selector_output) : type_constraint_simpl list =
  [
    SC_Typeclass tc;
    (match c with `Constructor c -> SC_Constructor c | `Row r -> SC_Row r);
  ]

(* ***********************************************************************
 * Propagator
 * *********************************************************************** *)



(* 

type t = { m : int; n : unit }

type lens_m = int  * { m : _;   n : unit }
type lens_n = unit * { m : int; n : _ }

lens_m (v:t) = t.m, fun new -> { m = new; n = v.n }
lens_n (v:t) = t.n, fun new -> { m = v.m; n = new }

val get_lenses : constructor_or_row -> type_value -> (type_value list * (type_variable list -> constructor_or_row)) option

check shape
list(_) != map(_)
map(_,_,_) != map(_,_,_)
list(_)             != record(f=_,g=_,h=_)
record(f=_,g=_,h=_) != record(f=_,g=_,h=_)

extract
map(int,string,bool)            → [int,string,bool]
record(f=int, g=string, h=bool) → [int,string,bool]

replacement
map(u,i,o)            → [a,b,c] → map(a, b, c)
record(f=u, g=i, h=o) → [a,b,c] → record(f=a, g=b, h=c)
-----------------------------------------------------------
x = map(u,i,o)
(x,y) ∈ [ [ map(int, string, eXist) ; int];
        [ [ list(float)            ; bool] ]

1) delete line list(float) because list(_) != map(_)

x = ctor (inject fresh vars "pointwise")
→ x = map(a, b, c)

return constraint fresh var == arg pointwise
→ return a = int, b = string, c = eXist

simplified constraint
(a,b,c,y) ∈ [ [ int ; string ; eXist ; int] ]
-----------------------------------------------------------
x = record(f=u,g=i,h=o)
(x,y) ∈ [ [ record(f=int, g=string, h=eXist) ; int  ];
        [ [ list(float)                      ; bool ] ]

x = row_ctor (inject fresh vars "pointwise")
→ x = record(f=a, g=b, h=c)

return constraint fresh var == arg pointwise
→ return a = int, b = string, c = bool

simplified constraint
(a,b,c,y) ∈ [ [ int ; string ; eXist ; int] ]





 *)









(* module Matrix = struct
 *   let matrix (tc : c_typeclass_simpl) =
 *     let { reason_typeclass_simpl; id_typeclass_simpl; original_id; tc; args } = tc in
 *     ??
 * end *)


(* type tag = [ `Constructor of constant_tag | `Row of row_tag ]
 * type 'a comparer = 'a -> 'a -> int *)

(* let get_tag : constructor_or_row -> tag = function
 *     `Constructor c -> `Constructor c.c_tag
 *   | `Row r -> `Row r.r_tag
 * let get_tag' = function P_constant { p_ctor_tag; p_ctor_args } -> ??
 *                       | P_row { p_row_tag; p_row_args } -> ??
 *                       (P_forall _ | P_variable _ | P_apply _ | P_row _ | P_constant _) -> None *)

(* type arg
 * let get_arg : constructor_or_row -> arg = ?? *)

(* module Eq = struct include Ast_typed.Compare let (==) fa b = fa b = 0 end *)

let restrict_one (cr : constructor_or_row) (allowed : type_value) =
  match cr, allowed.wrap_content with
  | `Constructor { reason_constr_simpl=_; tv=_; c_tag; tv_list }, P_constant { p_ctor_tag; p_ctor_args } ->
    if Compare.constant_tag c_tag p_ctor_tag = 0
    then if List.compare_lengths tv_list p_ctor_args = 0
      then Some p_ctor_args
(*      then Some (`Constructor p_ctor_args)*)
      else None (* case removed because type constructors are different *)
    else None   (* case removed because argument lists are of different lengths *)
  | `Row _, P_row _ -> failwith "TODO: support P_row similarly to P_constant"
(*  | `Row { reason_row_simpl=_; id_row_simpl=_; original_id=_; tv=_; r_tag; tv_map }, P_row { p_row_tag; p_row_args } ->
    if Compare.row_tag r_tag p_row_tag = 0
    then if List.compare ~compare:Compare.label (LMap.keys tv_map) (LMap.keys p_row_args) = 0
      then Some (`Row p_row_args)
      else None (* case removed because type constructors are different *)
    else None   (* case removed because argument lists are of different lengths *)
*)
  | _, (P_forall _ | P_variable _ | P_apply _ | P_row _ | P_constant _) -> None (* TODO: does this mean that we can't satisfy these constraints? *)

(* Restricts a typeclass to the possible cases given v = k(a, …) in c *)
let restrict repr (constructor_or_row : constructor_or_row) (tcs : c_typeclass_simpl) =
  let (tv_list, tv) = match constructor_or_row with
    | `Row r -> List.map (fun {associated_variable} -> associated_variable) @@ LMap.to_list r.tv_map , (repr r.tv)
    | `Constructor c -> c.tv_list , (repr c.tv)
  in
  let index =
    let repr_tv = (repr tv) in
    try List.find_index (fun x -> Compare.type_variable repr_tv (repr x) = 0) tcs.args
    with Failure _ ->
      failwith (Format.asprintf "problem: couldn't find tv = %a in tcs.args = %a"
                  PP.type_variable repr_tv (PP_helpers.list_sep_d PP.type_variable) tcs.args);
  in
  (* Eliminate the impossible cases and splice in the type arguments
     for the possible cases: *)
  let aux allowed_tuple =
    splice_or_none (fun allowed -> restrict_one constructor_or_row allowed) index allowed_tuple in
  let tc = List.filter_map aux tcs.tc in
  (* Replace the corresponding typeclass argument with the type
     variables passed to the type constructor *)
  let args = splice (fun _arg -> tv_list) index tcs.args in
  let id_typeclass_simpl = tcs.id_typeclass_simpl in
  { tc_bound = [](*TODO*); tc_constraints = [](*TODO*); reason_typeclass_simpl = tcs.reason_typeclass_simpl; original_id = tcs.original_id; id_typeclass_simpl ; tc ; args }

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

let propagator : (selector_output, typer_error) Type_variable_abstraction.Solver_types.propagator =
  fun selected repr ->
  (* The selector is expected to provide constraints with the shape (α
     = κ(β, …)) and to update the private storage to keep track of the
     refined typeclass *)
  let () = Format.printf "and tv: %a and repr tv :%a \n%!" (PP_helpers.list_sep_d PP.type_variable) selected.tc.args (PP_helpers.list_sep_d PP.type_variable) @@ List.map repr selected.tc.args in
  let restricted = restrict repr selected.c selected.tc in
  let () = Format.printf "restricted: %a\n!" PP.c_typeclass_simpl_short restricted in
  let%bind {deduced ; cleaned} = deduce_and_clean repr restricted in
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
        original_id = selected.tc.original_id;
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
  let ret = [
      {
        remove_constraints = [SC_Typeclass selected.tc];
        add_constraints = cleaned :: deduced;
        proof_trace = Axiom (HandWaved "cut with the following (cleaned => removed_typeclass) to show that the removal does not lose info, (removed_typeclass => selected.c => cleaned) to show that the cleaned vesion does not introduce unwanted constraints.")
      }
    ] in
  ok ret

(* ***********************************************************************
 * Heuristic
 * *********************************************************************** *)
    
let printer ppd (t : selector_output) =
  let open Format in
  let open Type_variable_abstraction.PP in
  let lst = t.tc in
  let a = t.c in fprintf ppd "%a and %a" c_typeclass_simpl_short lst constructor_or_row_short a

let pp_deduce_and_clean_result ppf {deduced;cleaned} =
  let open Format in
  let open Type_variable_abstraction.PP in
  fprintf ppf "{@[<hv 2>@
              deduced : %a;@
              cleaned : %a;@
              @]}"
    (PP_helpers.list_sep_d c_constructor_simpl) deduced
    c_typeclass_simpl cleaned

let printer_json (t : selector_output) =
  let open Type_variable_abstraction.Yojson in
  let lst = t.tc in
  let a = t.c in 
  `Assoc [
    ("tc",c_typeclass_simpl lst)
    ;("a",constructor_or_row a)]
let comparator { tc=a1; c=a2 } { tc=b1; c=b2 } =
  let open Type_variable_abstraction.Compare in
  c_typeclass_simpl a1 b1 <? fun () -> constructor_or_row a2 b2
end

module MM = M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)



open Ast_typed.Types
open Solver_types

module Compat = struct
  module All_plugins = Database_plugins.All_plugins.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
  open All_plugins
  let heuristic_name = MM.heuristic_name
  let selector repr c flds =
    let module Flds = struct
      let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
      let assignments : type_variable Assignments.t = flds#assignments
      let typeclasses_constraining : type_variable Typeclasses_constraining.t = flds#typeclasses_constraining
    end
    in
    MM.selector repr c (module Flds)
  let alias_selector a b flds =
    let module Flds = struct
      let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
      let assignments : type_variable Assignments.t = flds#assignments
      let typeclasses_constraining : type_variable Typeclasses_constraining.t = flds#typeclasses_constraining
    end
    in
    MM.alias_selector a b (module Flds)
  let get_referenced_constraints = MM.get_referenced_constraints
  let propagator = MM.propagator
  let printer = MM.printer
  let printer_json = MM.printer_json
  let comparator = MM.comparator
end
let heuristic = Heuristic_plugin Compat.{ heuristic_name; selector; alias_selector; get_referenced_constraints; propagator; printer; printer_json; comparator }

type nonrec deduce_and_clean_result = MM.deduce_and_clean_result = {
    deduced : c_constructor_simpl list ;
    cleaned : c_typeclass_simpl ;
  }
let restrict = MM.restrict
let deduce_and_clean = MM.deduce_and_clean
let pp_deduce_and_clean_result = MM.pp_deduce_and_clean_result






(*
                          ========NOTES=======

  (* typeclass size(a) = ∃ x, size(x) =>
                         a ∈ [ (int);
                               (pair(t,t));
                               (list(x)) ] (* une typeclass *) *)
  let x = fresh x
  let tc_sizearg3 a   =
                       tc "arguments for size"       [x;y] (* bound local vars *)
                                                     [ c_typeclass_simpl([x], tc_comparable) ;
                                                       c_typeclass_simpl([y], tc_length) ] (* new tcs *)
                                                     [a] (* ∈ *)
                                                     [ [int] ;       (* possibilities *)
                                                       [ pair[t,t] ];
                                                       [ map[x;y] ] ;
                                                       [ map[x;float] ] ;
                                                       [ map[float;x] ] ;
                                                       [ map[unit;unit] ] ;
                                                       [ P_variable bool ] ;
						       [ y ]
                                                     ]

                         tc_comp = a ∈ [ [unit];[cmp2];[cmp3] ]
                         tc_len = a ∈ [ [map(int,int)];[len2] ]
                         a = map[z;w]
                         // z = autre
                         // w = chose

selector:
  find in db αᵢ = κ(β…)  and ∃δ…, c… => (…,αᵢ,…) ∈ [ (…,τᵢⱼ,…) … ]
    if τᵢⱼ != (P_variable unbound)
  find in db αᵢ = Ξ(ℓ:β…) and ∃δ…, c… => (…,αᵢ,…) ∈ [ (τ…) … ]
    if τᵢⱼ != (P_variable unbound)

propagator:
  filter col (* = ᵢ *):
    List.filter (fltr col) (get_lines matrix)
  deduce:
    List.map deduce1 (get_columns matrix)

deduce1 column:
  if all_equal_root:
    return v = eq

fltr col line:
  match line[col] with:
    P_abs -> …
  | P_forall -> unsupported
  | P_constant/ctor -> true if same k + len(args)
  | P_row/row -> true if same r + keys
  | P_variable (bound) -> filter recursively in the c…
  | P_variable (unbound)
     -> do not touch a column which contains a var (wait for inlining)
  | P_apply -> unsupported



selector:
  find in db γᵢ = κ(β…)  and ∃δ…, c… => (…,αᵢ,…) ∈ [ (…,P_variable γᵢ,…) … ]
  find in db γᵢ = Ξ(ℓ:β…) and ∃δ…, c… => (…,αᵢ,…) ∈ [ (…,P_variable γᵢ,…) … ]

inline_var:
when a var which appears at the root of a column is found by the selector; inline it



inline:
if there is only one line, and it contains only variables used in a typeclass, then inline it



test:
	 
  def get_allowed(tc_constraint):
    return enumeration of all allowed types a which satisfy tc_constraint(args), i.e a matrix of type_value with len(args) columns and N rows

  def test_main():
    initial_args = a
    initial_typeclass = size(a)
    allowed = get_allowed(initial_typeclass)
    for each type in allowed:
      genealogies([], [], (a, initial_typeclass), [(a, type)])

  # Order of traversal of the tree where parents are always produced before children
  # (i.e. realistic orders of birth in a genealogy tree where the dates are missing and time-travel is not permitted):
  def genalogies(order, all_deduced, (args, constraint), candidates, get_var_from_path):
    if len(nodes) == 0:
      check_end(cleaned, order)
    else:
      for candidate in candidates:
	ctor_or_row = mk_constraint(candidate) # α = κ(βᵢ…)

        # test stuff
        deduced, cleaned = deduce_and_clean(ctor_or_row, constraint, get_var_from_path)
	new_deduced = new_deduced + deduced
        check(constraint, new_deduced, cleaned, filter, order, candidate)
        constraint = cleaned

        # tree stuff
	new_candidates = Set.add_list (Set.remove candidate candidates) children
        new_order = order + [candidate]
        genealogies(new_order, new_deduced, ctor_or_row, new_candidates)

  def check(constraint, all_deduced, cleaned, filter, order_before_candidate, candidate):
    if allowed(constraint) != List.filter(mk_filter(partial_assignment), allowed):
      print "Error: cleaned constraint is not as simplified as it should be or it is incorrect"

  def mk_filter(all_deduced, ):

  def check_end():
    # sanity check (already enforced by check())
    # the tc should be empty (fully inferred) at the end
    if len(cleaned.bound) == 0 and len(cleaned.tcs) == 0 and len(cleaned.args) == 0 and len(cleaned.possibilities) == 0:
      print "okay " + order
    else:
      print "error " + order

Test: list of type values
     1 simplify
         |
       2 map
     /       \
3 list    5 map
    |      /   \
 4 int  6 str  7 list
                 |
               8 int

map(x, map(string, x))
map(x, map(float,  x))
map(bool, bool)
x ∈ (list(int), bool)

extension(cleaned) == filter(extension(initial_typeclass), sent_info=map(list(_),_))
already_deduced + deduced ≥ sent_info
extension(cleaned) == filter(extension(initial_typeclass), sent_info=to_partial_type(already_deduced + deduced))
ideally: (already_deduced + deduced) is maximal

Propagator args: (c_constructor_simpl or c_row_simpl or ())                               Propagator deduced
                 + c_typeclass_simpl([a], …)

               1 ()                                                                        1 a=map(l,m)
						       							    
           2 a=map(b,c)				                                               2 []		    
            					       							    
 3 b=list(d)       5 c=map(e,f)			             3 [l=list(n),n=int,m=map(o,l)]                         5 []	    
           					       		   					    
  4 d=int        6 e=str  7 f=list(g)		                     4 []                                 6[o=str]       7[]
                				       							    
                        8 g=int			         	                                                 8[]

Deduced DAG

           map
	/     \
	|    map
	|    :  \
	|  (str)|
	|      /
	 \    /
          list
           |
          int






     

                                                     [ [ x;     y ] ;
                                                       [ x;     float ] ;
                                                       [ float; x ] ;
                                                       [ unit;  unit ] ;
						       [ int,   int] (* from y *)
                                                     ]






  ~> c_typeclass_simpl([x], tc_comparable)  (* x ∈ tc_comparable *)
                       tc "arguments for size"       [x;y] (* bound local vars *)
                                                     [ c_typeclass_simpl([y], tc_length)
                                                        ] (* new tcs *)
                                                     [a] (* ∈ *)
                                                     [
						       map[cmp1; map(int,int)]
						       map[cmp1; len2]
						       map[cmp2; map(int,int)]
						       map[cmp2; len2]
						       map[cmp3; map(int,int)]
						       map[cmp3; len2]
						       map[cmp1; float]
						       map[cmp2; float]
						       map[cmp3; float]
						       map[unit;unit]
						       map(int,int);
						       len2;
                                                     ]

  
                       tc "arguments for size"       […union of tvars from tc_co tc_len…] (* bound local vars *)
                                                     […union ……] (* new tcs *)
                                                     [b fresh;c fresh] (* ∈ *)
                                                     [ [ x ; y ] ;
                                                       [ unit; unit ] ;
                                                       [ y ]
                                                     ]

======== END NOTES =========

*)  

