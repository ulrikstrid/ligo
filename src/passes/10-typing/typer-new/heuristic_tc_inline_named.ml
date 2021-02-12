(* selector / propagation rule for inlining type variables which
   refer to other types.

   Given a typeclass
     (α₁, α₂, …) ∈ ∃δ…, c… => { (τ₁₁, τ₁₂, …) , … }
   where there are one or more τᵢⱼ = P_variable γ, and a second
   second hypothesis of the form
     γᵢ = κ(β₁, β₂, …)
      or
     γᵢ = Ξ(ℓᵢ : βᵢ, …)
   It inlines the definition of the type γᵢ.

   This rule can deduce a new assignment for other variables
   constrained by the typeclass if every possible type for that
   variable uses the same type constructor or row; it will then inline
   the arguments of the constructor or row, and continue inferring
   until the typeclass is in a minimal form. *)

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
    val typeclasses_using_as_unbound_var : Type_variable.t Typeclasses_using_as_unbound_var.t
    val by_constraint_identifier : Type_variable.t By_constraint_identifier.t
  end
end

module M = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  (* open Type_variable_abstraction *)
  open Type_variable_abstraction.Types
  (* open Type_variable_abstraction.Reasons *)

  module Utils = Heuristic_tc_utils.Utils(Type_variable)(Type_variable_abstraction)
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
 *   find in db γᵢ = κ(β…)  and (…,αᵢ,…) ∈ ∃δ…, c… => [ (…,P_variable γᵢ,…) … ]
 *   find in db γᵢ = Ξ(ℓ↦β…) and (…,αᵢ,…) ∈ ∃δ…, c… => [ (…,P_variable γᵢ,…) … ] *)
  
(* Find typeclass constraints in the dbs which constrain c_or_r.tv *)
let selector_by_variable : (type_variable -> type_variable) -> flds -> constructor_or_row -> type_variable -> selector_output list =
  fun repr (module Indexes) c_or_r tv ->
  let typeclasses = (Typeclasses_constraining.get_typeclasses_constraining_list (repr tv) Indexes.typeclasses_constraining) in
  List.map (fun tc -> { tc ; c = c_or_r }) typeclasses

(* Find constructor constraints γᵢ = κ(β …) and and row constraints
   γᵢ = Ξ(ℓ↦β …) where γᵢ is one of the variables occurring as a root
   of one of the cells of the matrix of the typeclass. *)
let selector_by_tc : (type_variable -> type_variable) -> flds -> c_typeclass_simpl -> selector_output list =
  fun repr (module Indexes) tc ->
  let aux (tval : type_value) =
    match tval.wrap_content with
      P_variable tv ->
      (* Since we are only refining the typeclass one type expression
         node at a time, we only need the top-level assignment for
         that variable, e.g. α = κ(βᵢ, …). We can therefore look
         directly in the assignments. *)
      (match Assignments.find_opt (repr tv) Indexes.assignments with
       | Some cr -> [{ tc ; c = cr }]
       | None   -> [])
    | _ -> [] in
  List.flatten @@ List.map aux (get_cells tc)

let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  fun repr type_constraint_simpl indexes ->
  match type_constraint_simpl with
    SC_Constructor c  -> selector_by_variable repr indexes (`Constructor c) c.tv
  | SC_Row r          -> selector_by_variable repr indexes (`Row         r) r.tv
  | SC_Alias        _  -> [] (* TODO: this case should go away since aliases are handled by the solver structure *)
  | SC_Poly         _  -> []
  | SC_Access_label _  -> []
  | SC_Typeclass   tc -> selector_by_tc repr indexes tc

(* When (αᵢ, …) ∈ { (τ, …) , … } and β = κ(δ …) are in the db,
   aliasing α and β should check if they are non-empty, and in that
   case produce a selector_output for all pairs. This will involve a
   lookup to see if α is constrained by a typeclass
   (typeclasses_constraining indexer). Add to this the logic for
   refined_typeclass vs. typeclass. *)

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun _a _b (module Indexes) ->
  []                            (* TODO *)
  (* let a_tcs = (Typeclasses_constraining.get_typeclasses_constraining_list a Indexes.typeclasses_constraining) in
   * let b_tcs = (Typeclasses_constraining.get_typeclasses_constraining_list b Indexes.typeclasses_constraining) in
   * let a_lhs_constructors = Grouped_by_variable.get_constructors_by_lhs a Indexes.grouped_by_variable in
   * let b_lhs_constructors = Grouped_by_variable.get_constructors_by_lhs b Indexes.grouped_by_variable in
   * let a_lhs_rows = Grouped_by_variable.get_rows_by_lhs a Indexes.grouped_by_variable in
   * let b_lhs_rows = Grouped_by_variable.get_rows_by_lhs b Indexes.grouped_by_variable in
   * let a_ctors = MultiSet.map_elements (fun a -> `Constructor a) a_lhs_constructors in
   * let a_rows  = MultiSet.map_elements (fun a -> `Row a        ) a_lhs_rows         in
   * let b_ctors = MultiSet.map_elements (fun a -> `Constructor a) b_lhs_constructors in
   * let b_rows  = MultiSet.map_elements (fun a -> `Row a        ) b_lhs_rows         in
   * List.flatten @@
   * List.map
   *   (fun tc ->
   *      List.map
   *        (fun c ->
   *           { tc ; c })
   *        (a_ctors @ b_ctors @ a_rows @ b_rows ))
   *   (a_tcs @ b_tcs) *)

let get_referenced_constraints ({ tc; c } : selector_output) : type_constraint_simpl list =
  [
    SC_Typeclass tc;
    (match c with `Constructor c -> SC_Constructor c | `Row r -> SC_Row r);
  ]

(* ***********************************************************************
 * Propagator
 * *********************************************************************** *)

let propagator : (selector_output, typer_error) Type_variable_abstraction.Solver_types.propagator =
  fun _selected _repr ->
  ok []                         (* TODO *)
  (* (\* The selector is expected to provide constraints with the shape (α
   *    = κ(β, …)) and to update the private storage to keep track of the
   *    refined typeclass *\)
   * let () = Format.printf "and tv: %a and repr tv :%a \n%!" (PP_helpers.list_sep_d PP.type_variable) selected.tc.args (PP_helpers.list_sep_d PP.type_variable) @@ List.map repr selected.tc.args in
   * let restricted = restrict repr selected.c selected.tc in
   * let () = Format.printf "restricted: %a\n!" PP.c_typeclass_simpl_short restricted in
   * let%bind {deduced ; cleaned} = deduce_and_clean repr restricted in
   * (\* TODO: this is because we cannot return a simplified constraint,
   *    and instead need to retun a constraint as it would appear if it
   *    came from the module (generated by the ill-named module
   *    "Wrap"). type_constraint_simpl is more or less a subset of
   *    type_constraint, but some parts have been shuffled
   *    around. Hopefully this can be sorted out so that we don't need a
   *    dummy value for the srcloc and maybe even so that we don't need a
   *    conversion (one may dream). *\)
   * let tc_args = List.map (fun x -> wrap (Todo "no idea") @@ P_variable (repr x)) cleaned.args in
   * let cleaned : type_constraint = {
   *     reason = cleaned.reason_typeclass_simpl;
   *     c = C_typeclass {
   *         tc_bound = [](\*TODO*\); tc_constraints = [](\*TODO*\);
   *       tc_args ;
   *       typeclass = cleaned.tc;
   *       original_id = selected.tc.original_id;
   *     }
   *   }
   * in
   * let aux (x : c_constructor_simpl) : type_constraint = {
   *   reason = "inferred: only possible type for that variable in the typeclass";
   *   c = C_equation {
   *     aval = wrap (Todo "?") @@ P_variable (repr x.tv) ;
   *     bval = wrap (Todo "? generated") @@
   *             P_constant {
   *               p_ctor_tag  = x.c_tag ;
   *               p_ctor_args = List.map
   *                 (fun v -> wrap (Todo "? probably generated") @@ P_variable (repr v))
   *                 x.tv_list ;
   *             }
   *     }
   *   }
   * in
   * let deduced : type_constraint list = List.map aux deduced in
   * let ret = [
   *     {
   *       remove_constraints = [SC_Typeclass selected.tc];
   *       add_constraints = cleaned :: deduced;
   *       proof_trace = Axiom (HandWaved "cut with the following (cleaned => removed_typeclass) to show that the removal does not lose info, (removed_typeclass => selected.c => cleaned) to show that the cleaned vesion does not introduce unwanted constraints.")
   *     }
   *   ] in
   * ok ret *)

(* ***********************************************************************
 * Heuristic
 * *********************************************************************** *)
    
let printer ppd (t : selector_output) =
  let open Format in
  let open Type_variable_abstraction.PP in
  let lst = t.tc in
  let a = t.c in fprintf ppd "%a and %a" c_typeclass_simpl_short lst constructor_or_row_short a

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
      let by_constraint_identifier : type_variable By_constraint_identifier.t = flds#by_constraint_identifier
      let typeclasses_using_as_unbound_var : type_variable Typeclasses_using_as_unbound_var.t = flds#typeclasses_using_as_unbound_var
    end
    in
    MM.selector repr c (module Flds)
  let alias_selector a b flds =
    let module Flds = struct
      let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
      let assignments : type_variable Assignments.t = flds#assignments
      let typeclasses_constraining : type_variable Typeclasses_constraining.t = flds#typeclasses_constraining
      let by_constraint_identifier : type_variable By_constraint_identifier.t = flds#by_constraint_identifier
      let typeclasses_using_as_unbound_var : type_variable Typeclasses_using_as_unbound_var.t = flds#typeclasses_using_as_unbound_var
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
