(* selector / propagation rule for specializing polymorphic types
 * For now: (x = forall y, z) and (x = k'(var' …))
 * produces the new constraint (x = z[y ↦ fresh_y])
 * where [from ↦ to] denotes substitution. *)

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_Variable_Abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module type S = sig
    open Type_Variable_Abstraction.Types
    module Grouped_by_variable : sig
      type _ t
      val get_constructors_by_lhs : type_variable -> type_variable t -> c_constructor_simpl MultiSet.t [@@warning "-32"]
      val get_rows_by_lhs : type_variable -> type_variable t -> c_row_simpl MultiSet.t [@@warning "-32"]
      val get_polys_by_lhs : type_variable -> type_variable t -> c_poly_simpl MultiSet.t [@@warning "-32"]
      val get_access_labels_by_result_type : type_variable -> type_variable t -> c_access_label_simpl MultiSet.t [@@warning "-32"]
      val get_access_labels_by_record_type : type_variable -> type_variable t -> c_access_label_simpl MultiSet.t [@@warning "-32"]
    end
    val grouped_by_variable : Type_variable.t Grouped_by_variable.t
  end
end


module Core = Typesystem.Core
open Solver_types
open Trace
open Typer_common.Errors
open Ast_typed.Reasons



(* TODO: we need to detect if a ∀ constraint has already been specialized or not
   The same need was present for the heuristic_tc_fundep (detect if a TC has already
   been refined, and if so find the update) *)
 
module M = functor (Type_variable : sig type t end) (Type_Variable_Abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_Variable_Abstraction
  open Type_Variable_Abstraction.Types
  type type_variable = Type_variable.t


  type flds = (module INDEXES(Type_variable)(Type_Variable_Abstraction).S)

  type selector_output = {
      poly : c_poly_simpl ;
      a_k_var : c_constructor_simpl ;
    }
  let heuristic_name = "specialize1"

 let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  (* find two rules with the shape (x = forall b, d) and x = k'(var' …) or vice versa *)
  (* TODO: do the same for two rules with the shape (a = forall b, d) and tc(a…) *)
  (* TODO: do the appropriate thing for two rules with the shape (a = forall b, d) and (a = forall b', d') *)
  fun repr type_constraint_simpl ((module Indexes) : flds)->
  match type_constraint_simpl with
  | SC_Constructor c                ->
    (* vice versa *)
    let other_cs = MultiSet.elements @@ Indexes.Grouped_by_variable.get_polys_by_lhs (repr c.tv) Indexes.grouped_by_variable in
    let cs_pairs = List.map (fun x -> { poly = x ; a_k_var = c }) other_cs in
    cs_pairs
  | SC_Alias       _                -> failwith "alias should not be visible here"
  | SC_Poly        p                ->
    let other_cs = MultiSet.elements @@ Indexes.Grouped_by_variable.get_constructors_by_lhs (repr p.tv) Indexes.grouped_by_variable in
    let cs_pairs = List.map (fun x -> { poly = p ; a_k_var = x }) other_cs in
    cs_pairs
  | SC_Typeclass   _                -> []
  | SC_Access_label _               -> []
  | SC_Row _                        -> []

(* when α = ∀ δ, γ and β = κ(ε, …) are in the db, aliasing α and β
   should check they are non-empty (and in that case produce a
   selector_output for all pairs) *)

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  let a_polys = MultiSet.elements @@ Indexes.Grouped_by_variable.get_polys_by_lhs a Indexes.grouped_by_variable in
  let a_ctors = MultiSet.elements @@ Indexes.Grouped_by_variable.get_constructors_by_lhs a Indexes.grouped_by_variable in
  let b_polys = MultiSet.elements @@ Indexes.Grouped_by_variable.get_polys_by_lhs b Indexes.grouped_by_variable in
  let b_ctors = MultiSet.elements @@ Indexes.Grouped_by_variable.get_constructors_by_lhs b Indexes.grouped_by_variable in
  List.flatten @@
  List.map
    (fun poly ->
       List.map
         (fun ctor ->
            { poly ; a_k_var = ctor })
         (a_ctors @ b_ctors))
    (a_polys @ b_polys)

let get_referenced_constraints ({ poly; a_k_var } : selector_output) : type_constraint_simpl list =
  [
    SC_Poly poly;
    SC_Constructor a_k_var;
  ]

let propagator : (selector_output , typer_error) Type_Variable_Abstraction.Solver_types.propagator =
  fun selected repr ->
  let a = selected.poly in
  let b = selected.a_k_var in

  (* The selector is expected to provide two constraints with the shape (x = forall y, z) and x = k'(var' …) *)
  assert (Type_Variable_Abstraction.Var.equal (repr (a : c_poly_simpl).tv) (repr (b : c_constructor_simpl).tv));

  (* produce constraints: *)

  (* create a fresh existential variable to instantiate the polymorphic type y *)
  let fresh_existential = Core.fresh_type_variable () in
  (* Produce the constraint (b.tv = a.body[a.binder |-> fresh_existential])
     The substitution is obtained by immediately applying the forall. *)
  (* TODO: this should not use apply, universally-quantified types are *not* type-level functions, even though the substitution is identical on both. *)
  let apply =
    wrap Propagator_specialize_apply @@
      P_apply { tf   = wrap Propagator_specialize_tf @@ P_forall a.forall ;
                targ = wrap Propagator_specialize_targ @@ P_variable fresh_existential ;
      }
  in
  let (reduced, new_constraints) = Typelang.check_applied @@ Typelang.type_level_eval apply in
  
  let eq1 = Misc.c_equation (wrap (Todo "solver: propagator: specialize1 eq1") @@ P_variable (repr b.tv)) reduced "propagator: specialize1" in
  let eqs = eq1 :: new_constraints in
    ok [
        {
          remove_constraints = [ SC_Poly a ];
          add_constraints = eqs;
          proof_trace = Axiom Axioms.specialize
        }
      ]

let printer ppf ({poly;a_k_var}) =
  let open Format in
  let open Type_Variable_Abstraction.PP in
  fprintf ppf "%a = %a"
    c_poly_simpl_short poly
    c_constructor_simpl_short a_k_var
let printer_json ({poly;a_k_var}) =
  let open Type_Variable_Abstraction.Yojson in
  `Assoc [
    ("poly",    c_poly_simpl poly);
    ("a_k_var", c_constructor_simpl a_k_var)]
let comparator { poly = a1; a_k_var = a2 } { poly = b1; a_k_var = b2 } =
  let open Type_Variable_Abstraction.Compare in
  (* Possible bug : change for Solver_should_be_generated *)
  c_poly_simpl a1 b1 <? fun () -> c_constructor_simpl a2 b2
end

module Type_variable = struct type t = Ast_typed.Types.type_variable end
module MM = M(Type_variable)(Type_variable_instance.Opaque_type_variable)

open Ast_typed.Types
open Solver_types

module Compat = struct
  open Database_plugins.All_plugins
  let heuristic_name = MM.heuristic_name
  let selector repr c (flds : < grouped_by_variable : type_variable GroupedByVariable.t ; .. >) =
    let module Flds = struct
      module Grouped_by_variable = GroupedByVariable
      let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
    end
    in
    MM.selector repr c (module Flds)
  let alias_selector a b (flds : < grouped_by_variable : type_variable GroupedByVariable.t ; .. >) =
    let module Flds = struct
      module Grouped_by_variable = GroupedByVariable
      let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
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
type nonrec selector_output = MM.selector_output = {
      poly : c_poly_simpl ;
      a_k_var : c_constructor_simpl ;
    }
let selector = Compat.selector

