(* selector / propagation rule for breaking down composite types
 * For now: break pair(a, b) = pair(c, d) into a = c, b = d *)


module BLAH = functor (Type_variable : sig type t end) -> struct
  module type S = sig
    module Types : sig
      type type_variable = Type_variable.t
      type p_constraints
      type constraint_identifier
      type constant_tag
      type row_tag
      type label
      type typeclass
      module LMap : Map.S with type key = label
      type 'a label_map = 'a LMap.t
      type type_value_ =
        | P_forall       of p_forall
        | P_variable     of type_variable
        | P_constant     of p_constant
        | P_apply        of p_apply
        | P_row          of p_row
      and p_forall = {
        binder      : type_variable ;
        constraints : p_constraints ;
        body        : type_value ;
      }
      and p_constant = {
        p_ctor_tag : constant_tag ;
        p_ctor_args : p_ctor_args ;
      }
      and p_apply = {
        tf : type_value ;
        targ : type_value ;
      }
      and p_row = {
        p_row_tag  : row_tag ;
        p_row_args : row_lmap ;
      }
      and type_value = type_value_ Stage_common.Types.location_wrap
      and p_ctor_args = type_value list

      and row_value = {
        associated_value : type_value;
        michelson_annotation : string option;
        decl_pos : int;
      }

      and row_lmap = row_value label_map

      and c_alias = {
        reason_alias_simpl : string ;
        a : type_variable ;
        b : type_variable ;
      }

      and c_constructor_simpl = {
        reason_constr_simpl : string ;
        id_constructor_simpl : constraint_identifier ;
        original_id  : constraint_identifier option ;
        tv : type_variable;
        c_tag : constant_tag;
        tv_list : type_variable list;
      }

      and c_row_simpl = {
        reason_row_simpl : string ;
        id_row_simpl : constraint_identifier ;
        original_id  : constraint_identifier option ;
        tv : type_variable;
        r_tag : row_tag;
        tv_map : row_variable label_map;
      }

      and row_variable = {
        associated_variable : type_variable;
        michelson_annotation : string option;
        decl_pos : int;
      }

      and c_typeclass_simpl = {
        reason_typeclass_simpl : string ;
        id_typeclass_simpl : constraint_identifier ;
        original_id        : constraint_identifier option ;
        tc   : typeclass          ;
        args : type_variable list ;
      }
      and c_access_label_simpl = {
        reason_access_label_simpl : string ;
        id_access_label_simpl : constraint_identifier ;
        record_type : type_variable ;
        label : label ;
        tv : type_variable ;
      }
      and c_poly_simpl = {
        reason_poly_simpl : string ;
        id_poly_simpl : constraint_identifier ;
        original_id   : constraint_identifier option ;
        tv     : type_variable ;
        forall : p_forall ;
      }
      and type_constraint_simpl =
        | SC_Constructor  of c_constructor_simpl
        | SC_Alias        of c_alias
        | SC_Poly         of c_poly_simpl
        | SC_Typeclass    of c_typeclass_simpl
        | SC_Access_label of c_access_label_simpl
        | SC_Row          of c_row_simpl

      and constructor_or_row = [
        | `Constructor of c_constructor_simpl
        | `Row of c_row_simpl
      ]

      and type_constraint_ =
        | C_equation of c_equation
        | C_typeclass of c_typeclass
        | C_access_label of c_access_label

      and c_equation = {
        aval : type_value ;
        bval : type_value ;
      }

      and tc_args = type_value list

      and c_typeclass = {
        tc_args : tc_args ;
        original_id : constraint_identifier option ;
        typeclass : typeclass ;
      }

      and c_access_label = {
        c_access_label_tval : type_value ;
        accessor : label ;
        c_access_label_tvar : type_variable ;
      }

      type type_constraint = {
        reason : string ;
        c : type_constraint_ ;
      }

      type axiom = HandWaved of string

      type proof_trace =
        | Axiom of axiom

      type update = {
        remove_constraints : type_constraint_simpl list ;
        add_constraints : type_constraint list ;
        proof_trace : proof_trace ;
      }
      type updates = update list
    end

    module Compare : sig
      open Types
      type 'a comparator = 'a -> 'a -> int
      val (<?) : int -> (unit -> int) -> int
      val c_constructor_simpl : c_constructor_simpl comparator
      val c_row_simpl : c_row_simpl comparator
      val label : label comparator
      module Solver_should_be_generated : sig
        (* This module can probably be merged with Compare just above,
           leaving as-is for now to avoid risking changing the semantics
           and breaking tests *)
        type 'a comparator = 'a -> 'a -> int
        val (<?) : int -> (unit -> int) -> int
        val compare_constructor_or_row : constructor_or_row comparator
        val compare_simple_c_row : row_tag comparator
        val compare_simple_c_constant : constant_tag comparator
      end
    end

    module PP : sig
      open Types
      type 'a pretty_printer = Format.formatter -> 'a -> unit
      val c_constructor_simpl : c_constructor_simpl pretty_printer
      val c_row_simpl : c_row_simpl pretty_printer
      val constructor_or_row_short : constructor_or_row pretty_printer

      module Solver_should_be_generated : sig
        (* This module can probably be merged with the rest of PP,
           leaving as-is for now to avoid risking changing the semantics
           and breaking tests *)
        type 'a pretty_printer = Format.formatter -> 'a -> unit
        val debug_pp_c_row_simpl : c_row_simpl pretty_printer
        val debug_pp_c_constructor_simpl : c_constructor_simpl pretty_printer
      end
    end

    module Yojson : sig
      open Types
      type 'a json_printer = 'a -> Yojson.Safe.t
      val constructor_or_row : constructor_or_row json_printer
    end

    module Var : sig
      open Types
      val equal : type_variable -> type_variable -> bool
    end

    module Solver_types : sig
      open Types
      type ('selector_output , 'errors) propagator = 'selector_output -> (type_variable -> type_variable) -> (updates, 'errors) Trace.result
    end

    module Misc : sig
      open Types
      val c_equation : type_value -> type_value -> string -> type_constraint
    end

    module Reasons : sig
      type t =
        | Forall
        | Forall_TC
        | Builtin_type
        | Propagator_break_ctor of string
        | Propagator_specialize_apply
        | Propagator_specialize_tf
        | Propagator_specialize_targ
        | Propagator_specialize_eq
        | Todo of string

      val pp : Format.formatter -> t -> unit
      val wrap : t -> 'v -> 'v Location.wrap
    end

    module Axioms : sig
      open Types
      val f_equal : axiom
      val specialize : axiom
    end
  end
end

module INDEXES = functor (Type_variable : sig type t end) (Blah : BLAH(Type_variable).S) -> struct
  module type S = sig
    open Blah.Types
    module Grouped_by_variable : sig
      type _ t
      [@@warning "-32"]
      val get_constructors_by_lhs : type_variable -> type_variable t -> c_constructor_simpl MultiSet.t
      [@@warning "-32"]
      val get_rows_by_lhs : type_variable -> type_variable t -> c_row_simpl MultiSet.t
      [@@warning "-32"]
      val get_polys_by_lhs : type_variable -> type_variable t -> c_poly_simpl MultiSet.t
      [@@warning "-32"]
      val get_access_labels_by_result_type : type_variable -> type_variable t -> c_access_label_simpl MultiSet.t
      [@@warning "-32"]
      val get_access_labels_by_record_type : type_variable -> type_variable t -> c_access_label_simpl MultiSet.t
      [@@warning "-32"]
    end
    val grouped_by_variable : Type_variable.t Grouped_by_variable.t
  end
end




(* open Typesystem.Solver_types *)
open Trace
open Typer_common.Errors
(* open Database_plugins.All_plugins *)

(* type ('type_variable, 'a) flds = <
 *   grouped_by_variable : 'type_variable GroupedByVariable.t ;
 *   ..
 * > as 'a *)

module M = functor (Type_variable : sig type t end) (Blah : BLAH(Type_variable).S) -> struct
  open Blah.Types
  open Blah.Misc
  open Blah.Reasons
  type type_variable = Type_variable.t

  type selector_output = {
    a_k_var : constructor_or_row ;
    a_k'_var' : constructor_or_row ;
  }

  type flds = (module INDEXES(Type_variable)(Blah).S)

  let heuristic_name = "break_ctor"

let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  (* find two rules with the shape x = k(var …) and x = k'(var' …) *)
  fun (repr : (type_variable -> type_variable)) type_constraint_simpl ((module Indexes) : flds) ->
    match type_constraint_simpl with
    | SC_Constructor c -> (
      (* Format.printf "In break_ctor.selector_ for %a\n%!" Blah.PP.type_constraint_simpl_short type_constraint_simpl;*)
      (* finding other constraints related to the same type variable and
      with the same sort of constraint (constructor vs. constructor)
      is symmetric *)
      let other_rows_lhs = Indexes.Grouped_by_variable.get_rows_by_lhs (repr c.tv) Indexes.grouped_by_variable in
      let tmp = Indexes.Grouped_by_variable.get_constructors_by_lhs (repr c.tv) Indexes.grouped_by_variable in
      let other_constructors_lhs = 
        List.filter (fun x -> not @@  (Blah.Compare.c_constructor_simpl c x = 0)) @@ MultiSet.elements @@
        tmp in
      (* Format.printf "Other constructor : (%a)\n%!" Blah.PP.(list_sep_d c_constructor_simpl_short) other_constructors_lhs; *)
      let () = ( if MultiSet.is_empty other_rows_lhs
                 then ()
                 else failwith (Format.asprintf "TODO: type error with %a ; %a" Blah.PP.c_constructor_simpl c (MultiSet.pp Blah.PP.c_row_simpl) other_rows_lhs))
      in    
      let cs_pairs = List.map (fun x -> { a_k_var = `Constructor c ; a_k'_var' = `Constructor x }) other_constructors_lhs in
      cs_pairs
    )
    | SC_Alias       _                -> []
    | SC_Typeclass   _                -> []
    | SC_Access_label _               -> []
    | SC_Poly        _                -> []
    | SC_Row         r                -> (
      (* Format.printf "In break_ctor.selector_ for %a\n%!" Blah.PP.type_constraint_simpl_short type_constraint_simpl; *)
      let other_rows_lhs = 
        List.filter (fun x -> not @@  (Blah.Compare.c_row_simpl r x = 0)) @@ MultiSet.elements @@
        Indexes.Grouped_by_variable.get_rows_by_lhs (repr r.tv) Indexes.grouped_by_variable in
      let constructors_lhs = Indexes.Grouped_by_variable.get_constructors_by_lhs (repr r.tv) Indexes.grouped_by_variable in
      let () = ( if MultiSet.is_empty constructors_lhs
                 then ()
                 else failwith (Format.asprintf "TODO: type error with %a ; %a" Blah.PP.c_row_simpl r (MultiSet.pp Blah.PP.c_constructor_simpl) constructors_lhs)) in
      let cs_pairs = List.map (fun x -> { a_k_var = `Row r ; a_k'_var' = `Row x }) other_rows_lhs in
      cs_pairs
    )

(* when a = k(…) and b = k'(…) are in the db, aliasing a and b should
   check if they're non-empty (and in that case produce a
   selector_output for all pairs / more efficiently any single pair
   since the break_ctor creates equivalence classes for the
   constructor arguments) *)

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  (* Format.printf "Break_ctor.alias_selector %a %a\n%!" Blah.PP.type_variable a Blah.PP.type_variable b ; *)
  let a_constructors = Indexes.Grouped_by_variable.get_constructors_by_lhs a Indexes.grouped_by_variable in
  let b_constructors = Indexes.Grouped_by_variable.get_constructors_by_lhs b Indexes.grouped_by_variable in
  let a_rows = Indexes.Grouped_by_variable.get_rows_by_lhs a Indexes.grouped_by_variable in
  let b_rows = Indexes.Grouped_by_variable.get_rows_by_lhs b Indexes.grouped_by_variable in
  let a_ctor = MultiSet.map_elements (fun a -> `Constructor a) a_constructors in
  let b_ctor = MultiSet.map_elements (fun a -> `Constructor a) b_constructors in
  let a_row = List.map (fun a -> `Row a) (MultiSet.elements a_rows) in
  let b_row = List.map (fun a -> `Row a) (MultiSet.elements b_rows) in
  match a_ctor @ a_row with
  | [] -> []
  | old_ctors_hd :: _ ->
    (match b_ctor @ b_row with
       [] -> []
     | new_ctors_hd :: _ ->
       [{ a_k_var = old_ctors_hd ; a_k'_var' = new_ctors_hd }])

let get_referenced_constraints ({ a_k_var; a_k'_var' } : selector_output) : type_constraint_simpl list =
  [
    (match a_k_var with `Constructor c -> SC_Constructor c | `Row r -> SC_Row r);
    (match a_k'_var' with `Constructor c -> SC_Constructor c | `Row r -> SC_Row r);
  ]

let printer ppf {a_k_var;a_k'_var'} =
  let open Format in
  let open Blah.PP in
  fprintf ppf "%a = %a"
    constructor_or_row_short a_k_var
    constructor_or_row_short a_k'_var'
let printer_json ({a_k_var;a_k'_var'}) =
  let open Blah.Yojson in
  `Assoc [
    ("a_k_var", constructor_or_row a_k_var);
    ("a_k'_var'", constructor_or_row a_k'_var')]
let comparator { a_k_var=a1; a_k'_var'=a2 } { a_k_var=b1; a_k'_var'=b2 } =
  let open Blah.Compare.Solver_should_be_generated in
  compare_constructor_or_row a1 b1 <? fun () -> compare_constructor_or_row a2 b2

let propagator : (selector_output, _) Blah.Solver_types.propagator =
  fun selected repr ->
  Format.printf "In break_ctor.propagator for %a\n%!" printer selected;
  let a = selected.a_k_var in
  let b = selected.a_k'_var' in
  let get_tv : constructor_or_row -> type_variable = fun cr ->
    match cr with
    | `Row r -> repr r.tv
    | `Constructor c -> repr c.tv
  in
  (* The selector is expected to provice two constraints with the shape x = k(var …) and x = k'(var' …) *)
  let a_tv = repr @@ get_tv a in
  let b_tv = repr @@ get_tv b in
  assert (Blah.Var.equal a_tv b_tv);
  (* produce constraints: *)
  (* a.tv = b.tv *) (* nope, already the same *)
  (* let eq1 = c_equation (wrap (Propagator_break_ctor "a") @@ P_variable a_tv) (wrap (Propagator_break_ctor "b") @@ P_variable b_tv) "propagator: break_ctor" in *)
  (* let () = if Blah.Debug.debug_new_typer then
      let p = Blah.PP.c_constructor_simpl in
      Printf.fprintf stderr "%s" @@ Format.asprintf "\npropagator_break_ctor\na = %a\nb = %a\n%!" p a p b in *)
  (* a.c_tag = b.c_tag *)
  ( match a , b with
    | `Row a , `Row b ->
      if (Blah.Compare.Solver_should_be_generated.compare_simple_c_row a.r_tag b.r_tag) <> 0 then
        (* TODO : use error monad *)
        failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)"
                    Blah.PP.Solver_should_be_generated.debug_pp_c_row_simpl a
                    Blah.PP.Solver_should_be_generated.debug_pp_c_row_simpl b
                    (Blah.Compare.Solver_should_be_generated.compare_simple_c_row a.r_tag b.r_tag))
    | `Constructor a , `Constructor b ->
      if (Blah.Compare.Solver_should_be_generated.compare_simple_c_constant a.c_tag b.c_tag) <> 0 then
        (* TODO : use error monad *)
        failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)"
                    Blah.PP.Solver_should_be_generated.debug_pp_c_constructor_simpl a
                    Blah.PP.Solver_should_be_generated.debug_pp_c_constructor_simpl b
                    (Blah.Compare.Solver_should_be_generated.compare_simple_c_constant a.c_tag b.c_tag))
    | _ -> failwith "type error : break_ctor propagator"
  );
  (* Produce constraint a.tv_list = b.tv_list *)
  let%bind eqs3 =
    match a , b with
    | `Row a , `Row b ->
      let aux = fun ((la,{associated_variable=aa;_}),(lb,{associated_variable=bb;})) ->
        let%bind () = Trace.Assert.assert_true (corner_case "TODO: different labels la lb") (Blah.Compare.label la lb = 0) in
        ok @@ c_equation
          (wrap (Propagator_break_ctor "a") @@ P_variable (repr aa))
          (wrap (Propagator_break_ctor "b") @@ P_variable (repr bb))
          "propagator: break_ctor: row"
      in
      let%bind bindings =  List.map2 (fun x y -> (x,y)) (LMap.bindings a.tv_map) (LMap.bindings b.tv_map)
        ~ok ~fail:(fun _ _-> fail @@ (corner_case "TODO: different number of labels (List.length a.tv_map) (List.length b.tv_map)"))
      in
      bind_map_list aux bindings
    | `Constructor a , `Constructor b -> (
      let aux = fun aa bb -> c_equation (wrap (Propagator_break_ctor "a") @@ P_variable (repr aa)) (wrap (Propagator_break_ctor "b") @@ P_variable (repr bb)) "propagator: break_ctor: ctor" in
      List.map2 aux a.tv_list b.tv_list
        ~ok ~fail:(fun _ _ -> fail @@ different_constant_tag_number_of_arguments __LOC__ a.c_tag b.c_tag (List.length a.tv_list) (List.length b.tv_list))
    )
    | _ -> failwith "type error in eqs3"
  in
  let eqs = eqs3 in
  (* Format.printf "Break_ctor : returning with new constraint %a\n%!" (PP_helpers.list_sep_d Blah.PP.type_constraint_short) @@ eqs ; *)
  ok [
    {
      remove_constraints = [];
      add_constraints = eqs;
      proof_trace = Axiom Blah.Axioms.f_equal
    }
  ]

end

module OpaqueApi = struct
  module Types = Ast_typed.Types
  module Compare = struct
    include Ast_typed.Compare
    module Solver_should_be_generated = Solver_should_be_generated
  end
  module PP = struct
    include Ast_typed.PP
    module Solver_should_be_generated = Solver_should_be_generated
  end
  module Yojson = Ast_typed.Yojson
  module Var = Var
  module Solver_types = Typesystem.Solver_types
  module Misc = Ast_typed.Misc
  module Reasons = Ast_typed.Reasons
  module Axioms = Axioms
end

module Type_variable = struct type t = Ast_typed.Types.type_variable end
module MM = M(Type_variable)(OpaqueApi)



open Ast_typed.Types
open Typesystem.Solver_types

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
  a_k_var : constructor_or_row ;
  a_k'_var' : constructor_or_row ;
}
let selector = Compat.selector

(* module H = functor (Type_variable : sig type t end) -> struct
 *   type selector_output = output_break_ctor
 *   let heuristic_name = heuristic_name
 *   let selector repr c indexes =
 *     let module Indexes = (val indexes : INDEXES(Type_variable).S) in
 *     selector repr c (object
 *       method grouped_by_variable = Indexes.grouped_by_variable
 *     end)
 *   let alias_selector a b indexes =
 *     let module Indexes = (val indexes : INDEXES(Type_variable).S) in
 *     alias_selector a b (object
 *       method grouped_by_variable = Indexes.grouped_by_variable
 *     end)
 *   let get_referenced_constraints = get_referenced_constraints
 *   let propagator = propagator
 *   let printer = printer
 *   let printer_json = printer_json
 *   let comparator = comparator
 * end *)
