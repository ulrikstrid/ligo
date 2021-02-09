(* TODO: move this with the AST, probably? *)
module Solver_should_be_generated = Solver_should_be_generated
module Axioms = Axioms

module TYPE_VARIABLE_ABSTRACTION = functor (Type_variable : sig type t end) -> struct
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

module Opaque_type_variable = struct
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
