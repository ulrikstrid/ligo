[@@@warning "-30-32"]

open Simple_utils.Function
include Stage_common.Types

(* pseudo-typeclasses: interfaces that must be provided for arguments
   of the givent polymmorphic types. For now, only one typeclass can
   be specified for a given polymorphic type. The implementation is
   provided by the Comparable module *)
(*@ typeclass poly_unionfind comparable *)
(*@ typeclass poly_set       comparable *)
type ast_core_type_expression = Ast_core.type_expression

type te_lmap = row_element label_map
and type_meta = ast_core_type_expression option

and type_content =
  | T_variable of type_variable
  | T_constant of type_injection
  | T_sum      of rows
  | T_record   of rows
  | T_arrow    of ty_expr arrow
  | T_module_accessor of ty_expr module_access
  | T_singleton of literal

and type_injection = {
  language : string ;
  injection : Ligo_string.t ;
  (* kind (?) *)
  parameters : ty_expr list ;
}

and rows = {
  content : row_element label_map;
  layout : layout ;
}

and te_list = type_expression list

and annot_option = string option

and row_element = type_expression row_element_mini_c

and type_map_args = {
    k : type_expression;
    v : type_expression;
  }

and michelson_or_args = {
    l : type_expression;
    r : type_expression;
  }

and type_expression = {
    type_content: type_content;
    type_meta: type_meta;
    orig_var: type_variable option ;
    location: location;
  }
and ty_expr = type_expression

and matching_content_cons = {
    hd : expression_variable;
    tl : expression_variable;
    body : expression;
    tv : type_expression;
  }

and matching_content_list = {
    match_nil : expression ;
    match_cons : matching_content_cons;
  }

and matching_content_some = {
    opt  : expression_variable ;
    body : expression ;
    tv   : type_expression ;
  }

and matching_content_option = {
    match_none : expression ;
    match_some : matching_content_some ;
  }

and expression_variable_list = expression_variable list
and type_expression_list = type_expression list

and matching_content_case = {
    constructor : label ;
    pattern : expression_variable ;
    body : expression ;
  }

and matching_content_case_list = matching_content_case list

and matching_content_variant = {
    cases: matching_content_case_list;
    tv: type_expression;
  }

and matching_content_record = {
  fields : (expression_variable * type_expression) label_map;
  body : expression;
  tv : type_expression;
}

and matching_expr =
  | Match_list    of matching_content_list
  | Match_option  of matching_content_option
  | Match_variant of matching_content_variant
  | Match_record of matching_content_record

and declaration_loc = declaration location_wrap

and module' = declaration_loc list

and module_with_unification_vars = Module_With_Unification_Vars of module'

and module_fully_typed = Module_Fully_Typed of module'

(* A Declaration_constant is described by
 *   a name + a type-annotated expression
 *   a boolean indicating whether it should be inlined
 *   the environment before the declaration (the original environment)
 *   the environment after the declaration (i.e. with that new declaration added to the original environment). *)
and declaration_constant = {
    name : string option ;
    binder : expression_variable ;
    expr : expression ;
    inline : bool ;
  }

and declaration_type = {
    type_binder : type_variable ;
    type_expr   : type_expression ;
  }

and declaration_module = {
    module_binder : module_variable ;
    module_       : module_fully_typed ;
  }

and declaration' =
  | Declaration_constant of declaration_constant
  | Declaration_type of declaration_type
  | Declaration_module of declaration_module
  | Module_alias       of module_alias

and declaration = declaration'

and expression = {
    expression_content: expression_content ;
    location: location ;
    type_expression: type_expression ;
  }

and expr = expression

and map_kv = {
    key : expression ;
    value : expression ;
  }

and look_up = {
    ds : expression;
    ind : expression;
  }

and expression_label_map = expression label_map
and expression_list = expression list

and expression_content =
  (* Base *)
  | E_literal of literal
  | E_constant of constant (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_application of application
  | E_lambda of lambda
  | E_recursive of recursive
  | E_let_in of let_in
  | E_type_in of (expr, ty_expr) type_in
  | E_mod_in of mod_in
  | E_mod_alias of expr mod_alias
  | E_raw_code of raw_code
  (* Variant *)
  | E_constructor of constructor (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expression_label_map
  | E_record_accessor of record_accessor
  | E_record_update   of record_update
  | E_module_accessor of expression module_access

and constant = {
    cons_name: constant' ;
    arguments: expression_list ;
  }

and application = {
  lamb: expression ;
  args: expression ;
  }

and lambda =  {
    binder: expression_variable ;
    result: expression ;
  }

and let_in = {
    let_binder: expression_variable ;
    rhs: expression ;
    let_result: expression ;
    inline : bool ;
  }

and mod_in = {
    module_binder: module_variable ;
    rhs: module_fully_typed ;
    let_result: expression ;
  }

and raw_code = {
  language : string;
  code : expression;
  }

and recursive = {
  fun_name : expression_variable;
  fun_type : type_expression;
  lambda : lambda;
  }

and constructor = {
    constructor: label;
    element: expression ;
  }

and record_accessor = {
    record: expression ;
    path: label ;
  }

and record_update = {
    record: expression ;
    path: label ;
    update: expression ;
  }

and matching = {
    matchee: expression ;
    cases: matching_expr ;
  }

and ascription = {
    anno_expr: expression ;
    type_annotation: type_expression ;
  }

and environment_element_definition =
  | ED_binder
  | ED_declaration of environment_element_definition_declaration

and environment_element_definition_declaration = {
    expression: expression ;
    free_variables: free_variables ;
  }

and free_variables = expression_variable list

and environment_element = {
    type_value: type_expression ;
    definition: environment_element_definition ;
  }

and expression_environment = environment_binding list

and environment_binding = {
    expr_var: expression_variable ;
    env_elt: environment_element ;
  }

and type_environment = type_environment_binding list

and type_environment_binding = {
    type_variable: type_variable ;
    type_: type_expression ;
  }

and module_environment = module_environment_binding list

and module_environment_binding = {
  module_variable : module_variable ;
  module_ : environment ;
}
and environment = {
  expression_environment: expression_environment ;
  type_environment: type_environment ;
  module_environment : module_environment ;
  }

(* Solver types


   The solver types are not actually part of the AST,
   so they could be moved to a separate file, but doing so would
   require updating every use of the Ast_typed.Types module to also
   include the solver types (a lot of work). Also, there is a lot of
   semi-duplication between the AST and the solver, so it's best to
   keep the two together until that gets refactored. *)

(* core *)
module ConstraintIdentifier = struct
  type t = T of int64
  let counter : int64 ref = ref Int64.zero 
  let fresh () = 
    let res = !counter in
    counter := Int64.succ !counter;
    T res

end

(* add information on the type or the kind for operator *)
type constant_tag =
  | C_arrow        (* * -> * -> * *)
  | C_option       (* * -> * *)
  | C_map          (* * -> * -> * *)
  | C_big_map      (* * -> * -> * *)
  | C_list         (* * -> * *)
  | C_set          (* * -> * *)
  | C_unit         (* * *)
  | C_string       (* * *)
  | C_nat          (* * *)
  | C_mutez        (* * *)
  | C_timestamp    (* * *)
  | C_int          (* * *)
  | C_address      (* * *)
  | C_bytes        (* * *)
  | C_key_hash     (* * *)
  | C_key          (* * *)
  | C_signature    (* * *)
  | C_operation    (* * *)
  | C_contract     (* * -> * *)
  | C_chain_id     (* * *)
  | C_bls12_381_g1 (* * *)
  | C_bls12_381_g2 (* * *)
  | C_bls12_381_fr (* * *)

type row_tag =
  | C_record    (* ( label , * ) … -> * *)
  | C_variant   (* ( label , * ) … -> * *)

(* TODO: rename to type_expression or something similar (it includes variables, and unevaluated functions + applications *)
type type_value_ =
  | P_forall       of p_forall
  | P_variable     of type_variable
  | P_constant     of p_constant
  | P_apply        of p_apply   (* TODO: remove this until it is usead (for now waiting on a kinding system and appropriate evaluation heuristics similar to eval_beta_root in src/stages/typesystem/misc.ml) *)
  | P_row          of p_row
    (* new stuff: *)
  | P_abs          of p_abs
  | P_constraint   of p_constraint

and type_value = type_value_ location_wrap

and p_apply = {
    tf : type_value ;
    targ : type_value ;
  }

and p_abs = { arg: type_variable; ret: type_value }
and p_constraint = { pc: type_constraint }

and p_ctor_args = type_value list
and p_ctor_args_list = p_ctor_args list
and p_constant = {
    p_ctor_tag : constant_tag ;
    p_ctor_args : p_ctor_args ;
  }

and row_value = {
  associated_value : type_value;
  michelson_annotation : string option;
  decl_pos : int;
}

and row_lmap = row_value label_map
and p_row = {
    p_row_tag  : row_tag ;
    p_row_args : row_lmap ;
  }

and p_constraints = type_constraint list

and p_forall = {
  binder      : type_variable ;
  constraints : p_constraints ;
  body        : type_value ;
}

(* Different type of constraint *)
and ctor_args = type_variable list (* non-empty list *)
and simple_c_constructor = {
    ctor_tag : constant_tag ;
    ctor_args : ctor_args ;
  }
and simple_c_constant = {
    constant_tag: constant_tag ; (* for type constructors that do not take arguments *)
  }
and c_const = {
    c_const_tvar : type_variable ;
    c_const_tval : type_value ;
  }
and c_equation = {
  aval : type_value ;
  bval : type_value ;
}
and tc_args = type_value list
and constraint_identifier = ConstraintIdentifier.t

and c_typeclass = {
  (* TODO: possible bug: bound variables must not intersect with the tc_args *)
  tc_bound : type_variable list ;
  tc_constraints : type_constraint list ;
  tc_args : tc_args ;
  original_id : constraint_identifier option ;
  typeclass : typeclass ;
}
and c_access_label = {
    c_access_label_record_type : type_value ;
    accessor : label ;
    c_access_label_tvar : type_variable ;
  }
and c_apply = {
  f: type_variable;
  arg: type_variable;
}
and type_constraint = {
  reason : string ;
  c : type_constraint_ ;
}
and type_constraint_ =
  (* | C_assignment of (type_variable * type_pattern) *)
  | C_equation of c_equation (* TVA = TVB *)
  | C_typeclass of c_typeclass (* TVL ∈ TVLs, for now in extension, later add intensional (rule-based system for inclusion in the typeclass) *)
  | C_access_label of c_access_label (* simple substitute for a type-level computation to ensure that TV.label is type_variable *)
  | C_apply of c_apply
(* | … *)

(* is the first list in case on of the type of the type class as a kind *->*->* ? *)
and tc_allowed = type_value list
and typeclass = tc_allowed list

(* end core *)

type 'a poly_unionfind = 'a UnionFind.Poly2.t
type 'a poly_set = 'a RedBlackTrees.PolySet.t

(* typevariable: to_string = (fun s -> Format.asprintf "%a" Var.pp s) *)
(* representant for an equivalence class of type variables *)
type 'v typeVariableMap = (type_variable, 'v) RedBlackTrees.PolyMap.t
let typeVariableMap_to_yojson f tvmap =
  bindings_to_yojson type_variable_to_yojson f @@ RedBlackTrees.PolyMap.bindings tvmap

let typeVariableMap_of_yojson f tvmap =
  Stdlib.Result.bind (Stage_common.Of_yojson.bindings type_variable_of_yojson f tvmap)
    (Stdlib.Option.to_result ~none:"Map with duplicates" <@ RedBlackTrees.PolyMap.from_list ~cmp:compare)

(* typevariable: to_string = (fun s -> Format.asprintf "%a" Var.pp s) *)
type unionfind = type_variable poly_unionfind
let unionfind_to_yojson _ = `String "type_varianle unionfind"
(* TODO : use error monad *)
let unionfind_of_yojson _ = Error ("can't parse unionfind")

type 'v constraint_identifierMap = (constraint_identifier, 'v) RedBlackTrees.PolyMap.t

and type_variable_set = type_variable poly_set

and constraint_identifier_set = constraint_identifier RedBlackTrees.PolySet.t
and constraint_identifier_set_map = constraint_identifier_set typeVariableMap

and c_constructor_simpl_typeVariableMap = c_constructor_simpl typeVariableMap
and c_typeclass_simpl_constraint_identifierMap = c_typeclass_simpl constraint_identifierMap
and constraint_identifier_constraint_identifierMap = (constraint_identifier, constraint_identifier) RedBlackTrees.PolyMap.t

and c_constructor_simpl_list = c_constructor_simpl list
and c_poly_simpl_list        = c_poly_simpl        list
and c_typeclass_simpl_list   = c_typeclass_simpl   list
and c_row_simpl_list         = c_row_simpl         list
and type_variable_lmap = type_variable label_map
and c_constructor_simpl = {
  reason_constr_simpl : string ;
  id_constructor_simpl : constraint_identifier ;
  original_id  : constraint_identifier option ;
  tv : type_variable;
  c_tag : constant_tag;
  (* Types wih no arguments like int, string etc. have an empty tv_list *)
  tv_list : type_variable list;
}
and c_row_simpl = {
  reason_row_simpl : string ;
  (* see description above in c_constructor_simpl *)
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
and c_const_e = {
    c_const_e_tv : type_variable ;
    c_const_e_te : type_expression ;
  }
and c_equation_e = {
    aex : type_expression ;
    bex : type_expression ;
  }

  (* my_tc       = ∃ α β, (Packable α && Packable β && Show α) => (χ,γ) ∈ [(int,string), (bool, string), (α,β), (α,mutez) ] *)
  (*env = [
    (bool, P_row C_variant [Label True unit; Label False unit])
  ]
  my_tc = {
    tc_bound = [α; β];
    tc_constraints = [
      SC_Typeclass { tc_bound = []; tc_constraints = []; args = [α]; tc = [(int),(bool),(nat)]; }   (* Packable α *)
      SC_Typeclass { tc_bound = []; tc_constraints = []; args = [β]; tc = [(int),(bool),(nat)]; }   (* Packable β *)
      SC_Typeclass { tc_bound = []; tc_constraints = []; args = [α]; tc = [(int),(string),(nat)]; } (* Show α *)
    ];
    vars = [χ;γ];
    [
      [ P_constant int []; P_constant string [] ];
      [ P_variable bool;   P_constant string [] ];
      [ P_variable α;      P_variable β         ];
      [ P_variable α;      P_constant mutez     ];
    ]*)
(* the type c_typeclass_simpl is "∃ tc_bound, tc_constraints => args ∈ tc"
   given the env
      Packable    = ∃ , () => (δ) ∈ [(int),(bool),(nat)]
      Show        = ∃ , () => (δ) ∈ [(int),(string),(nat)]
   and the typeclass
      my_tc       = ∃ α β, (Packable α && Packable β && Show α) => (χ,γ) ∈ [(int,string), (bool, string), (α,β), (α,mutez) ]
  
   the allowed combinations of types for χ and γ are
    χ = int   && γ = string
    χ = bool  && γ = string
    χ = int   && γ = int
    χ = int   && γ = bool
    χ = int   && γ = nat
    χ = nat   && γ = int
    χ = nat   && γ = bool
    χ = nat   && γ = nat
    χ = int   && γ = mutez
    χ = nat   && γ = mutez
    *)
and c_typeclass_simpl = {
  reason_typeclass_simpl : string ;
  (* see description above in c_constructor_simpl *)
  id_typeclass_simpl : constraint_identifier ;
  original_id        : constraint_identifier option ; (* Pointer to the original typeclass, if this one is a refinement of it *)
  tc_bound           : type_variable list ;
  tc_constraints     : type_constraint_simpl list ;
  tc   : typeclass          ;
  args : type_variable list ;
}
and c_access_label_simpl = {
  reason_access_label_simpl : string ;
  id_access_label_simpl : constraint_identifier ;
  record_type : type_variable ;
  label : label ;
  tv : type_variable ;          (* result of the access label, e.g. α in the constraint α = β.ℓ *)
}
and c_poly_simpl = {
  reason_poly_simpl : string ;
  (* see description above in c_constructor_simpl *)
  id_poly_simpl : constraint_identifier ;
  original_id   : constraint_identifier option ;
  tv     : type_variable ;
  forall : p_forall      ;
}
and c_apply_simpl = {
  id_apply_simpl : constraint_identifier;
  reason_apply_simpl : string;
  f: type_variable;
  arg: type_variable;
}
and c_abs_simpl = {
  id_abs_simpl : constraint_identifier;
  reason_abs_simpl : string;
  tv:type_variable;
  param: type_variable;
  body: type_value;
}
and type_constraint_simpl =
  | SC_Apply       of c_apply_simpl                   (* φ(α) *)
  | SC_Abs         of c_abs_simpl                     (* α = λβ.τ *)
  | SC_Constructor of c_constructor_simpl             (* α = ctor(β, …) *)
  | SC_Alias       of c_alias                         (* α = β *)
  | SC_Poly        of c_poly_simpl                    (* α = forall β, δ where δ can be a more complex type *)
  | SC_Typeclass   of c_typeclass_simpl               (* α ∈ TC(, …) *)
  | SC_Access_label of c_access_label_simpl           (* α = β.ℓ *)
  | SC_Row         of c_row_simpl                     (* α = row(l -> β, …) *)

and c_alias = {
    reason_alias_simpl : string ;
    (* see description above in c_constructor_simpl *)
    a : type_variable ;
    b : type_variable ;
  }


(* sub-sub component: lazy selector *)
  
type constructor_or_row = [ (* TODO : c_row_simpl and c_constructor_simpl must be merged*)
  | `Constructor of c_constructor_simpl
  | `Row of c_row_simpl
]

(* selector / propagation rule for breaking down composite types *)
(* For now: break pair(a, b) = pair(c, d) into a = c, b = d *)



type type_constraint_list = type_constraint list

(* For now all our "axioms" are just human-readable justifications
   used within the compiler to explain why an operation is valid. In
   the future, we can have proper signatures for the axioms *)
type axiom = HandWaved of string

type proof_trace =
  | Axiom of axiom
  (* | … future extension: allow for proof traces *)

type update = {
  remove_constraints : type_constraint_simpl list ;
  add_constraints : type_constraint_list ;
  add_constraints_simpl : type_constraint_simpl list ;
  proof_trace : proof_trace ;
}
type updates = update list
type updates_list = updates list
