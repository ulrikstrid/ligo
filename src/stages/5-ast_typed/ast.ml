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
  | T_constant of ty_expr type_operator
  | T_sum      of rows
  | T_record   of rows
  | T_arrow    of ty_expr arrow

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

and matching_expr =
  | Match_list    of matching_content_list
  | Match_option  of matching_content_option
  | Match_variant of matching_content_variant

and declaration_loc = declaration location_wrap

and program_ = declaration_loc list

and program_with_unification_vars = Program_With_Unification_Vars of program_

and program_fully_typed = Program_Fully_Typed of program_

(* A Declaration_constant is described by
 *   a name + a type-annotated expression
 *   a boolean indicating whether it should be inlined
 *   the environment before the declaration (the original environment)
 *   the environment after the declaration (i.e. with that new declaration added to the original environment). *)
and declaration_constant = {
    binder : expression_variable ;
    expr : expression ;
    inline : bool ;
  }

and declaration_type = {
    type_binder : type_variable ;
    type_expr : type_expression ;
  }

and declaration =
  | Declaration_constant of declaration_constant
  | Declaration_type of declaration_type

and expression = {
    expression_content: expression_content ;
    location: location ;
    type_expression: type_expression ;
  }

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
  | E_raw_code of raw_code
  (* Variant *)
  | E_constructor of constructor (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expression_label_map
  | E_record_accessor of record_accessor
  | E_record_update   of record_update

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
    source_environment: environment ;
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

and environment = {
  expression_environment: expression_environment ;
  type_environment: type_environment ;
  }

and named_type_content = {
    type_name : type_variable;
    type_value : type_expression;
  }



(* Solver types

   The solver types are not actually part of the AST,
   so they could be moved to a separate file, but doing so would
   require updating every use of the Ast_typed.Types module to also
   include the solver types (a lot of work). Also, there is a lot of
   semi-duplication between the AST and the solver, so it's best to
   keep the two together until that gets refactored. *)

(* core *)

(* add information on the type or the kind for operator *)
type constant_tag =
  | C_arrow     (* * -> * -> *    isn't this wrong? *)
  | C_option    (* * -> * *)
  | C_map       (* * -> * -> * *)
  | C_big_map   (* * -> * -> * *)
  | C_list      (* * -> * *)
  | C_set       (* * -> * *)
  | C_unit      (* * *)
  | C_string    (* * *)
  | C_nat       (* * *)
  | C_mutez     (* * *)
  | C_timestamp (* * *)
  | C_int       (* * *)
  | C_address   (* * *)
  | C_bytes     (* * *)
  | C_key_hash  (* * *)
  | C_key       (* * *)
  | C_signature (* * *)
  | C_operation (* * *)
  | C_contract  (* * -> * *)
  | C_chain_id  (* * *)

type row_tag =
  | C_record    (* ( label , * ) … -> * *)
  | C_variant   (* ( label , * ) … -> * *)

(* TODO: rename to type_expression or something similar (it includes variables, and unevaluated functions + applications *)
type type_value_ =
  | P_forall       of p_forall
  | P_variable     of type_variable
  | P_constant     of p_constant
  | P_apply        of p_apply
  | P_row          of p_row

and type_value = {
  tsrc : string;
  t : type_value_ ;
  }

and p_apply = {
    tf : type_value ;
    targ : type_value ;
  }

and p_ctor_args = type_value list
and p_ctor_args_list = p_ctor_args list
and p_constant = {
    p_ctor_tag : constant_tag ;
    p_ctor_args : p_ctor_args ;
  }

and tv_lmap = type_value label_map
and p_row = {
    p_row_tag  : row_tag ;
    p_row_args : tv_lmap ;
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
and c_typeclass = {
  tc_args : tc_args ;
  typeclass : typeclass ;
}
and c_access_label = {
    c_access_label_tval : type_value ;
    accessor : label ;
    c_access_label_tvar : type_variable ;
  }
and type_constraint = {
  reason : string ;
  c : type_constraint_ ;
}
and type_constraint_ =
  (* | C_assignment of (type_variable * type_pattern) *)
  | C_equation of c_equation (* TVA = TVB *)
  | C_typeclass of c_typeclass (* TVL ∈ TVLs, for now in extension, later add intensional (rule-based system for inclusion in the typeclass) *)
  | C_access_label of c_access_label (* poor man's type-level computation to ensure that TV.label is type_variable *)
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

type constraint_identifier =
| ConstraintIdentifier of int64
type 'v constraint_identifierMap = (constraint_identifier, 'v) RedBlackTrees.PolyMap.t

type refined_typeclass = {
  refined : c_typeclass_simpl ;
  original : c_typeclass_simpl ;
  vars : type_variable_set ;
}

and type_variable_set = type_variable poly_set
and refined_typeclass_constraint_identifierMap = refined_typeclass constraint_identifierMap

and constraint_identifier_set = constraint_identifier RedBlackTrees.PolySet.t
and constraint_identifier_set_map = constraint_identifier_set typeVariableMap

and c_constructor_simpl_typeVariableMap = c_constructor_simpl typeVariableMap
and constraints_typeVariableMap = constraints typeVariableMap
and c_typeclass_simpl_constraint_identifierMap = c_typeclass_simpl constraint_identifierMap
and constraint_identifier_constraint_identifierMap = (constraint_identifier, constraint_identifier) RedBlackTrees.PolyMap.t
and type_constraint_simpl_list = type_constraint_simpl list

and structured_dbs = {
  all_constraints            : type_constraint_simpl_list ;
  aliases                    : unionfind ;
  (* assignments (passive data structure). *)
  (*   Now                   : just a map from unification vars to types (pb: what about partial types?) *)
  (*   maybe just local assignments (allow only vars as children of pair(α,β)) *)
  (* TODO                    : the rhs of the map should not repeat the variable name. *)
  assignments                : c_constructor_simpl_typeVariableMap ;
  grouped_by_variable        : constraints_typeVariableMap ; (* map from (unionfind) variables to constraints containing them *)
  cycle_detection_toposort   : unit ;                        (* example of structured db that we'll add later *)
  (* TODO: later have all constraints get an identtifier, not just typeclass constraints. *)
  by_constraint_identifier   : c_typeclass_simpl_constraint_identifierMap ;
  refined_typeclasses        : refined_typeclass_constraint_identifierMap ;
  refined_typeclasses_back   : constraint_identifier_constraint_identifierMap ;
  typeclasses_constrained_by : constraint_identifier_set_map ;
}

and c_constructor_simpl_list = c_constructor_simpl list
and c_poly_simpl_list        = c_poly_simpl        list
and c_typeclass_simpl_list   = c_typeclass_simpl   list
and c_row_simpl_list         = c_row_simpl         list
and constraints = {
  (* If implemented in a language with decent sets, these should be sets not lists. *)
  constructor : c_constructor_simpl_list ; (* List of ('a = constructor(args…)) constraints *)
  poly        : c_poly_simpl_list        ; (* List of ('a = forall 'b, some_type) constraints *)
  tc          : c_typeclass_simpl_list   ; (* List of (typeclass(args…)) constraints *)
  row         : c_row_simpl_list         ; (* List of ('a = row (args..)) constraints *)
}
and type_variable_list = type_variable list
and type_variable_lmap = type_variable label_map
and c_constructor_simpl = {
  reason_constr_simpl : string ;
  (* If false, the constraint can be deleted without compromising the correctness of the typechecker: it might be a constraint used for bookkeeping which helps with inference, but its removal does not risk causing an ill-typed program to be accepted. If true, this constraint might (or might not) be necessary for correctness. It is always safe to use "true" for correctness. Use "false" only when being sure it is safe to remove that constraint. *)
  is_mandatory_constraint : bool ;
  tv : type_variable;
  c_tag : constant_tag;
  (* Types wih no arguments like int, string etc. have an empty tv_list *)
  tv_list : type_variable_list;
}
and c_row_simpl = {
  reason_row_simpl : string ;
  (* see description above in c_constructor_simpl *)
  is_mandatory_constraint : bool ;
  tv : type_variable;
  r_tag : row_tag;
  tv_map : type_variable_lmap;
}
and c_const_e = {
    c_const_e_tv : type_variable ;
    c_const_e_te : type_expression ;
  }
and c_equation_e = {
    aex : type_expression ;
    bex : type_expression ;
  }
and c_typeclass_simpl = {
  reason_typeclass_simpl : string ;
  (* see description above in c_constructor_simpl *)
  is_mandatory_constraint : bool ;
  id_typeclass_simpl     : constraint_identifier ;
  tc   : typeclass          ;
  args : type_variable_list ;
}
and c_poly_simpl = {
  reason_poly_simpl : string ;
  (* see description above in c_constructor_simpl *)
  is_mandatory_constraint : bool ;
  tv     : type_variable ;
  forall : p_forall      ;
}
and type_constraint_simpl =
  | SC_Constructor of c_constructor_simpl             (* α = ctor(β, …) *)
  | SC_Alias       of c_alias                         (* α = β *)
  | SC_Poly        of c_poly_simpl                    (* α = forall β, δ where δ can be a more complex type *)
  | SC_Typeclass   of c_typeclass_simpl               (* TC(α, …) *)
  | SC_Row         of c_row_simpl                     (* α = row(l -> β, …) *)

and deduce_and_clean_result = {
  deduced : c_constructor_simpl_list ;
  cleaned : c_typeclass_simpl ;
}

and c_alias = {
    reason_alias_simpl : string ;
    (* see description above in c_constructor_simpl *)
    is_mandatory_constraint : bool ;
    a : type_variable ;
    b : type_variable ;
  }


(* sub-sub component: lazy selector (don't re-try all selectors every time) *)
(* For now: just re-try everytime *)

(* selector / propagation rule for breaking down composite types *)
(* For now: break pair(a, b) = pair(c, d) into a = c, b = d *)
type output_break_ctor = {
    a_k_var : c_constructor_simpl ;
    a_k'_var' : c_constructor_simpl ;
  }

type output_specialize1 = {
    poly : c_poly_simpl ;
    a_k_var : c_constructor_simpl ;
  }

type output_tc_fundep = {
    tc : refined_typeclass ;
    c : c_constructor_simpl ;
  }

type m_break_ctor__already_selected = output_break_ctor poly_set
type m_specialize1__already_selected = output_specialize1 poly_set

type already_selected = {
  break_ctor  : m_break_ctor__already_selected  ;
  specialize1 : m_specialize1__already_selected ;
}

type type_constraint_list = type_constraint list
type update = {
  remove_constraints : type_constraint_simpl_list ;
  add_constraints : type_constraint_list ;
  justification : string ;
}
type updates = update list
type updates_list = updates list






(* This is in a functor to fix a dependency cycle between ast.ml and
   typer_errors.ml. *)
module Dep_cycle (Typer_errors : sig type typer_error end) = struct
  type ('old, 'new_) updater = {
    map : 'v . 'old -> 'old -> ('old, 'v) PolyMap.t -> ('new_, 'v) PolyMap.t;
  }


  (* Each normalizer returns an updated database (after storing the
     incoming constraint) and a list of constraints, used when the
     normalizer rewrites the constraints e.g. into simpler ones. *)
  (* TODO: If implemented in a language with decent sets, should be 'b set not 'b list. *)
  type ('state, 'a , 'b) normalizer = 'state -> 'a -> ('state * 'b list)
  type ('state, 'a) normalizer_rm = 'state -> 'a -> ('state, Typer_errors.typer_error) result

  module type DBPlugin = sig
    type 'typeVariable t
    val empty : cmp:('typeVariable -> 'typeVariable -> int) -> 'typeVariable t
    val add_constraint : (type_variable t, type_constraint_simpl, type_constraint_simpl) normalizer
    val remove_constraint : (type_variable t, type_constraint_simpl) normalizer_rm
    val merge_aliases : ('old, 'new_) updater -> 'old -> 'old -> 'old t -> 'new_ t
  end

  module Assignments : DBPlugin = struct
    type 'typeVariable t = ('typeVariable, c_constructor_simpl) PolyMap.t
    let empty ~cmp = PolyMap.create ~cmp
    let add_constraint _ = failwith "todo"
    let remove_constraint _ = failwith "todo"
    let merge_aliases : 'old 'new_ . ('old, 'new_) updater -> 'old -> 'old -> 'old t -> 'new_ t =
      fun updater a b state -> updater.map a b state
  end

  module GroupedByVariable : DBPlugin = struct
    (* map from (unionfind) variables to constraints containing them *)
    type 'typeVariable t = ('typeVariable, constraints) PolyMap.t
    let empty ~cmp = PolyMap.create ~cmp
    let add_constraint _ = failwith "todo"
    let remove_constraint _ = failwith "todo"
    let merge_aliases =
      fun updater a b state -> updater.map a b state
  end
  
  type 't plugin_instance = {
    state : 't;
    merge_aliases : type_variable -> type_variable -> 't -> 't;
  }

  type ex_plugin_instance =
    | Plugin_instance : 't plugin_instance -> ex_plugin_instance

  module type DBPluginInstance = sig
    module M : DBPlugin
    val v : 'a M.t
  end

  type 'typeVariable plugin_instances = {
    assignments         : 'typeVariable       Assignments.t;
    grouped_by_variable : 'typeVariable GroupedByVariable.t;
  }

  module Functor(In : sig type 'a t end)(Out : sig type 'a t end) = struct
    module type M = sig
      module M(P : DBPlugin) : sig
        module P = P
        val f : In.t P.t -> Out.t P.t
      end
    end
  end
  module type Functor = module type of Functor

  module Phunctor(In : sig type t end)(Out : sig type t end) = struct
    module type M = sig
      module P = P
      val f : In.t -> Out.t
    end
  end

  (* PerPluginType takes a type for 'typeVariable, a Plugin.t, and
     produces an arbitrary type which can depend on these two.

     e.g. given a module
       Ppt : PerPluginType
     the type
       Ppt.M(type_variable)(SomePlugin).t
     could be one of:
       type_variable SomePlugin.t
       int SomePlugin.t
       type_variable list
       int                                                          *)
  (* type PerPluginType = 🞰→(🞰→🞰)→🞰 *)
  module type PerPluginType = sig
    module M(TypeVariable : sig type t end)(Plugin : sig type 'typeVariable t end) : sig
      type t
    end
  end

  module MMM(TypeVariable : sig type t end)(Plugin : sig type 'typeVariable t end) = struct
    type t
  end

  module type Toto = functor (A : sig type t end) (B : sig type t end) -> sig
    val f : A.t -> B.t
  end

  let g (m : (module Toto)) =
    let module M' = ((val m)(struct type t = int end)(struct type t = string end)) in
    M'.f 42

  module type PluginInstances = functor (TypeVariable : sig type t end)(Ppt : PerPluginType) -> sig
    (* type t = { *)
    val assignments         : Ppt.M(TypeVariable)(Assignments).t
    val grouped_by_variable : Ppt.M(TypeVariable)(GroupedByVariable).t
    (* } *)
  end
  
  (* map_plugins :: (In : PerPluginType) →
                    (Out : PerPluginType) →
                    (f : Functor(In)(Out)) →
                    (TypeVarable : 🞰) →
                    (pluginInstances :  PluginInstances TypeVarable In) →
                    PluginInstances TypeVarable Out
   *)
  module MapPlugins (In : PerPluginType) (Out : PerPluginType) = struct
    module IA = In.M(struct type t = type_variable end)(Assignments)
    module OA = Out.M(struct type t = type_variable end)(Assignments)
    module type A = Phunctor(IA)(OA).M
    (* .M.M(Assignments) *)
    
    let map_plugins (f : (module Functor)) (instances : 'a plugin_instances) : 'b plugin_instances =
      let { assignments=_; grouped_by_variable=_ } = instances in
      let module Functor = (val f) in
      {
        (* Functor (In typeVariable Assignments) (Out typeVariable Assignments) *)
        assignments = (let module F = … in (failwith "F.f assignments"));
        grouped_by_variable = (let module F = Functor.M(GroupedByVariable) in (failwith "F.f grouped_by_variable"));
      }
  end

  (* WIP prototype of the solver's main loop with plugins *)
  let main (plugins : (module DBPlugin) list) =
    let updater = {
      map = fun _a _b m -> (*ReprMap.merge a b*) m
    } in
    let plugin_instances = List.map
        (fun plugin ->
           let module Plugin = (val plugin : DBPlugin) in
           Plugin_instance {
             state = Plugin.empty ~cmp:Var.compare;
             merge_aliases = fun a b state ->
               Plugin.merge_aliases updater a b state;
           })
        plugins
    in
    ignore plugin_instances;
end
