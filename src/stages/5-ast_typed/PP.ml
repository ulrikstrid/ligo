[@@@coverage exclude_file]
open Ast
open Format
open PP_helpers
include Stage_common.PP

type 'a pretty_printer = Format.formatter -> 'a -> unit

let lmap_sep value sep ppf m =
  let lst = List.sort (fun (Label a,_) (Label b,_) -> String.compare a b) m in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, v) = fprintf ppf "%a" value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep_t value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, {associated_type;_}) = fprintf ppf "%a" value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

(* Prints records which only contain the consecutive fields
   0..(cardinal-1) as tuples *)
let tuple_or_record_sep value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m
let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep_t value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep_t value (tag sep_record)) m
let list_sep_d_short x = list_sep x (tag " , ")
let list_sep_d x = list_sep x (tag " ,@ ")
let kv_short value_pp ~assoc ppf (k, v) = fprintf ppf "%a%s%a" label k assoc value_pp v
let lmap_sep_short x ~sep ~assoc ppf m =
  let lst = List.sort (fun (Label a,_) (Label b,_) -> String.compare a b) m in
  list_sep (kv_short x ~assoc) (tag sep) ppf lst
let lmap_sep_d x = lmap_sep x (tag " ,@ ")
let tuple_or_record_sep_expr value = tuple_or_record_sep value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " ,@ "
let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " *@ "

let type_variable ppf (t : type_variable) : unit = fprintf ppf "%a" Var.pp t
let module_variable ppf (m : module_variable) : unit = pp_print_string ppf m

open Format

let rec constraint_identifier_unicode (ci : Int64.t) =
  let digit =
    let ( - ) = Int64.sub in
    let ( / ) = Int64.div in
    let ( * ) = Int64.mul in
    match (ci - ((ci / 10L) * 10L)) with
      a when Int64.equal a 0L -> "₀"
    | a when Int64.equal a 1L -> "₁"
    | a when Int64.equal a 2L -> "₂"
    | a when Int64.equal a 3L -> "₃"
    | a when Int64.equal a 4L -> "₄"
    | a when Int64.equal a 5L -> "₅"
    | a when Int64.equal a 6L -> "₆"
    | a when Int64.equal a 7L -> "₇"
    | a when Int64.equal a 8L -> "₈"
    | a when Int64.equal a 9L -> "₉"
    | _ -> failwith (Format.asprintf "internal error: couldn't pretty-print int64: %Li (is it a negative number?)" ci)
  in
  if ci = 0L then "" else (constraint_identifier_unicode (Int64.div ci 10L)) ^ digit

let constraint_identifier_short ppf x =
  if Int64.equal x 0L
  then Format.fprintf ppf "₀"
  else Format.fprintf ppf "%s" (constraint_identifier_unicode x)

let list_sep_d_par f ppf lst =
  match lst with
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_content -> unit =
  fun ppf tc ->
  match tc with
  | T_variable        tv -> type_variable                 ppf tv
  | T_constant        tc -> type_injection ppf tc
  | T_sum              m -> fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row) (LMap.to_kv_list_rev m.content)
  | T_record           m -> fprintf ppf "%a" record m
  | T_arrow            a -> arrow         type_expression ppf a
  | T_module_accessor ma -> module_access type_expression ppf ma
  | T_singleton       x  -> literal       ppf             x

and row : formatter -> row_element -> unit =
  fun ppf { associated_type ; michelson_annotation=_ ; decl_pos=_ } ->
    fprintf ppf "%a"
      type_expression associated_type

and type_injection ppf {language;injection;parameters} =
  ignore language;
  fprintf ppf "%s%a" (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters

and record ppf {content; layout=_} =
  fprintf ppf "%a"
    (tuple_or_record_sep_type type_expression) content

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te.type_content

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev.wrap_content


let rec expression ppf (e : expression) =
  fprintf ppf "%a"
    expression_content e.expression_content

and expression_content ppf (ec: expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "(%a)@(%a)" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "%a(%a)" label c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "%a(%a)" constant' c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "%a" (tuple_or_record_sep_expr expression) m
  | E_record_accessor ra ->
      fprintf ppf "%a.%a" expression ra.record label ra.path
  | E_record_update {record; path; update} ->
      fprintf ppf "{ %a with { %a = %a } }" expression record label path expression update
  | E_lambda {binder; result} ->
      fprintf ppf "lambda (%a) return %a" expression_variable binder
        expression result
  | E_matching {matchee; cases;} ->
      fprintf ppf "match %a with %a" expression matchee (matching expression) cases
  | E_let_in {let_binder; rhs; let_result; inline} ->
      fprintf ppf "let %a = %a%a in %a" expression_variable let_binder expression
        rhs option_inline inline expression let_result
  | E_type_in   ti -> type_in expression type_expression ppf ti
  | E_mod_in {module_binder; rhs; let_result} ->
      fprintf ppf "let %a = %a in %a" module_variable module_binder module_fully_typed
        rhs expression let_result
  | E_mod_alias ma -> mod_alias expression ppf ma
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_recursive { fun_name;fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )"
        expression_variable fun_name
        type_expression fun_type
        expression_content (E_lambda lambda)
  | E_module_accessor ma -> module_access expression ppf ma


and option_inline ppf inline =
  if inline then
    fprintf ppf "[@inline]"
  else
    fprintf ppf ""

and matching_variant_case : (_ -> expression -> unit) -> _ -> matching_content_case -> unit =
  fun f ppf {constructor=c; pattern; body} ->
  fprintf ppf "| %a %a -> %a" label c expression_variable pattern f body

and matching : (formatter -> expression -> unit) -> _ -> matching_expr -> unit = fun f ppf m -> match m with
  | Match_variant {cases ; tv=_} ->
      fprintf ppf "%a" (list_sep (matching_variant_case f) (tag "@.")) cases
  | Match_list {match_nil ; match_cons = {hd; tl; body; tv=_}} ->
      fprintf ppf "| Nil -> %a @.| %a :: %a -> %a" f match_nil expression_variable hd expression_variable tl f body
  | Match_option {match_none ; match_some = {opt; body; tv=_}} ->
      fprintf ppf "| None -> %a @.| Some %a -> %a" f match_none expression_variable opt f body
  | Match_record {fields ; body ; tv = _} ->
      let with_annots f g ppf (a , b) = fprintf ppf "%a:%a" f a g b in
      (* let fields = LMap.map (fun (v,_) -> v) fields in *)
      fprintf ppf "| %a -> %a"
        (tuple_or_record_sep_expr (with_annots expression_variable type_expression)) fields
        f body

and declaration ppf (d : declaration) =
  match d with
  | Declaration_constant {name = _; binder; expr; inline} ->
      fprintf ppf "const %a = %a%a" expression_variable binder expression expr option_inline inline
  | Declaration_type {type_binder; type_expr} ->
      fprintf ppf "type %a = %a" type_variable type_binder type_expression type_expr
  | Declaration_module {module_binder; module_} ->
      fprintf ppf "module %a = %a" module_variable module_binder module_fully_typed module_
  | Module_alias {alias; binders} ->
      fprintf ppf "module %a = %a" module_variable alias (list_sep module_variable (tag ".")) @@ List.Ne.to_list binders

and module_fully_typed ppf (Module_Fully_Typed p : module_fully_typed) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)

let module_with_unification_vars ppf (Module_With_Unification_Vars p : module_with_unification_vars) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)

let typeVariableMap = fun f ppf tvmap   ->
      let lst = List.sort (fun (a, _) (b, _) -> Var.compare a b) (RedBlackTrees.PolyMap.bindings tvmap) in
      let aux ppf (k, v) =
        fprintf ppf "(Var %a, %a)" Var.pp k f v in
      fprintf ppf "typeVariableMap [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let typeVariableSet = fun ppf s   ->
      let lst = List.sort (fun (a) (b) -> Var.compare a b) (RedBlackTrees.PolySet.elements s) in
      let aux ppf (k) =
        fprintf ppf "(Var %a)" Var.pp k in
      fprintf ppf "typeVariableSet [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst
let constraint_identifier_set = fun ppf s   ->
      let lst = List.sort (fun (ConstraintIdentifier.T a) (ConstraintIdentifier.T b) -> Int64.compare a b) (RedBlackTrees.PolySet.elements s) in
      let aux ppf (ConstraintIdentifier.T k) =
        fprintf ppf "(ConstraintIdentifier %Li)" k in
      fprintf ppf "constraint_identifier_set [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let identifierMap = fun f ppf idmap ->
      let lst = List.sort (fun (ConstraintIdentifier.T a, _) (ConstraintIdentifier.T b, _) -> Int64.compare a b) (RedBlackTrees.PolyMap.bindings idmap) in
      let aux ppf (ConstraintIdentifier.T k, v) =
        fprintf ppf "(ConstraintIdentifier %Li, %a)" k f v in
      fprintf ppf "typeVariableMap [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let biMap = fun fk fv ppf idmap ->
      let lst = RedBlackTrees.PolyBiMap.bindings idmap in
      let aux ppf (k, v) =
        fprintf ppf "(%a, %a)" fk k fv v in
      fprintf ppf "typeVariableMap [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst
let poly_unionfind = (fun f ppf p   ->
  let lst = (UnionFind.Poly2.partitions p) in
  let aux1 ppf l = fprintf ppf "[@[<hv 2> (*%a*) %a @]@ ]"
                  f (UnionFind.Poly2.repr (List.hd l) p)
                  (list_sep (f) (fun ppf () -> fprintf ppf " ;@ ")) l in
  let aux2 = list_sep aux1 (fun ppf () -> fprintf ppf " ;@ ") in
  fprintf ppf "UnionFind [@[<hv 2>@ %a @]@ ]" aux2 lst)

let constant_tag ppf c_tag = match c_tag with
  | C_arrow        -> fprintf ppf "C_arrow"
  | C_option       -> fprintf ppf "C_option"
  | C_map          -> fprintf ppf "C_map"
  | C_big_map      -> fprintf ppf "C_big_map"
  | C_list         -> fprintf ppf "C_list"
  | C_set          -> fprintf ppf "C_set"
  | C_unit         -> fprintf ppf "C_unit"
  | C_string       -> fprintf ppf "C_string"
  | C_nat          -> fprintf ppf "C_nat"
  | C_mutez        -> fprintf ppf "C_mutez"
  | C_timestamp    -> fprintf ppf "C_timestamp"
  | C_int          -> fprintf ppf "C_int"
  | C_address      -> fprintf ppf "C_address"
  | C_bytes        -> fprintf ppf "C_bytes"
  | C_key_hash     -> fprintf ppf "C_key_hash"
  | C_key          -> fprintf ppf "C_key"
  | C_signature    -> fprintf ppf "C_signature"
  | C_operation    -> fprintf ppf "C_operation"
  | C_contract     -> fprintf ppf "C_contract"
  | C_chain_id     -> fprintf ppf "C_chain_id"
  | C_bls12_381_g1 -> fprintf ppf "C_bls12_381_g1"
  | C_bls12_381_g2 -> fprintf ppf "C_bls12_381_g2"
  | C_bls12_381_fr -> fprintf ppf "C_bls12_381_fr"

let row_tag ppf = function
    C_record -> fprintf ppf "C_record"
  | C_variant -> fprintf ppf "C_variant"

let rec c_equation ppf {aval; bval} =
  fprintf ppf "{@[<hv 2>@ aval : %a;@ bval : %a;@]@ }"
    type_value aval
    type_value bval

and c_equation_short ppf {aval; bval} =
  fprintf ppf "%a = %a"
    type_value_short aval
    type_value_short bval

and c_typeclass ppf {tc_bound; tc_constraints; tc_args; typeclass=tc;original_id} =
  fprintf ppf "{@[<hv 2>@ tc_bound : %a;@ tc_constraints : %a;@ tc_args : %a;@ typeclass : %a;@ original_id : %s;@]@ }"
    (list_sep_d type_variable) tc_bound
    (list_sep_d type_constraint) tc_constraints
    (list_sep_d type_value) tc_args
    typeclass tc
    (match     original_id with Some (ConstraintIdentifier.T
                                        x) -> Int64.to_string x | None ->"null")

and c_typeclass_short ppf {tc_bound; tc_constraints; tc_args; typeclass=tc;original_id=_} =
  if List.length tc_bound = 0 && List.length tc_constraints = 0 then
    fprintf ppf "(%a) ∈ %a"
      (list_sep_d_short type_value_short) tc_args
      typeclass_short tc
  else
    fprintf ppf "∃ %a, %a => (%a) ∈ %a"
      (list_sep_d_short type_variable) tc_bound
      (list_sep_d_short type_constraint) tc_constraints
      (list_sep_d_short type_value_short) tc_args
      typeclass_short tc

and c_access_label ppf {c_access_label_record_type; accessor; c_access_label_tvar} =
  fprintf ppf "{@[<hv 2>@
              c_access_label_record_type : %a;@
              accessor : %a;@
              c_access_label_tvar : %a;@
              @]}"
    type_value c_access_label_record_type
    label accessor
    type_variable c_access_label_tvar

and c_access_label_short ppf {c_access_label_record_type; accessor; c_access_label_tvar} =
  fprintf ppf "%a.%a = %a"
    type_value_short_ c_access_label_record_type.wrap_content
    label accessor
    type_variable c_access_label_tvar

and c_apply ppf ({ f; arg } : c_apply) =
  fprintf ppf "{@,@[<hv 2>
              f : %a
              arg : %a
              @]@,}"
    type_variable f
    type_variable arg

and c_apply_short ppf ({ f; arg } : c_apply) =
  fprintf ppf "%a(%a)"
    type_variable f
    type_variable arg

and type_constraint_ ppf = function
    C_equation     eq -> fprintf ppf "C_equation (%a)" c_equation eq
  | C_typeclass    tc -> fprintf ppf "C_typeclass (%a)" c_typeclass tc
  | C_access_label al -> fprintf ppf "C_access_label (%a)" c_access_label al
  | C_apply        ap -> fprintf ppf "C_apply (%a)" c_apply ap

and type_constraint_short_ ppf = function
    C_equation     eq -> fprintf ppf "%a" c_equation_short eq
  | C_typeclass    tc -> fprintf ppf "%a" c_typeclass_short tc
  | C_access_label al -> fprintf ppf "%a" c_access_label_short al
  | C_apply        ap -> fprintf ppf "%a" c_apply_short ap

and type_constraint_short ppf {reason=_; c} = fprintf ppf "%a" type_constraint_short_ c
and type_constraint ppf {reason; c} = fprintf ppf "{@[<hv 2>@ reason : %s;@ c : %a;@ @]}" reason type_constraint_ c
and p_constraints ppf const = fprintf ppf "%a" (list_sep_d_short type_constraint) const

and p_constraints_short ppf const = fprintf ppf "%a" (list_sep_d_short type_constraint_short) const

and p_forall ppf {binder;constraints;body} =
  fprintf ppf "{@[<hv 2>@ binder : %a;@ constraints : %a;@ body : %a;@]@ }"
    type_variable binder
    p_constraints constraints
    type_value body

and p_forall_short ppf {binder;constraints;body} =
  fprintf ppf "(∀ %a, %a => %a)"
    type_variable binder
    p_constraints_short constraints
    type_value_short body


and p_constant ppf {p_ctor_tag; p_ctor_args} =
  fprintf ppf "{@[<hv 2>@ p_ctor_tag : %a;@ p_ctor_args : (%a);@]@ }"
    constant_tag p_ctor_tag
    (list_sep_d type_value) p_ctor_args

and p_constant_short ppf {p_ctor_tag; p_ctor_args} =
  match p_ctor_tag, p_ctor_args with
  | Ast_typed__.Ast.C_arrow, [a;b] -> fprintf ppf "%a -> %a" type_value_short a type_value_short b
  | tag, [] -> fprintf ppf "%a" constant_tag tag
  | tag, args -> fprintf ppf "%a(%a)"
                   constant_tag tag
                   (list_sep_d_short type_value_short) args

and p_apply ppf {tf; targ} =
  fprintf ppf "{@[<hv 2>@ tf : %a;@ targ : %a;@]@ }"
    type_value tf
    type_value targ

and p_row ppf {p_row_tag; p_row_args} =
  fprintf ppf "{@[<hv 2>@ p_row_tag : %a;@ p_row_args : %a;@]@ }"
    row_tag p_row_tag
    (lmap_sep_d row_value) @@ LMap.to_kv_list p_row_args

and p_abs ppf {arg; ret} =
  fprintf ppf "{@[<hv 2>@ arg: %a;@ ret: %a;@]@ }"
    type_variable arg
    type_value ret

and p_constraint ppf {pc} =
  fprintf ppf "{@[<hv 2>@ pc: %a;@]@ }"
    type_constraint pc

and p_row_short ppf {p_row_tag; p_row_args} =
  match p_row_tag, LMap.cardinal p_row_args with
    C_record, 0 ->
    fprintf ppf "{ }"
  | C_record, _ ->
    fprintf ppf "{ %a }"
      (lmap_sep_short row_value_short ~sep:" ; " ~assoc:" : ") @@ LMap.to_kv_list p_row_args
  | C_variant, 0 ->
    fprintf ppf "(empty variant)"
  | C_variant, _ ->
    fprintf ppf "%a"
      (lmap_sep_short row_value_short ~sep:" | " ~assoc:" of ") @@ LMap.to_kv_list p_row_args

and row_value : formatter -> row_value -> unit =
  fun ppf { associated_value ; michelson_annotation=_ ; decl_pos } ->
    fprintf ppf "{associated_value %a ; pos %i}"
      type_value associated_value
      decl_pos

and row_value_short : formatter -> row_value -> unit =
  fun ppf { associated_value ; michelson_annotation=_ ; decl_pos=_ } ->
    fprintf ppf "%a"
      type_value_short associated_value


and type_value_ ppf = function
    P_forall    fa -> fprintf ppf "%a" p_forall fa
  | P_variable  tv -> fprintf ppf "%a" type_variable tv
  | P_constant   c -> fprintf ppf "%a" p_constant c
  | P_apply    app -> fprintf ppf "%a" p_apply app
  | P_row        r -> fprintf ppf "%a" p_row r
  | P_abs        a -> fprintf ppf "%a" p_abs a
  | P_constraint c -> fprintf ppf "%a" p_constraint c

and type_value_short_ ppf = function
  | P_constant c  -> fprintf ppf "%a" p_constant_short c
  | P_variable tv -> fprintf ppf "%a" type_variable tv
  | P_forall   fa -> fprintf ppf "%a" p_forall_short fa
  | P_apply   _app -> fprintf ppf "apply"
  | P_row       r -> fprintf ppf "%a" p_row_short r
  | P_abs       _ -> fprintf ppf "abs"
  | P_constraint _ -> fprintf ppf "constraint"

and type_value ppf t =
  fprintf ppf "{@[<hv 2> @ t : %a;@ loc : %a;@]@ }"
    type_value_ t.wrap_content
    Location.pp t.location

and type_value_short ppf t =
  fprintf ppf "%a" type_value_short_ t.wrap_content

and typeclass ppf tc = fprintf ppf "%a" (list_sep_d (list_sep_d type_value)) tc
and typeclass_alowed_short ppf tca = fprintf ppf "(%a)" (list_sep_d_short type_value_short) tca
and typeclass_short ppf tc = fprintf ppf "[%a]" (list_sep_d_short typeclass_alowed_short) tc
let c_constructor_simpl ppf ({id_constructor_simpl = ConstraintIdentifier.T ci; reason_constr_simpl; original_id; tv;c_tag;tv_list} : c_constructor_simpl) =
  fprintf ppf "{@[<hv 2> @ id_constructor_simpl : %Li;@ original_id : %s;@ reason_constr_simpl : %s;@ tv : %a;@ c_tag : %a;@ tv_list : %a;@]@ }"
    ci
    (match original_id with Some (ConstraintIdentifier.T x) -> Format.asprintf "%Li" x | None -> "null")
    reason_constr_simpl
    type_variable tv
    constant_tag c_tag
    (list_sep_d_short type_variable) tv_list

let c_constructor_simpl_short ppf ({id_constructor_simpl = ConstraintIdentifier.T ci; reason_constr_simpl=_; original_id=_; tv;c_tag;tv_list} : c_constructor_simpl) =
  match c_tag, tv_list with
    Ast_typed__.Ast.C_arrow, [a;b] ->
    fprintf ppf "%a ~%a %a -> %a"
      type_variable tv
      constraint_identifier_short ci
      type_variable a
      type_variable b
  | tag, [] ->
    fprintf ppf "%a ~%a %a"
      type_variable tv
      constraint_identifier_short ci
      constant_tag tag
  | tag, args ->
    fprintf ppf "%a ~%a %a(%a)"
      type_variable tv
      constraint_identifier_short ci
      constant_tag tag
      (list_sep_d_short type_variable) args

let c_alias ppf ({reason_alias_simpl;a;b}: c_alias) =
  fprintf ppf "{@[<hv 2> @ reason_alias_simpl : %s;@ a : %a;@ b : %a;@]@ }"
    reason_alias_simpl
    type_variable a
    type_variable b

let c_alias_short ppf ({reason_alias_simpl=_;a;b}: c_alias) =
  fprintf ppf "%a alias %a"
    type_variable a
    type_variable b

let c_poly_simpl ppf ({id_poly_simpl = ConstraintIdentifier.T ci; reason_poly_simpl; original_id; tv; forall}) =
  fprintf ppf "{@[<hv 2> @ id_poly_simpl : %Li;@ original_id : %s;@ reason_poly_simpl : %s;@ tv : %a;@ forall : %a;@]@ }"
    ci
    (match original_id with Some (ConstraintIdentifier.T x) -> Format.asprintf "%Li" x | None -> "null")
    reason_poly_simpl
    type_variable tv
    p_forall forall

let c_poly_simpl_short ppf ({id_poly_simpl = ConstraintIdentifier.T ci; reason_poly_simpl=_; original_id=_; tv; forall}) =
  fprintf ppf "%a ~%a %a"
    type_variable tv
    constraint_identifier_short ci
    p_forall_short forall

let rec c_typeclass_simpl ppf ({ tc_bound ; tc_constraints ; id_typeclass_simpl = ConstraintIdentifier.T ci; reason_typeclass_simpl; original_id; tc; args}) =
  fprintf ppf "{@[<hv 2 >@ tc_bound : %a;@ tc_constraints : %a;@ id_typeclass_simpl : %Li;@ original_id : %s;@ reason_typeclass_simpl : %s;@ tc : %a;@ args : %a;@]@ }"
    (list_sep_d type_variable) tc_bound
    (list_sep_d type_constraint_simpl) tc_constraints
    ci
    (match original_id with Some (ConstraintIdentifier.T x) -> Format.asprintf "%Li" x | None -> "null" )
    reason_typeclass_simpl
    typeclass tc
    (list_sep_d type_variable) args

and c_typeclass_simpl_short ppf ({tc_bound ; tc_constraints ; id_typeclass_simpl = ConstraintIdentifier.T ci; reason_typeclass_simpl=_; original_id=_; tc; args}) =
  if List.length tc_bound = 0 && List.length tc_constraints = 0 then
    fprintf ppf "(%a) ∈%a %a"
      (list_sep_d_short type_variable) args
      constraint_identifier_short ci
      typeclass_short tc
  else
    fprintf ppf "∃ %a, %a => (%a) ∈%a %a"
      (list_sep_d_short type_variable) tc_bound
      (list_sep_d_short type_constraint_simpl_short) tc_constraints
      (list_sep_d_short type_variable) args
      constraint_identifier_short ci
      typeclass_short tc

and constraint_identifier ppf (ConstraintIdentifier.T ci) =
  fprintf ppf "ConstraintIdentifier %Li" ci

and constraint_identifierMap = fun f ppf tvmap   ->
      let lst = RedBlackTrees.PolyMap.bindings tvmap in
      let aux ppf (k, v) =
        fprintf ppf "(%a, %a)" constraint_identifier k f v in
      fprintf ppf "constraint_identifierMap [@ @[<hv 2> %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

and row_variable : formatter -> row_variable -> unit =
  fun ppf { associated_variable ; michelson_annotation=_ ; decl_pos } ->
    fprintf ppf "{associated_variable %a ; pos %i}"
      type_variable associated_variable
      decl_pos
and c_row_simpl ppf ({id_row_simpl = ConstraintIdentifier.T ci; reason_row_simpl; original_id; tv; r_tag; tv_map}) =
  fprintf ppf "{@[<hv 2> @ id_row_simpl : %Li;@ original_id : %s;@ reason_row_simpl : %s;@ tv : %a;@ r_tag : %a;@ tv_map : %a;@]@ }"
    ci
    (match original_id with Some (ConstraintIdentifier.T x) -> Format.asprintf "%Li" x | None -> "null")
    reason_row_simpl
    type_variable tv
    row_tag r_tag
    (lmap_sep_d row_variable) @@ LMap.to_kv_list tv_map

and c_row_simpl_short ppf ({id_row_simpl = ConstraintIdentifier.T ci; reason_row_simpl=_; original_id=_; tv; r_tag; tv_map}) =
  match r_tag with
    C_record ->
    fprintf ppf "%a ~%a { %a }"
      type_variable tv
      constraint_identifier_short ci
      (lmap_sep_short row_variable ~sep:" ; " ~assoc:" : ") @@ LMap.to_kv_list tv_map
  | C_variant ->
    fprintf ppf "%a ~%a %a"
      type_variable tv
      constraint_identifier_short ci
      (lmap_sep_short row_variable ~sep:" | " ~assoc:" of ") @@ LMap.to_kv_list tv_map

and c_access_label_simpl ppf { id_access_label_simpl = ConstraintIdentifier.T ci ; reason_access_label_simpl ; record_type ; label = l ; tv } =
  fprintf ppf "{@,@[<hv 2>
              id_access_label_simpl : %Li; @
              reason_access_label_simpl : %s; @
              record_type : %a ;@
              label : %a
              tv : %a
              @]@,}"
    ci
    reason_access_label_simpl
    type_variable record_type
    label l
    type_variable tv


and c_access_label_simpl_short ppf { id_access_label_simpl = _ ; reason_access_label_simpl = _ ; record_type ; label = l ; tv } =
  fprintf ppf "%a = %a.%a"
    type_variable tv
    type_variable record_type
    label l

and c_apply_simpl ppf { id_apply_simpl = ConstraintIdentifier.T ci ; reason_apply_simpl ; f; arg } =
  fprintf ppf "{@,@[<hv 2>
              id_apply_simpl : %Li; @
              reason_apply_simpl : %s; @
              f : %a
              arg : %a
              @]@,}"
    ci
    reason_apply_simpl
    type_variable f
    type_variable arg

and c_apply_simpl_short ppf { id_apply_simpl = _ ; reason_apply_simpl = _ ; f ; arg } =
  fprintf ppf "%a(%a)"
    type_variable f
    type_variable arg

and c_abs_simpl ppf { id_abs_simpl = ConstraintIdentifier.T ci ; reason_abs_simpl ; tv; param; body } =
  fprintf ppf "{@,@[<hv 2>
              id_access_label_simpl : %Li; @
              reason_access_label_simpl : %s; @
              tv : %a ;@
              param : %a
              body : %a
              @]@,}"
    ci
    reason_abs_simpl
    type_variable tv
    type_variable param
    type_value body

and c_abs_simpl_short ppf { id_abs_simpl = _ ; reason_abs_simpl = _ ; tv; param ; body } =
  fprintf ppf "%a = λ%a, %a"
    type_variable tv
    type_variable param
    type_value body

and type_constraint_simpl ppf (tc: type_constraint_simpl) = match tc with
  | SC_Apply       a -> fprintf ppf "SC_Constructor (%a)" c_apply_simpl a
  | SC_Abs         a -> fprintf ppf "SC_Constructor (%a)" c_abs_simpl a
  | SC_Constructor c -> fprintf ppf "SC_Constructor (%a)" c_constructor_simpl c
  | SC_Alias       a -> fprintf ppf "SC_Alias (%a)" c_alias a
  | SC_Poly        p -> fprintf ppf "SC_Poly (%a)" c_poly_simpl p
  | SC_Typeclass   t -> fprintf ppf "SC_Typeclass (%a)" c_typeclass_simpl t
  | SC_Access_label l -> fprintf ppf "SC_Access_label (%a)" c_access_label_simpl l
  | SC_Row         r -> fprintf ppf "SC_Row (%a)" c_row_simpl r

and type_constraint_simpl_short ppf (tc: type_constraint_simpl) = match tc with
  | SC_Apply       a -> fprintf ppf "SC_Constructor (%a)" c_apply_simpl_short a
  | SC_Abs         a -> fprintf ppf "SC_Constructor (%a)" c_abs_simpl_short a
  | SC_Constructor c -> fprintf ppf "%a" c_constructor_simpl_short c
  | SC_Alias       a -> fprintf ppf "%a" c_alias_short a
  | SC_Poly        p -> fprintf ppf "%a" c_poly_simpl_short p
  | SC_Typeclass   t -> fprintf ppf "%a" c_typeclass_simpl_short t
  | SC_Access_label l -> fprintf ppf "%a" c_access_label_simpl_short l
  | SC_Row         r -> fprintf ppf "%a" c_row_simpl_short r

let constraint_identifier ppf (ConstraintIdentifier.T ci) =
  fprintf ppf "ConstraintIdentifier %Li" ci


(* let structured_dbs ppf ({refined_typeclasses;refined_typeclasses_back;typeclasses_constrained_by;by_constraint_identifier;all_constraints;aliases;assignments;grouped_by_variable;cycle_detection_toposort} : structured_dbs) =
 *   fprintf ppf "{@,@[<hv 2> refined_typeclasses : %a ;@ refined_typeclasses_back : %a ;@ typeclasses_constrained_by : %a ;@ by_constraint_identifier : %a ;@ all_constraints : %a ;@ aliases : %a ;@ assignments : %a;@ gouped_by_variable : %a;@ cycle_detection_toposort : %a @]@,}"
 *     (identifierMap refined_typeclass) refined_typeclasses
 *     (constraint_identifierMap constraint_identifier) refined_typeclasses_back
 *     (typeVariableMap constraint_identifier_set) typeclasses_constrained_by
 *     (identifierMap c_typeclass_simpl) by_constraint_identifier
 *     (list_sep_d type_constraint_simpl) all_constraints
 *     (poly_unionfind type_variable) aliases
 *     (typeVariableMap c_constructor_simpl) assignments
 *     (typeVariableMap constraints) grouped_by_variable
 *     (fun _ppf _ -> ()) cycle_detection_toposort *)

let constructor_or_row ppf (t : constructor_or_row ) =
  match t with
  | `Row r -> c_row_simpl ppf r
  | `Constructor c -> c_constructor_simpl ppf c

let constructor_or_row_short ppf (t : constructor_or_row ) =
  match t with
  | `Row r -> c_row_simpl_short ppf r
  | `Constructor c -> c_constructor_simpl_short ppf c

let axiom ppf = function |HandWaved s -> fprintf ppf "HandWaved %s" s

let proof_trace ppf = function
  Axiom a -> fprintf ppf "Axiom %a" axiom a

let update ppf {remove_constraints;add_constraints;add_constraints_simpl;proof_trace=x} =
  fprintf ppf "{@[<hv 2> @ remove_constraints : %a;@ add_constraints : %a;@ add_constraints_simpl : %a;@ proof_trace : %a;@]@ }"
    (list type_constraint_simpl_short) remove_constraints
    (list type_constraint_short) add_constraints
    (list type_constraint_simpl_short) add_constraints_simpl
    proof_trace x

let updates_list ppf = fprintf ppf "%a" (list (list update))

let environment_element_definition ppf = function
  | ED_binder -> fprintf ppf "Binder"
  | ED_declaration {expression=e;free_variables=fv} ->
    fprintf ppf "Declaration : {expression : %a ;@ free_variables : %a}" expression e (list expression_variable) fv
let rec environment_element ppf ({type_value;definition} : environment_element) =
  fprintf ppf "{@[<hv 2> @ type_value : %a;@ definition : %a;@]@ }"
    type_expression type_value
    environment_element_definition definition


and environment_binding ppf ({expr_var;env_elt} : environment_binding) =
  fprintf ppf "{@[<hv 2> @ expr_var : %a;@ env_elt : %a;@]@ }"
    expression_variable expr_var
    environment_element env_elt

and type_environment_binding ppf ({type_variable=tv;type_} : type_environment_binding) =
  fprintf ppf "{@[<hv 2> @ type_variable : %a;@ type_ : %a;@]@ }"
    type_variable tv
    type_expression type_

and module_environment_binding ppf ({module_variable;module_} : module_environment_binding) =
  fprintf ppf "{@[<hv 2> @ odule_variable : %s ;@ module_ : %a;@]@ }"
    module_variable
    environment module_

and environment ppf ({expression_environment;type_environment=_;module_environment} : environment) =
  fprintf ppf "{@[<hv 2> @ expression_environment : (%a); module_environment : (%a) @]@ }"
    (list_sep_d environment_binding) expression_environment
    (list_sep_d module_environment_binding) module_environment
