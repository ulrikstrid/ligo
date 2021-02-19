[@@@warning "-27"]
open Errors
open Trace
open Function

module CST = Cst.Jsligo
module AST = Ast_imperative

open AST

(* type nonrec 'a result = ('a , abs_error) result *)

type nested_match_repr = (*TODO  , move that in AST. (see !909) *)
  | PatternVar of AST.ty_expr binder
  | TupleVar of AST.ty_expr binder * nested_match_repr list
  | RecordVar of AST.ty_expr binder * AST.label list * nested_match_repr list

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map snd tl)

let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst

let get_value : 'a Raw.reg -> 'a = fun x -> x.value

let build_ins = ["Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout"]
  @ ["Michelson"]

open Predefined.Tree_abstraction.Cameligo

let r_split = Location.r_split

let compile_variable var = Location.map Var.of_name @@ Location.lift_region var
let compile_attributes attributes : string list =
  List.map (fst <@ r_split) attributes

module Compile_type = struct


  (*
    `type_compiler_opt` represents an abstractor for a single pattern.
    For instance, you could have a `type_compiler_opt` that is supposed to compile
    only `List(42)`.

    The goal of defining those auxiliary typers is to have a clean compiler function.
    If things are not done this way, then the function will be a big pattern-match with
    a lot of edge cases. Like, "compile type applications this way, except when it's `List`,
    or `sapling_state`, etc.".

    Instead, one can define a `type_compiler_opt` matching their pattern, and then use
    `try_type_compilers`.
  *)

  type type_compiler_opt = CST.type_expr -> (AST.type_expression option, abs_error) result


  let rec type_expression_to_variable : CST.type_expr -> (CST.variable, _) result = function
    | TVar v -> ok v
    | _ -> failwith "Expected a variable in this variant"

  and type_expression_to_constructor : CST.type_expr -> (string * AST.type_expression * attributes, _) result = function
    | TConstr v -> ok (v.value, t_unit () ~loc:(Location.lift v.region), [])
    | TProd {value = {inside = (TString s, rest); _}; region} ->
      let lst = List.map snd rest in
      let%bind lst = bind_map_list compile_type_expression lst in
      let t = t_tuple lst in
      ok @@ (s.value, t, [])
    | _ -> failwith "Expected a variable in this variant"

    (* todo: add `int` singletons to JsLIGO *)
    (* let get_t_int_singleton_opt = function
    *   | CST.TInt x ->
    *     let (_,z) = x.value in
    *     Some z
    *   | _ -> None
    *)
  and get_t_int_singleton_opt = function
  | _ -> None

  and get_t_string_singleton_opt = function
  | CST.TString s -> Some s.value
  | _ -> None

  (*
    This chains the application of multiple `type_compiler_opt`. If the first returns `None`, use
    the next one, etc.
  *)
  and type_compiler_opt_list : type_compiler_opt list -> type_compiler_opt = fun compilers te ->
    match compilers with
    | [] -> ok None
    | hd :: tl -> (
      match%bind hd te with
      | Some x -> ok (Some x)
      | None -> type_compiler_opt_list tl te
    )

  (*
    `try_type_compilers compilers type_expression other` will try to run the `compilers` on
    `type_expression`. If they all return `None`, it will run `other` instead.
  *)
  and try_type_compilers :
    type_compiler_opt list -> CST.type_expr ->
    (unit -> (AST.type_expression, _) result) ->
    (AST.type_expression, _) result =
  fun compilers te other ->
  match%bind type_compiler_opt_list compilers te with
  | Some x -> ok x
  | None -> other ()

  and compile_type_function_args : CST.fun_type_args -> (type_expression, _) result = fun args ->
    let unpar = args.inside in
    let (hd , tl_sep) = unpar in
    let tl = List.map snd tl_sep in
    let aux : CST.fun_type_arg -> (type_expression, _) result = fun x -> compile_type_expression x.type_expr in
    let%bind lst = Trace.bind_map_list aux (hd :: tl) in
    let tpl = t_tuple lst in
    ok tpl

  and compile_sapling : type_compiler_opt = fun te ->
    match te with
    | TApp app -> (
      let ((operator,args), loc) = r_split app in
      match operator.value with
      | "sapling_state" -> (
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let%bind a' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          ok @@ Some (t_sapling_state ~loc singleton)
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      )
      | "sapling_transaction" ->
           let lst = npseq_to_list args.value.inside in
           (match lst with
           | [(a : CST.type_expr)] -> (
             let sloc = Location.lift @@ Raw.type_expr_to_region a in
             let%bind a' =
               trace_option (michelson_type_wrong te operator.value) @@
                 get_t_int_singleton_opt a in
             let singleton = t_singleton ~loc:sloc (Literal_int a') in
             ok @@ Some (t_sapling_transaction ~loc singleton)
             )
           | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | _ -> ok None
    )
    | _ -> ok None

  (* this is a bad design, michelson_or and pair should be an operator
  see AnnotType *)
  and compile_michelson_pair_or : type_compiler_opt = fun te ->
    match te with
    | TApp app -> (
      let ((operator,args), loc) = r_split app in
      match operator.value with
      | "michelson_or" ->
        let lst = npseq_to_list args.value.inside in
        let%bind lst = match lst with
        | [TProd a] -> ok @@ npseq_to_list a.value.inside
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value
        in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let%bind b' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let%bind d' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let%bind a' = compile_type_expression a in
          let%bind c' = compile_type_expression c in
          ok @@ Some (t_michelson_or ~loc a' b' c' d')
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | "michelson_pair" ->
        let lst = npseq_to_list args.value.inside in
        let%bind lst = match lst with
        | [TProd a] -> ok @@ npseq_to_list a.value.inside
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value
        in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let%bind b' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let%bind d' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let%bind a' = compile_type_expression a in
          let%bind c' = compile_type_expression c in
          ok @@ Some (t_michelson_pair ~loc a' b' c' d')
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | _ -> ok None
    )
    | _ -> ok None

and compile_type_expression : CST.type_expr -> (type_expression, _) result =
  fun te ->
  let self = compile_type_expression in
  let return te = ok @@ te in
  (* This is not efficient. It would make more sense to split each type_compiler in their own match branch. *)
  try_type_compilers [
    compile_sapling ;
    compile_michelson_pair_or ;
  ] te @@ fun () ->
  match te with
  | TConstr _constr -> failwith "TODO: parser is weird when there is a single constructor"
  | TSum sum ->
      let sum_type, loc = r_split sum in
      let {variants; attributes; _} : CST.sum_type = sum_type in
      let lst = npseq_to_list variants in
      let attr = compile_attributes attributes in
      let aux (v : CST.type_expr) : (string * type_expression * string list, _) result =
        let%bind (constructor, type_expr, variant_attr) = type_expression_to_constructor v in
        ok @@ (constructor, type_expr, variant_attr) in
      let%bind sum = bind_map_list aux lst
      in return @@ t_sum_ez_attr ~loc ~attr sum
  | TObject record ->
    let injection, loc = r_split record in
    let attributes = compile_attributes injection.attributes in
    let lst = npseq_to_list injection.ne_elements in
    let aux (field : CST.field_decl CST.reg) =
      let f, _ = r_split field in
      let%bind type_expr =
        self f.field_type in
      let field_attr = compile_attributes f.attributes in
      return @@ (f.field_name.value, type_expr, field_attr) in
    let%bind fields = bind_map_list aux lst in
    return @@ t_record_ez_attr ~loc ~attr:attributes fields
  | TProd prod  ->
    let (nsepseq, loc) = r_split prod in
    let lst = npseq_to_list nsepseq.inside in
    let%bind lst = bind_map_list self lst in
    return @@ t_tuple ~loc lst
  | TApp app ->
    let ((operator,args), loc) = r_split app in
    let operators = Var.of_name operator.value in
    let lst = npseq_to_list args.value.inside in
    let%bind lst = bind_map_list self lst in
    return @@ t_app ~loc operators lst
  | TFun func ->
    let ((input_type,_,output_type), loc) = r_split func in
    let%bind input_type = compile_type_function_args input_type in
    let%bind output_type = self output_type in
    return @@ t_function ~loc input_type output_type
  | TPar par ->
    let (par, _) = r_split par in
    let type_expr = par.inside in
    self type_expr
  | TVar var ->
    let (name,loc) = r_split var in
    let v = Var.of_name name in
    return @@ t_variable ~loc v
  | TWild _reg -> failwith "TWild unsupported"
  | TString _s -> fail @@ unsupported_string_singleton te
end

open Compile_type

let expression_to_variable : CST.expr -> (CST.variable, _) result = function
  | EVar var -> ok var
  | EConstr var -> ok var
  | _ as _e ->


    failwith "expected a variable"

let selection_to_variable : CST.selection -> (CST.variable, _) result = function
  | FieldName sfn -> (
      let (sfn , _) = r_split sfn in
      ok sfn.value
    )
  | _ -> failwith "expected a field name"

let compile_expression_to_int : CST.expr -> (z, _) result = function
  | EArith (Int i) -> ok (snd (i.value))
  | _ -> failwith "expected an int in this expression"

let compile_selection : CST.selection -> (_ access * location, _) result = fun selection ->
  match selection with
    FieldName name ->
    let (name, loc) = r_split name in
    ok (Access_record name.value.value, loc)
  | Component comp ->
    let (index_expr, loc) = r_split comp in
    let%bind index = compile_expression_to_int index_expr.inside in
    ok (Access_tuple index, loc)

let compile_expression_constructor : CST.expr -> (AST.expr, _) result = function
  | _ -> failwith "TODO1"

let compile_expression_match : CST.expr -> (AST.expr, _) result = function
  | _ -> failwith "TODO2"

let compile_expression_module_access : CST.expr -> (AST.expr, _) result = function
  | _ -> failwith "TODO3"

let array_item_to_expression : CST.array_item -> (CST.expr, _) result = function
  | Expr_entry expr -> ok expr
  | (Empty_entry _
  | Rest_entry _) as r ->
    fail @@ expected_an_expression r

let get_t_string_singleton_opt = function
| CST.TString s -> Some s.value
| _ -> None


(* let get_t_int_singleton_opt = function
| CST.TInt i -> Some i.value
| _ -> None *)

(* type statement_result =
| Context of (AST.expression -> AST.expression)
| Return of AST.expression *)

type statement_result =
  Binding of (AST.expression -> AST.expression)
| Expr of AST.expression
| Break of AST.expression
| Return of AST.expression

type env = {
  let_vars   : string list;
  const_vars : string list;
  types      : string list;
  in_switch  : bool;
  in_loop    : bool;
}

let env_l = {
  let_vars    = [];
  const_vars  = [];
  types       = [];
  in_switch   = false;
  in_loop     = false;
}

type foo =
  Match_nil of AST.expression
| Match_cons of expression_ Var.t location_wrap * expression_ Var.t location_wrap


let rec compile_tuple_expression ?loc tuple_expr env =
  let%bind lst = bind_map_list (fun e -> compile_expression_in e env) @@ nseq_to_list tuple_expr in
  match lst with
    hd::[] -> ok hd
  | lst -> ok @@ e_tuple ?loc lst

and compile_bin_op (op_type : AST.constant') (op : _ CST.bin_op CST.reg) env =
  let self = compile_expression_in in
  let return e = ok @@ e in
  let (op, loc) = r_split op in
  let%bind a = self op.arg1 env in
  let%bind b = self op.arg2 env in
  return @@ e_constant ~loc (Const op_type) [a; b]

and compile_un_op (op_type : AST.constant') (op : _ CST.un_op CST.reg) env =
  let self = compile_expression_in in
  let return e = ok @@ e in
  let (op, loc) = r_split op in
  let%bind arg = self op.arg env in
  return @@ e_constant ~loc (Const op_type) [arg]

and compile_expression : CST.expr -> (AST.expr, _) result = fun e ->
  compile_expression_in e env_l

and compile_expression_in : CST.expr -> env -> (AST.expr, _) result = fun e env ->
  let self: CST.expr -> env -> (AST.expr, _) result = compile_expression_in in
  let return e = ok @@ e in
  match e with
    EVar var ->
    let (var, loc) = r_split var in
    (match constants var with
      Some const -> return @@ e_constant ~loc const []
    | None -> return @@ e_variable_ez ~loc var
    )
  | EPar par -> self par.value.inside env
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return @@ e_unit ~loc ()
  | EBytes bytes ->
    let (bytes, loc) = r_split bytes in
    let (_s,b) = bytes in
    return @@ e_bytes_hex ~loc b
  | EString str ->(
    match str with
    | String str ->
      let (str, loc) = r_split str in
      return @@ e_string ~loc str
    | Verbatim str ->
      let (str, loc) = r_split str in
      return @@ e_verbatim ~loc str
  )
  | EArith arth ->
    ( match arth with
      Add plus   -> compile_bin_op C_ADD plus env
    | Sub minus  -> compile_bin_op C_SUB minus env
    | Mult times -> compile_bin_op C_MUL times env
    | Div slash  -> compile_bin_op C_DIV slash env
    | Mod mod_   -> compile_bin_op C_MOD mod_ env
    | Neg minus  -> compile_un_op C_NEG minus env
    | Int i ->
      let ((_,i), loc) = r_split i in
      return @@ e_int_z ~loc i
    )
  | ELogic logic -> (
    match logic with
      BoolExpr be -> (
      match be with
        Or or_   -> compile_bin_op C_OR  or_ env
      | And and_ -> compile_bin_op C_AND and_ env
      | Not not_ -> compile_un_op  C_NOT not_ env
      | True  reg -> let loc = Location.lift reg in return @@ e_true  ~loc ()
      | False reg -> let loc = Location.lift reg in return @@ e_false ~loc ()
    )
    | CompExpr ce -> (
      match ce with
        Lt lt    -> compile_bin_op C_LT  lt env
      | Leq le   -> compile_bin_op C_LE  le env
      | Gt gt    -> compile_bin_op C_GT  gt env
      | Geq ge   -> compile_bin_op C_GE  ge env
      | Equal eq -> compile_bin_op C_EQ  eq env
      | Neq ne   -> compile_bin_op C_NEQ ne env
    )
  )
  | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, EObject {value = {inside = fields; _}; _})]); _}; _}); region} ->
    (* Pattern matching for JsLIGO is implemented as a 'built-in function' as
       JavaScript and TypeScript don't have native pattern matching. *)
    let fields' = Utils.nsepseq_to_list fields in
    let compile_simple_pattern p =
      let rec aux = function
        CST.EVar v -> ok @@ Var.of_name v.value
      | EPar par -> aux par.value.inside
      | ESeq {value = (hd, []); _} -> aux hd
      | EAnnot {value = (a, _, _); _} -> aux a
      | EUnit _ -> ok @@ Var.of_name "_"
      | _ -> failwith "improve error message"
      in
      aux p
    in
    let compile_constr_pattern = function
      CST.Property {value = {name = EConstr {value = constr; _}; value; _}; _}
    |     Property {value = {name = EVar    {value = constr; _}; value; _}; _} -> (
        match value with
          EFun {value = {parameters; body; _}; _} ->
            let%bind parameters = compile_simple_pattern parameters in
            let%bind expr = compile_function_body_to_expression body env in
            ok ((Label constr, Location.wrap @@ parameters), expr)
        | _ as e -> fail @@ invalid_case constr e (* TODO: improve error message *)
      )
    | _ -> failwith "todo: write a better error message"
    in
    let loc = Location.lift region in
    let%bind matchee = compile_expression_in input env in
    let%bind constrs = bind_map_list compile_constr_pattern fields' in
    let cases = AST.Match_variant constrs in
    ok @@ e_matching ~loc matchee cases
  | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, ECall {value = EVar {value="list"; _}, Multiple { value = {inside = (CST.EArray args, _); _} ;_} ;_})]); _}; _}); region} ->
    let args = Utils.nsepseq_to_list args.value.inside in
    let compile_simple_pattern p =
      let rec aux = function
        CST.EVar v -> ok @@ Var.of_name v.value
      | EPar par -> aux par.value.inside
      | ESeq {value = (hd, []); _} -> aux hd
      | EAnnot {value = (a, _, _); _} -> aux a
      | EUnit _ -> ok @@ Var.of_name "_"
      | _ -> failwith "improve error message"
      in
      aux p
    in
    let rec compile_parameter = function
      CST.EPar p -> compile_parameter p.value.inside
    | ESeq {value = (EAnnot {value = (EArray {value = {inside = (Empty_entry _, []); _}; _}, _, _); _}, _); _} ->
      ok @@ Match_nil (e_unit ())
    | ESeq {value = (EAnnot {value = (EArray {value = {inside = (Expr_entry hd, [(_, Rest_entry {value = {expr = tl; _}; _})]); _}; _}, _, _); _}, _); _} ->
      let hd_loc = Location.lift @@ Raw.expr_to_region hd in
      let tl_loc = Location.lift @@ Raw.expr_to_region tl in
      let%bind hd = compile_simple_pattern hd in
      let%bind tl = compile_simple_pattern tl in
      let hd = Location.wrap ~loc:hd_loc hd in
      let tl = Location.wrap ~loc:tl_loc tl in
      ok @@ Match_cons (hd, tl)
    | _ -> failwith "write a good error here"
    in
    let compile_case = function
      CST.EFun {value = {parameters; body; _}; _} ->
        let%bind args = compile_parameter parameters in
        let%bind b    = compile_function_body_to_expression body env in
        ok (args, b)
    | _ -> failwith "nope 3"
    in
    (match args with
      [CST.Expr_entry a; CST.Expr_entry b]
    | [CST.Expr_entry a; CST.Expr_entry b; CST.Rest_entry _] ->
        let%bind (params_a, body_a) = compile_case a in
        let%bind (params_b, body_b) = compile_case b in
        (match params_a, params_b, body_a, body_b with
          Match_nil match_nil,  Match_cons (a,b), _, body
        | Match_cons (a,b), Match_nil match_nil, body, _ ->
          (* failwith *)
          let%bind matchee = compile_expression_in input env in
          let loc = Location.lift region in
          ok @@ e_matching ~loc matchee @@ AST.Match_list {match_nil;match_cons = (a,b, body) }
        | _ -> failwith "no"
        )
        (* ok () *)
    | _ -> failwith "no")
    (* in
    ignore(args);
    ignore(compile_case);
    (* let _ = bind_map_list compile_case args in *)
    failwith "TODO: pattern match on lists" *)

  (* This case is due to a bad besign of our constant it as to change
    with the new typer so LIGO-684 on Jira *)
  | ECall {value=(EVar var,args);region} ->
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map snd tl in
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let%bind args = bind_map_list (fun e -> self e env) @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let%bind args = compile_tuple_expression args env in
      return @@ e_application ~loc func args
    )
  (* TODO: modules *)
  (* | ECall {value=(EVar {value={module_name;field};region=_},args);region} when
   *   List.mem module_name.value build_ins ->
   *   let args = match args with
   *     | Unit the_unit -> CST.EUnit the_unit,[]
   *     | Multiple xs ->
   *        let hd,tl = xs.value.inside in
   *        hd,List.map snd tl in
   *   let loc = Location.lift region in
   *   let%bind fun_name = match field with
   *     EVar v -> ok @@ v.value | EModA _ -> fail @@ unknown_constant module_name.value loc
   *     |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|ELetIn _|EFun _|ESeq _|ECodeInj _
   *     |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETypeIn _
   *     |ETuple _|EPar _ -> failwith "Corner case : This couldn't be produce by the parser"
   *   in
   *   let var = module_name.value ^ "." ^ fun_name in
   *   (match constants var with
   *     Some const ->
   *     let%bind args = bind_map_list self @@ nseq_to_list args in
   *     return @@ e_constant ~loc const args
   *   | None ->
   *     fail @@ unknown_constant var loc
   *     ) *)

  | ECall call ->
    let ((func, args), loc) = r_split call in
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map snd tl in
    let%bind func = self func env in
    let%bind args = compile_tuple_expression args env in
    return @@ e_application ~loc func args
  | EArray items ->
    let (items, loc) = r_split items in
    let items = npseq_to_list items.inside in
    (match items with
      [Empty_entry _] -> return @@ e_tuple ~loc []
    | _ ->
      (match List.rev items with
        | Rest_entry _ :: _ ->

          failwith "todo..."


        | _ ->
          let%bind exprs = bind_map_list array_item_to_expression items in
          let%bind exprs' = bind_map_list (fun e -> compile_expression_in e env) exprs in
          return @@ e_tuple ~loc exprs'))
  | EObject {value = {inside = (Property_rest {value = {expr; _}; _}, rest); _}; _} ->
    let%bind record = compile_expression_in expr env in
    let aux up =
      let (_, p) = up in
      match p with
        CST.Punned_property {value = EVar v as evar; region} ->
          let%bind expr = compile_expression_in evar env in
          ok ([Access_record v.value], expr, Location.lift region)
      | Property {value = {name = EVar name; value; _}; region} ->
          let%bind expr = compile_expression_in value env in
          ok ([Access_record name.value], expr, Location.lift region)
      | Property_rest _ -> fail @@ rest_not_supported_here p
      | _ -> fail @@ property_not_supported p
    in
    let%bind updates = bind_map_list aux rest in
    let aux e (path, update, loc) = e_update ~loc e path update in
    return @@ List.fold_left aux record updates
  | EObject obj ->
    let (obj, loc) = r_split obj in
    let aux : CST.property -> (string * expression, _) result = fun fa ->
      match fa with
      | Punned_property prop -> (
          let (prop, loc) = r_split prop in
          let%bind var = expression_to_variable prop in
          ok (var.value , e_variable ~loc (Location.wrap ~loc @@ Var.of_name var.value))
        )
      | Property prop2 -> (
          let (prop2 , _) = r_split prop2 in
          let%bind var = expression_to_variable prop2.name in
          let%bind expr = compile_expression_in prop2.value env in
          ok (var.value , expr)
        )
      | Property_rest _ -> (
          fail @@ rest_not_supported_here fa
        )
    in
    let%bind obj = bind_map_list aux @@ npseq_to_list obj.inside in
    return @@ e_record_ez ~loc obj
  | EProj proj ->
    let (proj, loc) = r_split proj in
    let%bind (selection , _) = compile_selection proj.selection in
    let%bind selected = compile_expression_in proj.expr env in
    return @@ e_accessor ~loc selected [ selection ]
  | EFun func ->
    (* todo : make it in common with let function *)
    let (func, loc) = r_split func in
    let ({parameters; lhs_type; body} : CST.fun_expr) = func in
    let%bind lhs_type = bind_map_option (compile_type_expression <@ snd) lhs_type in
    let%bind (binder,exprs) = compile_parameter parameters in
    let%bind body = compile_function_body_to_expression body env in
    let aux (binder,attr,rhs) expr = e_let_in binder attr rhs expr in
    let expr = List.fold_right aux exprs body  in
    return @@ e_lambda ~loc binder lhs_type expr
  | EConstr constr ->
    let (constr, loc) = r_split constr in
    return @@ e_constructor ~loc constr (e_unit ())
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _ , ty) = annot in
    let%bind expr = self expr env in
    let%bind ty   = compile_type_expression ty in
    return @@ e_annotation ~loc expr ty
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let%bind code = self ci.code env in
    return @@ e_raw_code ~loc language code
  | ESeq seq -> (
    let (seq, loc) = r_split seq in
    let%bind seq = bind_map_list (fun e -> self e env) @@ npseq_to_list seq in
    match seq with
      [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
       [] ->  return @@ prev
      | hd :: tl -> bind (return <@ e_sequence ~loc prev) @@ aux hd tl
      in
      aux hd @@ tl
  )
  | EAssign (e1, _, e2) ->
    (* expression access list *)
    let%bind expr = compile_expression_in e2 env in
    ok @@ e_assign (Location.wrap @@ Var.fresh ()) [Access_tuple Z.zero] expr
  | ENew {value = (_, e); _} -> fail @@ new_not_supported e

and conv : CST.pattern -> (nested_match_repr,_) result =
  fun p ->
  match p with
  | CST.PWild reg ->
    let loc = Location.lift reg in
    let var = Location.wrap ~loc @@ Var.fresh () in
    ok (PatternVar { var ; ascr = None })
  | CST.PVar var ->
    let (var,loc) = r_split var in
    let var = Location.wrap ~loc @@ Var.of_name var in
    ok (PatternVar { var ; ascr = None })
  | CST.PArray tuple ->


    let (tuple, _loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple.inside in
    let patterns = List.Ne.to_list lst in
    let%bind nested = bind_map_list conv patterns in
    let var = Location.wrap @@ Var.fresh () in
    ok (TupleVar ({var ; ascr = None} , nested))
  (* | CST.PObject record ->
    let (inj, _loc) = r_split record in
    let aux : CST.field_pattern CST.reg -> (label * nested_match_repr,_) result = fun field ->
      let { field_name ; eq=_ ; pattern } : CST.field_pattern = field.value in
      let%bind pattern = conv pattern in
      ok (AST.Label field_name.value , pattern)
    in
    let%bind lst = bind_map_ne_list aux @@ npseq_to_ne_list inj.ne_elements in
    let lst = List.Ne.to_list lst in
    let (labels,nested) = List.split lst in
    let var = Location.wrap @@ Var.fresh () in
    ok (RecordVar ({var ; ascr = None}, labels , nested)) *)
  | _ ->
    fail @@ unsupported_pattern_type p

and get_binder : nested_match_repr -> AST.ty_expr binder =
  fun s ->
  match s with
  | TupleVar (x,_) -> x
  | PatternVar x -> x
  | RecordVar (x,_,_) -> x

and fold_nested_z
  f acc l =
  match l with
  | [] -> acc
  | ( PatternVar _ as z ) :: l ->
    let x  = f acc z in
    fold_nested_z f x l
  | (TupleVar (_,inner) as z)::l ->
    let x = f acc z in
    let x = fold_nested_z f x inner in
    fold_nested_z f x l
  | (RecordVar (_,_,inner) as z)::l ->
    let x = f acc z in
    let x = fold_nested_z f x inner in
    fold_nested_z f x l

and nestrec : AST.expression -> (AST.expression -> AST.expression) -> nested_match_repr list -> AST.expression =
  fun res f lst ->
    let aux :  (AST.expression -> AST.expression) -> nested_match_repr -> (AST.expression -> AST.expression) =
      fun f z ->
        match z with
        | PatternVar _ -> f
        | TupleVar (matchee,nested) ->
          let binders = List.map get_binder nested in
          let f' = fun body -> f (e_matching (e_variable matchee.var) (Match_tuple (binders,body))) in
          f'
        | RecordVar (matchee,labels,nested) ->
          let binders = List.map get_binder nested in
          let lbinders = List.combine labels binders in
          let f' = fun body -> f (e_matching (e_variable matchee.var) (Match_record (lbinders,body))) in
          f'
    in
    match lst with
    | PatternVar _ :: tl -> nestrec res f tl
    | TupleVar (matchee,nested) :: tl ->
      let binders = List.map get_binder nested in
      let f' = fun body -> f (e_matching (e_variable matchee.var) (Match_tuple (binders,body))) in
      let f'' = fold_nested_z aux f' nested in
      nestrec res f'' tl
    | RecordVar (matchee,labels,nested) :: tl ->
      let binders = List.map get_binder nested in
      let lbinders = List.combine labels binders in
      let f' = fun body -> f (e_matching (e_variable matchee.var) (Match_record (lbinders,body))) in
      let f'' = fold_nested_z aux f' nested in
      nestrec res f'' tl
    | [] -> f res

and compile_array_let_destructuring : AST.expression -> (CST.pattern, Region.t) Utils.nsepseq CST.brackets Region.reg -> (AST.expression -> AST.expression, _) result =
  fun matchee tuple ->
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple.inside in
    let patterns = List.Ne.to_list lst in
    let%bind patterns = bind_map_list conv patterns in
    let binders = List.map get_binder patterns in
    let f = fun body -> e_matching ~loc matchee (Match_tuple (binders, body)) in
    ok (fun let_result -> nestrec let_result f patterns)

and compile_object_let_destructuring : AST.expression -> (CST.pattern, Region.t) Utils.nsepseq CST.braced CST.reg -> (AST.expression -> AST.expression, _) result =
  fun matchee record ->
    let (record, loc) = r_split record in
    let aux : CST.pattern -> (label * CST.pattern, _) result = fun field ->
      match field with
        PDestruct {value = {property; target; _}; _} ->
          ok @@ (AST.Label property.value, target.value.binders)
      | _ ->
        fail @@ unsupported_pattern_type field
    in
    let%bind lst = bind_map_list aux @@ Utils.nsepseq_to_list record.inside in
    let (labels,patterns) = List.split lst in
    let%bind patterns = bind_map_list conv patterns in
    let binders = List.map get_binder patterns in
    let lbinders = List.combine labels binders in
    let f = fun body -> e_matching ~loc matchee (Match_record (lbinders, body)) in
    ok (fun let_result -> nestrec let_result f patterns)

and compile_parameter : CST.expr ->
    (type_expression binder * (type_expression binder * Types.attributes * expression) list, _) result = fun expr ->
  let return ?ascr loc (exprs: (type_expression binder * Types.attributes * expression) list) var =
    ok ({var=Location.wrap ~loc var; ascr}, exprs) in
  match expr with
  | EPar { value = { inside = ESeq { value = arguments; _ }; _ }; region} ->
    let argument = function
      CST.EAnnot ea ->
        let (ea, loc) = r_split ea in
        let (expr, _, type_expr) : CST.annot_expr = ea in
        let%bind ascr = compile_type_expression type_expr in
        (match expr with
          CST.EVar ev ->
            return ~ascr loc [] @@ Var.of_name ev.value
        | EArray {value = {inside = array_items; _}; _} ->
            let array_item = function
              CST.Empty_entry reg ->
                let loc = Location.lift reg in
                return loc [] @@ Var.fresh ()
            | Expr_entry EVar e ->
                let (var,loc) = r_split e in
                return loc [] @@ Var.of_name var
            | Rest_entry _ -> failwith "w--p"
            | _ -> fail @@ not_a_valid_parameter expr
            in
            let%bind lst = bind_map_ne_list array_item @@ npseq_to_ne_list array_items in
            let (lst,exprs) = List.Ne.split lst in
            let var, ascr, expr = match lst with
              {var;ascr}, [] ->
              Location.unwrap var, ascr, []
            | var, lst ->
              let binder = Var.fresh () in
              let aux (i: Z.t) (b: type_expression binder) =
                Z.add i Z.one,
                (b, [], e_accessor (e_variable @@ Location.wrap ~loc binder) @@ [Access_tuple i])
              in
              binder,
              Option.map (t_tuple ~loc) @@ Option.bind_list @@ List.map (fun e -> e.ascr) @@ var::lst,
              List.fold_map aux Z.zero @@ var :: lst
            in
            let exprs = List.flatten @@ expr :: List.Ne.to_list exprs in
            return ?ascr loc exprs @@ var
        | _ -> fail @@ not_a_valid_parameter expr
        )
    | _ as e -> fail @@ not_a_valid_parameter e
    in
    let%bind lst = bind_map_ne_list argument @@ npseq_to_ne_list arguments in
    let (lst,exprs) = List.Ne.split lst in
    let loc = Location.lift region in
    let var, ascr, expr = match lst with
      {var;ascr}, [] ->
      Location.unwrap var, ascr, []
    | var, lst ->
      let binder = Var.fresh () in
      let aux (i: Z.t) (b: type_expression binder) =
        Z.add i Z.one,
        (b, [], e_accessor (e_variable @@ Location.wrap ~loc binder) @@ [Access_tuple i])
      in
      binder,
      Option.map (t_tuple ~loc) @@ Option.bind_list @@ List.map (fun e -> e.ascr) @@ var::lst,
      List.fold_map aux Z.zero @@ var :: lst
    in
    let exprs = List.flatten @@ expr :: List.Ne.to_list exprs in
    return ?ascr loc exprs @@ var

  | EVar var ->
    let (var,loc) = r_split var in
    return loc [] @@ Var.of_name var
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return ~ascr:(t_unit ~loc ()) loc [] @@ Var.fresh ()
  | _ -> fail @@ not_a_valid_parameter expr


and compile_function_body_to_expression : CST.fun_expr_body -> env -> (AST.expression, _) result = fun body env ->
  match body with
  | FunctionBody statements -> compile_statements_to_expression statements.value.inside env
  | ExpressionBody expr -> compile_expression_in expr env

and compile_let_to_declaration ?(const = false) : CST.let_binding Region.reg -> env -> (AST.declaration, _) result = fun let_binding env ->
  let ({binders; lhs_type; expr = let_rhs; attributes; _} : CST.let_binding) = let_binding.value in
  let%bind lst = compile_let_binding attributes let_rhs lhs_type binders let_binding.region env in
  match lst with
    (name, binder, attr, expr) :: tl -> ok @@ AST.Declaration_constant {name; binder; attr; expr}
  | []                               -> failwith "nope"

(*
  JsLIGO has statements. There are two cases when compiling a statement:
  - A `return` statement are easy: the resulting expression is just the
    content of the return
  - `let` and `const` are subtler. There are no expression corresponding to
    `const x = 42 ;`. The result of compiling this statement is actually the
    function that takes `body` as a parameter and returns `let x = 42 in body`
*)

and merge_statement_results : statement_result -> statement_result -> statement_result = fun f s ->
  match f, s with
    Binding a, Binding b -> Binding (a <@ b)
  | Binding a, Expr    b -> Expr (a b)
  | Binding a, Break   b -> Break (a @@ e_unit ())
  | Binding a, Return  b -> Return (a b)
  | Expr    a, Binding b -> Binding (e_sequence a <@ b )
  | Expr    a, Expr    b -> Expr (e_sequence a b)
  | Expr    a, Break   b -> Break a
  | Expr    a, Return  b -> Return (e_sequence a b)
  | Break   a, _ ->         Break a
  | Return  a, _ ->         Return a

and compile_let_binding: CST.attributes -> CST.expr -> (Region.t * CST.type_expr) option -> CST.pattern -> Region.t -> env -> (('a * type_expression binder * Ast_imperative__.Types.attributes * expression) list, _) result =
  fun attributes let_rhs type_expr binders region env ->
  let attributes = compile_attributes attributes in
  let%bind expr = compile_expression_in let_rhs env in
  let%bind lhs_type =
      bind_map_option (compile_type_expression <@ snd) type_expr in
  let aux = function
  | CST.PVar name -> (*function or const *)
    let fun_binder = compile_variable name in
    ok @@ [(Some name.value, {var=fun_binder;ascr=lhs_type}, attributes, expr)]
  | CST.PArray a ->  (* tuple destructuring (for top-level only) *)
    let (tuple, loc) = r_split a in
    let array_items = npseq_to_list tuple.inside in
    let var = Location.wrap ~loc @@ Var.fresh () in
    let body = e_variable var in
    let aux i binder = (
      let binder: type_expression binder =
        match binder with
          CST.PVar p ->
            {
              var  = Location.wrap (Var.of_name p.value);
              ascr = None
            }
        | _ -> failwith "Not implemented yet..."
      in
      Z.add i Z.one, (None, binder, attributes,
                      e_accessor body @@ [Access_tuple i]))
    in
    ok @@ (None, {var; ascr = None}, [], expr) :: List.fold_map aux Z.zero array_items
  | _ ->
    fail @@ unsupported_pattern_type @@ binders
    (* ok (Some name.value, {var=fun_binder;ascr=lhs_type}, attributes, expr) *)
  in
  aux binders

and compile_statements : CST.statements -> env -> (statement_result * env, _) result = fun statements env ->
  let rec aux env result = function
    (_, hd) :: tl ->
      let%bind (a, env) = compile_statement hd env in
      aux env (merge_statement_results result a) tl
  | [] -> ok (result, env)
  in
  let hd  = fst statements in
  let snd = snd statements in
  let%bind (init, env) = compile_statement hd env in
  aux env init snd

and compile_statement : CST.statement -> env -> (statement_result * env, _) result = fun statement env  ->
  let self = compile_statement in
  let self_expr = compile_expression_in in
  let self_statements = compile_statements in
  let binding e env = ok @@ (Binding (fun f -> e f), env) in
  let expr e env = ok @@ (Expr e, env) in
  let return r env = ok @@ (Return r, env) in
  let in_switch_or_loop = env.in_switch || env.in_loop in
  let compile_initializer ({value = {binders; lhs_type; expr = let_rhs; attributes}; region} : CST.let_binding Region.reg) : (expression -> expression, _) result =
    match binders with
      PArray array -> (
        let%bind matchee = compile_expression_in let_rhs env in
        compile_array_let_destructuring matchee array
      )
    | PObject o ->
      let%bind matchee = compile_expression_in let_rhs env in
      compile_object_let_destructuring matchee o
    | _ ->
      let%bind lst = compile_let_binding attributes let_rhs lhs_type binders region env in
      let aux (_name,binder,attr,rhs) expr = e_let_in ~loc: (Location.lift region) binder attr rhs expr in
      ok @@ List.fold_right aux lst
  in
  let rec initializers (result: expression -> expression) (rem: (Region.t * CST.let_binding Region.reg) list) : (expression -> expression, _) result =
    match rem with
    | (_, hd) :: tl ->
      let%bind init = compile_initializer hd in
      let new_result = result <@ init in
      initializers new_result tl
    | [] -> ok result
  in
  match statement with
  | SExpr e ->
    let%bind e = self_expr e env in
    expr e env
  | SBlock {value = {inside; _}; region} ->
    (* is this a correct block? *)
    let%bind statements = self_statements inside env in
    ok statements
  | SCond cond ->
    let (cond, loc) = r_split cond in
    let%bind test         = self_expr cond.test.inside env in
    let%bind (then_clause, env)  = self cond.ifso env in
    let%bind else_clause = bind_map_option (fun (_, s) -> self s env) cond.ifnot in
    let compile_clause = function
      Binding e -> expr, (e @@ e_unit ())
    | Expr e -> expr, (e_sequence e (e_unit ()))
    | Break b -> return, (e_sequence b (e_unit ()))
    | Return r -> return, r
    in
    let (m, then_clause) = compile_clause then_clause in
    let (m, else_clause, env) = (match else_clause with
        Some (s, env) -> let a, b = compile_clause s in (a, b, env)
      | None -> m, e_unit (), env
    ) in
    m (e_cond ~loc test then_clause else_clause) env
  | SReturn {value = {expr; _}; region} -> (
    match expr with
      Some v ->
        let%bind expr = compile_expression_in v env in
        return expr env
    | None ->
        return (e_unit ~loc:(Location.lift region) ()) env
    )
  | SLet li ->
    (* TODO: ensure assignment can only happen to let values, not const values. *)
    let (li, loc) = r_split li in
    let {bindings; _} : CST.let_ = li in

    let hd = fst bindings in
    let tl = snd bindings in
    let%bind init = compile_initializer hd in
    let%bind initializers' = initializers init tl in
    binding initializers' env
  | SConst li ->
    let (li, loc) = r_split li in
    let {bindings; _} : CST.const_ = li in
    let hd = fst bindings in
    let tl = snd bindings in
    let%bind init = compile_initializer hd in
    let%bind initializers' = initializers init tl in
    binding initializers' env
  | SSwitch switch ->
    let (s, loc) = r_split switch in
    let ({expr = switch_expr; cases; _}: CST.switch) = s in
    let rec compile_cases: CST.expr -> CST.switch_case list -> (statement_result * env, _) result = fun switch_expr -> function
      CST.Switch_case {expr = arg2; statements; _} :: remaining ->
        let eq = ({
          value = {
            op   = Region.ghost;
            arg1 = switch_expr;
            arg2
          };
          region = Region.ghost
        }: CST.equal_cmp CST.bin_op Region.reg) in
        let%bind test = compile_bin_op C_EQ eq env in
        let%bind (then_clause, env) =
          match statements with
          | Some statements -> compile_statements statements {env with in_switch = true}
          | None -> expr (e_unit ()) env
        in
        let has_break_or_return = function
          Break _
        | Return _ -> true
        | _ -> false
        in
        let to_expr = function
          Break b   -> b
        | Return r  -> r
        | Binding b -> b @@ e_unit ()
        | Expr e    -> e
        in
        let then_ = to_expr then_clause in
        let%bind (else_, env) = compile_cases switch_expr remaining in
        let else_ = to_expr else_ in
        if has_break_or_return then_clause then (
          expr (e_cond ~loc test then_ else_) env
        )
        else
          expr (e_sequence ~loc (e_cond ~loc test (to_expr then_clause) (e_unit ~loc ())) else_) env
    | CST.Switch_default_case {statements; _} :: remaining ->
      let eq = ({
        value = {
          op   = Region.ghost;
          arg1 = switch_expr;
          arg2 = switch_expr
        };
        region = Region.ghost
      }: CST.equal_cmp CST.bin_op Region.reg) in
        let%bind test = compile_bin_op C_EQ eq env in
        let%bind (then_clause, env) =
          match statements with
          | Some statements -> compile_statements statements {env with in_switch = true}
          | None -> expr (e_unit ()) env
        in
        let has_break_or_return = function
          Break _
        | Return _ -> true
        | _ -> false
        in
        let to_expr = function
          Break b   -> b
        | Return r  -> r
        | Binding b -> b @@ e_unit ()
        | Expr e    -> e
        in
        let then_ = to_expr then_clause in
        let%bind (else_, env) = compile_cases switch_expr remaining in
        let else_ = to_expr else_ in
        if has_break_or_return then_clause then
          expr (e_cond ~loc test then_ else_) env
        else
          expr (e_sequence ~loc then_ else_) env
    | _ -> expr (e_unit ()) env
    in
    let cases' = Utils.nseq_to_list cases in
    compile_cases switch_expr cases'
  | SBreak b ->
    if not in_switch_or_loop then
      fail @@ not_in_switch_or_loop b
    else
      ok (Break (e_unit ()), env)
  | SType ti ->
    let (ti, loc) = r_split ti in
    let ({name;type_expr;_}: CST.type_decl) = ti in
    let type_binder = Var.of_name name.value in
    let%bind rhs = compile_type_expression type_expr in
    (* let%bind body = compile_expression_in body in *)
    binding (e_type_in ~loc type_binder rhs) env

and compile_statements_to_expression : CST.statements -> env -> (AST.expression, _) result = fun statements env ->
  let%bind (statement_result, env) = compile_statements statements env in
  ok @@ (match statement_result with
    Binding b -> b (e_unit ())
  | Expr e -> e_sequence e (e_unit ())
  | Break r
  | Return r -> r)

[@@@warning "-39"]

let rec compile_statement_to_declaration : CST.statement -> env -> (AST.declaration list, _) result = fun statement env ->
  match statement with
  | SType {value; _} ->
    let name = value.name.value in
    let%bind type_expr = compile_type_expression value.type_expr in
    ok @@ [AST.Declaration_type {type_binder = Var.of_name name; type_expr}]
  | SLet {value = {bindings;_ }; _} -> (
    let bindings = npseq_to_list bindings in
    bind_map_list (fun binding ->
      compile_let_to_declaration binding env
    ) bindings
  )
  | SConst {value = {bindings; _}} -> (
    let bindings = npseq_to_list bindings in
    bind_map_list (fun binding ->
      compile_let_to_declaration binding env
    ) bindings
  )
  | _ ->
    fail @@ statement_not_supported_at_toplevel statement

and compile_statements_to_program : CST.ast -> env -> (AST.module_, _) result = fun ast env ->
  let aux : CST.statement -> (declaration location_wrap list, _) result = fun statement ->
    let%bind declarations = compile_statement_to_declaration statement env in
    ok @@ List.map (fun d ->
      let loc = Location.lift @@ CST.statement_to_region statement in
      Location.wrap ~loc d
    ) declarations
  in
  let statements = nseq_to_list ast.statements in
  let statements = List.map fst statements in
  let%bind declarations = bind_map_list aux statements in
  let lst = List.flatten declarations in
  ok lst

let compile_module : CST.ast -> _ result =
  fun t ->
    compile_statements_to_program t env_l
