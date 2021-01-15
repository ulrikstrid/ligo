[@@@warning "-27"]
open Errors
open Trace
open Function

module CST = Cst.Jsligo
module AST = Ast_imperative

open AST

type nonrec 'a result = ('a , abs_error) result

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

  let type_expression_to_variable : CST.type_expr -> CST.variable result = function
    | TVar v -> ok v
    | _ -> failwith "Expected a variable in this variant"

  let type_expression_to_constructor : CST.type_expr -> CST.constr result = function
    | TConstr v -> ok v
    | _ -> failwith "Expected a variable in this variant"

    (* todo: add `int` singletons to JsLIGO *)
    (* let get_t_int_singleton_opt = function
    *   | CST.TInt x ->
    *     let (_,z) = x.value in
    *     Some z
    *   | _ -> None
    *)
  let get_t_int_singleton_opt = function
  | _ -> None

  let get_t_string_singleton_opt = function
  | CST.TString s -> Some s.value
  | _ -> None

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

  type type_compiler_opt = CST.type_expr -> AST.type_expression option result

  (*
    This chains the application of multiple `type_compiler_opt`. If the first returns `None`, use
    the next one, etc.
  *)
  let rec type_compiler_opt_list : type_compiler_opt list -> type_compiler_opt = fun compilers te ->
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
  let try_type_compilers :
    type_compiler_opt list -> CST.type_expr ->
    (unit -> AST.type_expression result) ->
    AST.type_expression result =
  fun compilers te other ->
  match%bind type_compiler_opt_list compilers te with
  | Some x -> ok x
  | None -> other ()

  let rec compile_type_function_args : CST.fun_type_args -> type_expression result = fun args ->
    let unpar = args.inside in
    let (hd , tl_sep) = unpar in
    let tl = List.map snd tl_sep in
    let aux : CST.fun_type_arg -> type_expression result = fun x -> compile_type_expression x.type_expr in
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

and compile_type_expression : CST.type_expr -> type_expression result =
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
      let aux (v : CST.type_expr) : (string * type_expression * string list) result =
        let%bind constructor = type_expression_to_constructor v in
        let type_expr = t_unit () in
        let variant_attr = [] in
        ok @@ (constructor.value, type_expr, variant_attr) in
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

let expression_to_variable : CST.expr -> CST.variable result = function
  | EVar var -> ok var
  | _ -> failwith "expected a variable"

let selection_to_variable : CST.selection -> CST.variable result = function
  | FieldName sfn -> (
      let (sfn , _) = r_split sfn in
      ok sfn.value
    )
  | _ -> failwith "expected a field name"

let compile_expression_to_int : CST.expr -> z result = function
  | EArith (Int i) -> ok (snd (i.value))
  | _ -> failwith "expected an int in this expression"

let compile_selection : CST.selection -> (_ access * location) result = fun selection ->
  match selection with
    FieldName name ->
    let (name, loc) = r_split name in
    ok (Access_record name.value.value, loc)
  | Component comp ->
    let (index_expr, loc) = r_split comp in
    let%bind index = compile_expression_to_int index_expr.inside in
    ok (Access_tuple index, loc)

let compile_expression_constructor : CST.expr -> AST.expr result = function
  | _ -> failwith "TODO"

let compile_expression_match : CST.expr -> AST.expr result = function
  | _ -> failwith "TODO"

let compile_expression_module_access : CST.expr -> AST.expr result = function
  | _ -> failwith "TODO"

let array_item_to_expression : CST.array_item -> CST.expr result = function
  | Expr_entry expr -> ok expr
  | Empty_entry | Rest_entry _ -> failwith "Expected an expression here"

let get_t_string_singleton_opt = function
| CST.TString s -> Some s.value
| _ -> None

(* let get_t_int_singleton_opt = function
| CST.TInt i -> Some i.value
| _ -> None *)

type statement_result =
| Context of (AST.expression -> AST.expression)
| Return of AST.expression

let rec compile_tuple_expression ?loc tuple_expr =
  let%bind lst = bind_map_list compile_expression @@ nseq_to_list tuple_expr in
  match lst with
    hd::[] -> ok hd
  | lst -> ok @@ e_tuple ?loc lst

and compile_expression : CST.expr -> AST.expr result = fun e ->
  let self = compile_expression in
  let return e = ok @@ e in
  let compile_bin_op (op_type : AST.constant') (op : _ CST.bin_op CST.reg) =
    let (op, loc) = r_split op in
    let%bind a = self op.arg1 in
    let%bind b = self op.arg2 in
    return @@ e_constant ~loc (Const op_type) [a; b]
  in
  let compile_un_op (op_type : AST.constant') (op : _ CST.un_op CST.reg) =
    let (op, loc) = r_split op in
    let%bind arg = self op.arg in
    return @@ e_constant ~loc (Const op_type) [arg]
  in
  match e with
    EVar var ->
    let (var, loc) = r_split var in
    (match constants var with
      Some const -> return @@ e_constant ~loc const []
    | None -> return @@ e_variable_ez ~loc var
    )
  | EPar par -> self par.value.inside
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
      Add plus   -> compile_bin_op C_ADD plus
    | Sub minus  -> compile_bin_op C_SUB minus
    | Mult times -> compile_bin_op C_MUL times
    | Div slash  -> compile_bin_op C_DIV slash
    | Mod mod_   -> compile_bin_op C_MOD mod_
    | Neg minus  -> compile_un_op C_NEG minus
    | Int i ->
      let ((_,i), loc) = r_split i in
      return @@ e_int_z ~loc i
    )
  | ELogic logic -> (
    match logic with
      BoolExpr be -> (
      match be with
        Or or_   -> compile_bin_op C_OR  or_
      | And and_ -> compile_bin_op C_AND and_
      | Not not_ -> compile_un_op  C_NOT not_
      | True  reg -> let loc = Location.lift reg in return @@ e_true  ~loc ()
      | False reg -> let loc = Location.lift reg in return @@ e_false ~loc ()
    )
    | CompExpr ce -> (
      match ce with
        Lt lt    -> compile_bin_op C_LT  lt
      | Leq le   -> compile_bin_op C_LE  le
      | Gt gt    -> compile_bin_op C_GT  gt
      | Geq ge   -> compile_bin_op C_GE  ge
      | Equal eq -> compile_bin_op C_EQ  eq
      | Neq ne   -> compile_bin_op C_NEQ ne
    )
  )
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
      let%bind args = bind_map_list self @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let%bind args = compile_tuple_expression args in
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
    let%bind func = self func in
    let%bind args = compile_tuple_expression args in
    return @@ e_application ~loc func args
  | EArray items ->
    let (items, loc) = r_split items in
    let items = npseq_to_list items.inside in
    let%bind exprs = bind_map_list array_item_to_expression items in
    let%bind exprs' = bind_map_list compile_expression exprs in
    return @@ e_tuple ~loc exprs'
  | EObject obj ->
    let (obj, loc) = r_split obj in
    let aux : CST.property -> (string * expression) result = fun fa ->
      match fa with
      | Punned_property prop -> (
          let (prop, loc) = r_split prop in
          let%bind var = expression_to_variable prop in
          ok (var.value , e_variable ~loc (Location.wrap ~loc @@ Var.of_name var.value))
        )
      | Property prop2 -> (
          let (prop2 , _) = r_split prop2 in
          let%bind var = expression_to_variable prop2.name in
          let%bind expr = compile_expression prop2.value in
          ok (var.value , expr)
        )
      | Property_rest _ -> (
          failwith "No rest"
        )
    in
    let%bind obj = bind_map_list aux @@ npseq_to_list obj.inside in
    return @@ e_record_ez ~loc obj
  | EProj proj ->
    let (proj, loc) = r_split proj in
    let%bind (selection , _) = compile_selection proj.selection in
    let%bind selected = compile_expression proj.expr in
    return @@ e_accessor ~loc selected [ selection ]
  | EFun func ->
    (* todo : make it in common with let function *)
    let (func, loc) = r_split func in
    let ({parameters; lhs_type; body} : CST.fun_expr) = func in
    let%bind lhs_type = bind_map_option (compile_type_expression <@ snd) lhs_type in
    let%bind (binder,exprs) = compile_parameter parameters in
    let%bind body = compile_function_body_to_expression body in
    let aux (binder,attr,rhs) expr = e_let_in binder attr rhs expr in
    let expr = List.fold_right aux exprs body  in
    return @@ e_lambda ~loc binder lhs_type expr
  | EConstr constr ->
    let (constr, loc) = r_split constr in
    return @@ e_constructor ~loc constr (e_unit ())
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _ , ty) = annot in
    let%bind expr = self expr in
    let%bind ty   = compile_type_expression ty in
    return @@ e_annotation ~loc expr ty
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let%bind code = self ci.code in
    return @@ e_raw_code ~loc language code
  | ESeq seq -> (
    let (seq, loc) = r_split seq in
    let%bind seq = bind_map_list self @@ npseq_to_list seq in
    match seq with
      [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
       [] ->  return @@ prev
      | hd :: tl -> bind (return <@ e_sequence ~loc prev) @@ aux hd tl
      in
      aux hd @@ tl
  )
  | EAssign _ -> failwith "TODO"
  | ENew _ -> failwith "new is not supported"

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

and compile_parameter : CST.expr -> _ result = fun expr ->
  let return ?ascr loc exprs var =
    ok ({var=Location.wrap ~loc var; ascr}, exprs) in
  match expr with
  | EVar var ->
    let (var,loc) = r_split var in
    return loc [] @@ Var.of_name var
  | EArray tuple ->
    let (tuple, loc) = r_split tuple in
    let%bind parameters_expr = bind_map_ne_list array_item_to_expression @@ npseq_to_ne_list tuple.inside in
    let%bind lst = bind_map_ne_list compile_parameter parameters_expr in
    let (lst,exprs) = List.Ne.split lst in
    let var, ascr, expr = match lst with
      {var;ascr}, [] ->
      Location.unwrap var, ascr, []
    | var, lst ->
      let binder = Var.fresh () in
      let aux i b =
        Z.add i Z.one,
        (b, [], e_accessor (e_variable @@ Location.wrap ~loc binder) @@ [Access_tuple i])
      in
      binder,
      Option.map (t_tuple ~loc) @@ Option.bind_list @@ List.map (fun e -> e.ascr) @@ var::lst,
      List.fold_map aux Z.zero @@ var :: lst
    in
    let exprs = List.flatten @@ expr :: List.Ne.to_list exprs in
    return ?ascr loc exprs @@ var
  | _ -> failwith "unsupported parameter"

(* and compile_let_to_expression : CST.let_ -> AST.expression result = fun _let_ ->
 *   failwith "TODO" *)

and compile_function_body_to_expression : CST.fun_expr_body -> AST.expression result = function
  | FunctionBody statements -> compile_statements_to_expression statements.value.inside
  | ExpressionBody expr -> compile_expression expr

and compile_let_to_declaration ?(const = false) : CST.let_binding -> AST.declaration result = fun _let_ ->
  failwith "TODO"


(*
  JsLIGO has statements. There are two cases when compiling a statement:
  - A `return` statement are easy: the resulting expression is just the
    content of the return
  - `let` and `const` are subtler. There are no expression corresponding to
    `const x = 42 ;`. The result of compiling this statement is actually the
    function that takes `body` as a parameter and returns `let x = 42 in body`
*)
and compile_previous_statement_to_expression_context :
  CST.statement -> AST.expression -> AST.expression result =
fun _statement ->
  failwith "TODO"

and compile_statement : CST.statement -> statement_result result = fun statement ->
  match statement with
  (* | SVar v -> failwith "TODO: var statement"
  | SExpr e -> (
    let e' = compile_expression e in
    ok @@ Context (
      fun next -> e_sequence e' (e_unit ())
    )
  )
  | SReturn r -> (
    let r' = compile_expression r in
    ok @@ Return r'
  )
  | SLet l -> (

  ) *)
  | _ -> failwith "TODO"

and compile_statements_to_expression : CST.statements -> AST.expression result = fun statements ->
  failwith "TODO"

let rec compile_statement_to_declaration : CST.statement -> AST.declaration result = fun statement ->
  match statement with
  | SLet let_ -> (
    let binding_lst = npseq_to_list let_.value.bindings in
    match binding_lst with
    | [ binding ] -> compile_let_to_declaration binding.value
    (* todo: make `compile_statement_to_declaration` return a `AST.declaration list` instead *)
    | _ -> failwith "let does not support multiple bindings yet"
  )
  | SConst const_ -> (
    let binding_lst = npseq_to_list const_.value.bindings in
    match binding_lst with
    | [ binding ] -> compile_let_to_declaration ~const:true binding.value
    (* todo *)
    | _ -> failwith "const does not support multiple bindings yet"
  )
  | _ ->
    failwith "expected a let or a const"

and compile_statements_to_program : CST.ast -> AST.module_ result = fun ast ->
  let aux : CST.statement -> declaration location_wrap result = fun statement ->
    let%bind declaration = compile_statement_to_declaration statement in
    let loc = Location.lift @@ CST.statement_to_region statement in
    ok (Location.wrap ~loc declaration)
  in
  let stmt = npseq_to_list ast.statements in
  let apply toplevel acc =
    match toplevel with
      CST.TopLevel stmt -> stmt::acc
    | Directive _ -> acc in
  let stmt = List.fold_right apply stmt [] in
  let%bind lst = bind_map_list aux @@ stmt in
  ok lst

let compile_module : CST.ast -> _ result =
  fun t -> failwith "TODO"
