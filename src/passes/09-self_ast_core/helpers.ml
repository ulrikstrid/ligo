open Ast_core
open Trace
open Ast_core.Helpers
open Stage_common

include Ast_core.PP

let bind_map_lmap_t f map = bind_lmap (
  LMap.map
    (fun ({associated_type;_} as field) ->
      let* field' = f associated_type in
      ok {field with associated_type = field'})
    map)

type ('a,'err) folder = 'a -> expression -> ('a, 'err) result
let rec fold_expression : ('a, 'err) folder -> 'a -> expression -> ('a,'err) result = fun f init e ->
  let self = fold_expression f in
  let idle = fun acc _ -> ok @@ acc in
  let* init' = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ -> ok init'
  | E_constant c -> Folds.constant self init' c
  | E_application app -> Folds.application self init' app
  | E_lambda l -> Folds.lambda self idle init' l
  | E_ascription a -> Folds.ascription self idle init' a
  | E_constructor c -> Folds.constructor self init' c
  | E_matching {matchee=e; cases} -> (
    let* res = self init' e in
    let aux acc ({body ; _ }: _ Ast_sugar.match_case) = self acc body in
    let* res = bind_fold_list aux res cases in
    ok res
  )
  | E_record m -> Folds.record self init' m
  | E_record_update ru -> Folds.record_update self init' ru
  | E_record_accessor ra -> Folds.record_accessor self init' ra
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let* res = self init' rhs in
      let* res = self res let_result in
      ok res
    )
  | E_type_in ti -> Folds.type_in self idle init' ti
  | E_mod_in  mi ->
    let* res = bind_fold_list (fun acc (x: declaration Location.wrap) -> match x.wrap_content with
      | Declaration_constant dc ->
        let* res = self acc dc.expr in
        ok @@ res
      | _ -> ok @@ acc) init' mi.rhs
    in
    let* res = self res mi.let_result in
    ok @@ res
  | E_mod_alias ma -> Folds.mod_alias self init' ma
  | E_recursive r -> Folds.recursive self idle init' r
  | E_module_accessor { module_name = _ ; element } -> (
    let* res = self init' element in
    ok res
  )

type 'err exp_mapper = expression -> (expression , 'err) result
type 'err ty_exp_mapper = type_expression -> (type_expression , 'err) result
type 'err abs_mapper =
  | Expression of 'err exp_mapper
  | Type_expression of 'err ty_exp_mapper
let rec map_expression : 'err exp_mapper -> expression -> (expression , 'err) result = fun f e ->
  let self = map_expression f in
  let* e' = f e in
  let return expression_content = ok { e' with expression_content } in
  match e'.expression_content with
  | E_ascription ascr -> (
      let* ascr = Maps.ascription self ok ascr in
      return @@ E_ascription ascr
    )
  | E_matching {matchee=e;cases} -> (
    let* e' = self e in
    let aux { pattern ; body } =
      let* body' = self body in
      ok { pattern ; body = body'}
    in
    let* cases' = bind_map_list aux cases in
    return @@ E_matching {matchee=e';cases=cases'}
  )
  | E_record_accessor acc -> (
      let* acc = Maps.record_accessor self acc in
      return @@ E_record_accessor acc
    )
  | E_record m -> (
    let* m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_record_update ru -> (
    let* ru = Maps.record_update self ru in
    return @@ E_record_update ru
  )
  | E_constructor c -> (
      let* c = Maps.constructor self c in
      return @@ E_constructor c
  )
  | E_application app -> (
    let* app = Maps.application self app in
    return @@ E_application app
  )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let* rhs = self rhs in
      let* let_result = self let_result in
      return @@ E_let_in { let_binder ; rhs ; let_result; inline }
    )
  | E_type_in ti -> (
      let* ti = Maps.type_in self ok ti in
      return @@ E_type_in ti
    )
  | E_mod_in  mi ->
    let* rhs = bind_map_list (fun (x: declaration Location.wrap) -> match x.wrap_content with
      | Declaration_constant dc ->
        let* expr = self dc.expr in
        let dc : declaration = Declaration_constant {dc with expr } in
        ok @@ {x with wrap_content=dc}
      | _ -> ok @@ x) mi.rhs
    in
    let* let_result = self mi.let_result in
    return @@ E_mod_in {mi with rhs;let_result}
  | E_mod_alias ma ->
    let* ma = Maps.mod_alias self ma in
    return @@ E_mod_alias ma
  | E_lambda l -> (
      let* l = Maps.lambda self ok l in
      return @@ E_lambda l
    )
  | E_recursive r ->
      let* r = Maps.recursive self ok r in
      return @@ E_recursive r
  | E_constant c -> (
      let* c = Maps.constant self c in
      return @@ E_constant c
    )
  | E_module_accessor { module_name; element } -> (
    let* element = self element in
    return @@ E_module_accessor { module_name; element }
  )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'

and map_type_expression : 'err ty_exp_mapper -> type_expression -> (type_expression , 'err) result =
    fun f te ->
  let self = map_type_expression f in
  let* te' = f te in
  let return type_content = ok { type_content; location=te.location ; sugar = te.sugar } in
  match te'.type_content with
  | T_sum { fields ; layout } ->
    let* fields = bind_map_lmap_t self fields in
    return @@ (T_sum { fields ; layout })
  | T_record {fields ; layout} ->
    let* fields = bind_map_lmap_t self fields in
    return @@ T_record {fields;layout}
  | T_arrow arr ->
    let* arr = Maps.arrow self arr in
    return @@ T_arrow arr
  | T_app a ->
    let* a' = Maps.type_app self a in
    return @@ T_app a'
  | T_variable _ -> ok te'
  | T_module_accessor ma ->
    let* ma = Maps.module_access self ma in
    return @@ T_module_accessor ma
  | T_singleton _ -> ok te'

and map_module : 'err abs_mapper -> module_ -> (module_ , 'err) result = fun m p ->
  let aux = fun (x : declaration) ->
    let return (x:declaration) = ok @@ x in
    match x,m with
    | (Declaration_type dt, Type_expression m') -> (
        let* dt = Maps.declaration_type (map_type_expression m') dt in
        return @@ (Declaration_type dt)
      )
    | (Declaration_constant decl_cst, Expression m') -> (
        let* expr = map_expression m' decl_cst.expr in
        return @@ (Declaration_constant {decl_cst with expr})
      )
    | decl,_ -> ok decl
  (* | Declaration_type of (type_variable * type_expression) *)
  in
  bind_map_list (bind_map_location aux) p

type ('a , 'err) fold_mapper = 'a -> expression -> (bool * 'a * expression , 'err) result
let rec fold_map_expression : ('a , 'err) fold_mapper -> 'a -> expression -> ('a * expression , 'err) result = fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = ok @@ (acc,a) in
  let* (continue, init',e') = f a e in
  if (not continue) then ok(init',e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_ascription ascr -> (
      let* (res,ascr) = Fold_maps.ascription self idle init' ascr in
      ok (res, return @@ E_ascription ascr)
    )
  | E_matching {matchee=e;cases} -> (
      let* (res,e') = self init' e in
      let aux acc { pattern ; body } =
        let* (res,body') = self acc body in
        ok (res,{ pattern ; body = body'})
      in
      let* (res, cases') = bind_fold_map_list aux res cases in
      ok @@ (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record m -> (
    let* (res, m') = Stage_common.Helpers.bind_fold_map_lmap (fun res _ e -> self res e) init' m in
    ok (res, return @@ E_record m')
  )
  | E_record_accessor acc -> (
      let* (res, acc) = Fold_maps.record_accessor self init' acc in
      ok (res, return @@ E_record_accessor acc)
    )
  | E_record_update ru -> (
    let* res,ru = Fold_maps.record_update self init' ru in
    ok (res, return @@ E_record_update ru)
  )
  | E_constructor c -> (
      let* (res,c) = Fold_maps.constructor self init' c in
      ok (res, return @@ E_constructor c)
  )
  | E_application app -> (
      let* res,app = Fold_maps.application self init' app in
      ok (res, return @@ E_application app)
    )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let* (res,rhs) = self init' rhs in
      let* (res,let_result) = self res let_result in
      ok (res, return @@ E_let_in { let_binder ; rhs ; let_result ; inline })
    )
  | E_type_in ti -> (
      let* res,ti = Fold_maps.type_in self idle init' ti in
      ok (res, return @@ E_type_in ti)
    )
  | E_mod_in  mi ->
    let* res,rhs = bind_fold_map_list (fun acc (x: declaration Location.wrap) -> match x.wrap_content with
      | Declaration_constant dc ->
        let* res,expr = self acc dc.expr in
        let dc : declaration = Declaration_constant {dc with expr } in
        ok @@ (res,{x with wrap_content=dc})
      | _ -> ok @@ (acc,x)) init' mi.rhs
    in
    let* res,let_result = self res mi.let_result in
    ok (res, return @@ E_mod_in {mi with rhs;let_result})
  | E_mod_alias ma ->
    let* res,ma = Fold_maps.mod_alias self init' ma in
    ok ( res, return @@ E_mod_alias ma)
  | E_lambda l -> (
      let* res,l = Fold_maps.lambda self idle init' l in
      ok ( res, return @@ E_lambda l)
    )
  | E_recursive r ->
      let* res,r = Fold_maps.recursive self idle init' r in
      ok ( res, return @@ E_recursive r)
  | E_constant c -> (
      let* res,c = Fold_maps.constant self init' c in
      ok (res, return @@ E_constant c)
    )
  | E_module_accessor { module_name; element } -> (
    let* (res,element) = self init' element in
    ok (res, return @@ E_module_accessor { module_name; element })
  )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> ok (init', return e')
