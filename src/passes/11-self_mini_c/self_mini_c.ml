open Mini_c
open Trace

(* TODO hack to specialize map_expression to identity monad *)
let map_expression :
  (expression -> expression) -> (expression -> expression) =
  fun f e ->
  match Helpers.map_expression (fun e -> ok (f e)) e with
  | Ok (e, _) -> e
  | Error _ -> assert false (* impossible *)


(* Conservative purity test: ok to treat pure things as impure, must
   not treat impure things as pure. *)

(* true if the name names a pure constant -- i.e. if uses will be pure
   assuming arguments are pure *)
let is_pure_constant : constant' -> bool =
  function
  | C_UNIT
  | C_CAR | C_CDR | C_PAIR
  | C_NIL | C_CONS
  | C_NEG | C_OR | C_AND | C_XOR | C_NOT
  | C_EQ  | C_NEQ | C_LT | C_LE | C_GT | C_GE
  | C_SOME
  | C_UPDATE | C_MAP_FIND_OPT | C_MAP_ADD | C_MAP_UPDATE
  | C_INT | C_ABS | C_IS_NAT
  | C_BALANCE | C_AMOUNT | C_ADDRESS | C_NOW | C_SOURCE | C_SENDER | C_CHAIN_ID
  | C_SET_MEM | C_SET_ADD | C_SET_REMOVE | C_SLICE
  | C_SHA256 | C_SHA512 | C_BLAKE2b | C_CHECK_SIGNATURE
  | C_HASH_KEY | C_BYTES_PACK | C_CONCAT
    -> true
  (* unfortunately impure: *)
  | C_ADD | C_SUB |C_MUL|C_DIV|C_MOD | C_LSL | C_LSR 
  (* impure: *)
  | C_ASSERTION | C_ASSERT_INFERRED
  | C_MAP_FIND
  | C_FOLD_WHILE
  | C_CALL
  (* TODO... *)
  | _
    -> false

let rec is_pure : expression -> bool = fun e ->
  match e.content with
  | E_literal _
  | E_closure _
  | E_skip
  | E_variable _
  | E_make_empty_map _
  | E_make_empty_big_map _
  | E_make_empty_list _
  | E_make_empty_set _
  | E_make_none _
    -> true

  | E_if_bool (cond, bt, bf)
  | E_if_none (cond, bt, (_, bf))
  | E_if_cons (cond, bt, (_, bf))
  | E_if_left (cond, (_, bt), (_, bf))
    -> List.for_all is_pure [ cond ; bt ; bf ]

  | E_let_in (_, _, e1, e2)
  | E_sequence (e1, e2)
    -> List.for_all is_pure [ e1 ; e2 ]

  | E_constant (c)
    -> is_pure_constant c.cons_name && List.for_all is_pure c.arguments
  | E_record_update (e, _,up)
    -> is_pure e && is_pure up

  (* I'm not sure about these. Maybe can be tested better? *)
  | E_application _
  | E_iterator _
  | E_fold _
    -> false

  (* Could be pure, but, divergence is an effect, so halting problem
     is near... *)
  | E_while _ -> false

let occurs_in : expression_variable -> expression -> bool =
  fun x e ->
  let fvs = Free_variables.expression [] e in
  Free_variables.mem x fvs

let occurs_count : expression_variable -> expression -> int =
  fun x e ->
  let fvs = Free_variables.expression [] e in
  Free_variables.mem_count x fvs

(* Let "inlining" mean transforming the code:

     let x = e1 in e2

   to:

     e2[e1/x]

   (where the latter signifies substituting e1 for x in e2.)

   Things which can go wrong for inlining:

   - If `e1` is not pure, inlining may fail to preserve semantics.
   - Free variables of `e1` may be shadowed in e2, at usages of `x`. This
     is not a problem if the substitution is capture-avoiding.
   - ?
*)

let should_inline : expression_variable -> expression -> bool =
  fun x e ->
  occurs_count x e <= 1

let inline_let : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_let_in ((x, _a), should_inline_here, e1, e2) ->
    if is_pure e1 && (should_inline_here || should_inline x e2)
    then
      let e2' = Subst.subst_expression ~body:e2 ~x:x ~expr:e1 in
      (changed := true ; e2')
    else
      e
  | _ -> e

let inline_lets : bool ref -> expression -> expression =
  fun changed ->
  map_expression (inline_let changed)


(* Let "beta" mean transforming the code:

     (\x. e1) e2

   to:

     let x = e2 in e1

   Things which can go wrong for beta reduction:

   - Nothing?
*)

let beta : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_application ({ content = E_closure { binder = x ; body = e1 } ; type_value = T_function (xtv, tv) }, e2) ->
    (changed := true ;
     Expression.make (E_let_in ((x, xtv), false, e2, e1)) tv)

  (* also do CAR (PAIR x y) ↦ x, or CDR (PAIR x y) ↦ y, only if x and y are pure *)
  | E_constant {cons_name = C_CAR| C_CDR as const; arguments = [ { content = E_constant {cons_name = C_PAIR; arguments = [ e1 ; e2 ]} ; type_value = _ } ]} ->
    if is_pure e1 && is_pure e2
    then (changed := true ;
          match const with
          | C_CAR -> e1
          | C_CDR -> e2
          | _ -> assert false)
    else e
  | _ -> e

let betas : bool ref -> expression -> expression =
  fun changed ->
  map_expression (beta changed)


(* Reduce obviously useless record updates. We will flatten

     { { r with p1 = x } with p1 = y }

   to:

     { r with p1 = y }

   (etc)

   Note that this is only sound when x is pure.
 *)

let flatten_nested_update : bool ref -> expression -> expression =
  fun changed e ->
  (* 'reflect' nested record update expressions, pulling out the leaf
     record expression and a list of updates (in reverse order; first
     update in list is last to take effect) *)
  let rec reflect_updates e =
    match e.content with
    | E_record_update (r , path , expr) ->
      begin
        match reflect_updates r with
        | None -> Some (r, [])
        | Some (r, updates) -> Some (r, (path, expr, e.type_value) :: updates)
      end
    | _ -> None in
  (* remove updates to the same path, keeping only the last one *)
  let rec collapse_updates updates =
    match updates with
    | [] -> []
    | (path, _, _) as update :: updates ->
      let updates = collapse_updates updates in
      let redundant (path', expr, _) = path = path' && is_pure expr in
      let updates = List.filter (fun update -> not (redundant update)) updates in
      update :: updates in
  (* 'reify' a record expression and list of updates back into nested
     record update expressions *)
  let reify_updates (r, updates) =
    List.fold_right
      (fun (path, expr, ty) r ->
         { content = E_record_update (r, path, expr) ;
           type_value = ty })
      updates r in
  match reflect_updates e with
  | None -> e
  | Some (r, updates) ->
    let updates' = collapse_updates updates in
    if List.length updates' < List.length updates
    then (changed := true ;
          reify_updates (r, updates'))
    else e

let flatten_nested_updates : bool ref -> expression -> expression =
  fun changed ->
  map_expression (flatten_nested_update changed)

let contract_check =
  let all = [Michelson_restrictions.self_in_lambdas] in
  let all_e = List.map Helpers.map_sub_level_expression all in
  bind_chain all_e

let rec all_expression : expression -> expression =
  fun e ->
  let changed = ref false in
  let e = inline_lets changed e in
  let e = betas changed e in
  let e = flatten_nested_updates changed e in
  if !changed
  then all_expression e
  else e
