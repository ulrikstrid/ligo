open Mini_c
open Trace

(* Overly conservative for now: ok to treat pure things as impure,
   must not treat impure things as pure. *)
let is_pure : expression -> bool = fun e ->
  match e.content with
  | E_closure _ -> true
  | _ -> false

let rec elim_dead_lambdas : bool ref -> expression -> expression result = fun changed e ->
  let mapper : Helpers.mapper = fun e ->
    match e.content with
    | E_let_in ((x, _), e1, e2) when is_pure e1 ->
      let fvs = Free_variables.expression [] e2 in
      if Free_variables.mem x fvs
      then ok e
      else
        (* pure e1 is not used, eliminate! *)
        (changed := true ; ok e2)
    | _ -> ok e in
  Helpers.map_expression mapper e

(* Overly conservative (I hope) heuristic for whether inlining a
   lambda inside the given expression will be sound.

   We return false whenever the expression contains a let_in or
   assignment which might affect the meaning of either the bound name
   of the lambda, or any of the captured variables in the lambda. *)
let can_inline : var_name -> anon_function -> expression -> bool result = fun f anon e ->
  let fvs = Free_variables.lambda [] anon in
  let is_dangerous : var_name -> bool =
    fun v -> String.equal v f || Free_variables.mem v fvs in
  let can_inline = ref true in
  let%bind _ =
    Helpers.map_expression
      (fun e ->
         match e.content with
         | E_let_in ((v, _), _, _)
         | E_assignment (v, _, _) ->
           if is_dangerous v
           then (can_inline := false ; ok e)
           else ok e
         | _ -> ok e)
      e in
  ok !can_inline

(* A heuristic for whether we should inline a lambda in the given
   expression: if it is applied exactly once. *)
let should_inline : var_name -> expression -> bool result = fun f e ->
  let count = ref 0 in
  let%bind _ =
    Helpers.map_expression
      (fun e ->
         match e.content with
         | E_application ({ content = E_variable v ; type_value = _ }, _) ->
           if String.equal v f
           then (count := !count + 1; ok e)
           else ok e
         | _ -> ok e)
      e in
  ok (!count = 1)

(* Try to inline the given lambda in the given expression. *)
let inline_lambda : bool ref -> var_name -> anon_function -> expression -> expression result = fun changed f anon e ->
  let%bind can = can_inline f anon e in
  let%bind should = should_inline f e in
  if can && should
  then
    let%bind e =
      Helpers.map_expression
        (fun e ->
           match e.content with
           | E_application ({ content = E_variable v; _ }, ({ type_value = args_type; _ } as args))
             when String.equal v f ->
             changed := true ;
             ok { e with content = E_let_in ((anon.binder , args_type), args, anon.body) }
           | _ -> ok e)
        e in
    ok e
  else ok e

(* Try to inline all lambdas in the expression *)
let inline_lambdas : bool ref -> expression -> expression result = fun changed e ->
  let mapper : Helpers.mapper = fun e ->
    match e.content with
    | E_let_in ((f, tv), ({ content = E_closure anon ; _ } as lam), e2) ->
      let%bind e2 = inline_lambda changed f anon e2 in
      ok @@
      let fvs = Free_variables.expression [] e2 in
      if Free_variables.mem f fvs
      then { e with content = E_let_in ((f, tv), lam, e2) }
      else
        (changed := true ; e2)
    | _ -> ok e in
  Helpers.map_expression mapper e

(* Iterate the function on the value until the 'changed' bool ref is
   not set to true, then return the value. *)
let rec fix : (bool ref -> 'a -> 'a result) -> 'a -> 'a result = fun f x ->
  let changed = ref false in
  let%bind x = f changed x in
  if !changed
  then fix f x
  else ok x

let rec all_expression : expression -> expression result =
  fix
    (fun changed e ->
       let%bind e = elim_dead_lambdas changed e in
       let%bind e = inline_lambdas changed e in
       ok e)
