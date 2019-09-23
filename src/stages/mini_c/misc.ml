open Types
open Combinators
open Trace

module Errors = struct

  let missing_entry_point name =
    let title () = "missing entry point" in
    let content () = "no entry point with the given name" in
    let data = [
      ("name" , fun () -> name) ;
    ] in
    error ~data title content

  let not_functional_main name =
    let title () = "not functional main" in
    let content () = "main should be a function" in
    let data = [
      ("name" , fun () -> Format.asprintf "%s" name) ;
    ] in
    error ~data title content

  let not_transpiled_literal exp =
    let title () = "not transpiled literal" in
    let content () = "Expression does not appear to be a transpiled literal value. When using --bigmap you must provide a literal value for the storage." in
    let data =
      [ ("val" , fun () -> Format.asprintf "%a" PP.expression exp) ] in
    error ~data title content
end

(*
   Converts `expr` in `fun () -> expr`.
*)
let functionalize (body : expression) : expression =
  let content = E_literal (D_function { binder = "_" ; body }) in
  let type_value = t_function t_unit body.type_value in
  { content ; type_value }

let get_entry (lst : program) (name : string) : (expression * int) result =
  let%bind entry_expression =
    trace_option (Errors.missing_entry_point name) @@
    let aux x =
      let (((decl_name , decl_expr) , _)) = x in
      if (decl_name = name)
      then Some decl_expr
      else None
    in
    List.find_map aux lst
  in
  let entry_index =
    let aux x =
      let (((decl_name , _) , _)) = x in
      decl_name = name
    in
    List.find_index aux lst
  in
  ok (entry_expression , entry_index)


(*
   Assume the following code:
   ```
     const x = 42
     const y = 120
     const z = 423
     const f = () -> x + y
   ```
   It is transformed in:
   ```
     const f = () ->
       let x = 42 in
       let y = 120 in
       let z = 423 in
       x + y
   ```

   The entry-point can be an expression, which is then functionalized if
   `to_functionalize` is set to true.
*)
let aggregate_entry (lst : program) (name : string) (to_functionalize : bool) : expression result =
  let%bind (entry_expression , entry_index) = get_entry lst name in
  let pre_declarations = List.until entry_index lst in
  let wrapper =
    let aux prec cur =
      let (((name , expr) , _)) = cur in
      e_let_in name expr.type_value expr prec
    in
    fun expr -> List.fold_right' aux expr pre_declarations
  in
  match (entry_expression.content , to_functionalize) with
  | (E_literal (D_function l) , false) -> (
      let l' = { l with body = wrapper l.body } in
      let e' = { entry_expression with content = E_literal (D_function l') } in
      ok e'
    )
  | (E_closure l , false) -> (
      let l' = { l with body = wrapper l.body } in
      let%bind t' =
        let%bind (_ , input_ty , output_ty) = get_t_closure entry_expression.type_value in
        ok (t_function input_ty output_ty)
      in
      let e' = {
        content = E_literal (D_function l') ;
        type_value = t' ;
      } in
      ok e'
    )
  | (_ , true) -> (
      ok @@ functionalize @@ wrapper entry_expression
    )
  | _ -> (
      Format.printf "Not functional: %a\n" PP.expression entry_expression ;
      fail @@ Errors.not_functional_main name
  )

let rec expression_to_value (exp: expression) : value result =
  match exp.content with
  | E_literal v -> ok @@ v
  | E_constant ("PAIR" , fst::snd::[]) ->
     let%bind fstl = expression_to_value fst in
     let%bind sndl = expression_to_value snd in
     ok @@ D_pair (fstl , sndl)
  | E_constant ("UNIT", _) -> ok @@ D_unit
  | E_make_empty_list _ ->
     ok @@ D_list []
  | E_constant ("CONS", [el; els]) ->
     let%bind el = expression_to_value el in
     let%bind els = expression_to_value els in
     begin match els with
     | D_list els -> ok @@ D_list (el :: els)
     | _ -> fail @@ Errors.not_transpiled_literal exp
     end
  | E_constant ("NONE", []) ->
     ok @@ D_none
  | E_constant ("SOME", [x]) ->
     let%bind x = expression_to_value x in
     ok @@ D_some x
  | E_constant ("LEFT", [x]) ->
     let%bind x = expression_to_value x in
     ok @@ D_left x
  | E_constant ("RIGHT", [x]) ->
     let%bind x = expression_to_value x in
     ok @@ D_right x
  | E_make_empty_set _ ->
     ok @@ D_set []
  | E_constant ("SET_ADD", [el; els]) ->
     let%bind el = expression_to_value el in
     let%bind els = expression_to_value els in
     begin match els with
     | D_set els -> ok @@ D_set (el :: els)
     | _ -> fail @@ Errors.not_transpiled_literal exp
     end
  | E_make_empty_map _ ->
     begin
       match exp.type_value with
       | T_big_map _ -> ok @@ D_big_map []
       | T_map _ -> ok @@ D_map []
       | _ -> fail @@ Errors.not_transpiled_literal exp
     end
  | E_constant ("UPDATE", [k;v;prev]) ->
     begin
       match v.content with
       | E_constant ("SOME" , [i]) ->
          let%bind kl = expression_to_value k in
          let%bind il  = expression_to_value i in
          let%bind prevl = expression_to_value prev in
          begin match prevl with
          | D_map kvl -> ok @@ D_map ((kl, il) :: kvl)
          | D_big_map kvl -> ok @@ D_big_map ((kl, il) :: kvl)
          | _ -> fail @@ Errors.not_transpiled_literal exp
          end
       | _ -> fail @@ Errors.not_transpiled_literal exp
     end
  | _ ->
     fail @@ Errors.not_transpiled_literal exp
