(*
  Due to the nature of the pattern matching compilation (Core -> Typed), FAILWITH expression are sometimes generated when
  patterns matching a given expression alternates between variable and constructors.

  Those generated failwith expressions could be avoided but would unnecessarily complicate the pattern matching compiler.

  ```
  match (u1,u2) with
  | Nil , ys  -> 1
  | xs  , Nil -> 2
  | Cons (a,b) , Cons (c,d) -> a + b + c + d
  ```

  being compiled to

  ```
  match u1 with
  | Nil -> 1
  | Cons _ ->
    match u2 with
    | Nil -> 2
    | Cons _ ->
      match u1 with
      | Nil -> failwith "PARTIAL_MATCH"
      | Cons (a,b) ->
        match u2 with
        | Nil -> failwith "PARTIAL_MATCH"
        | Cons (c,d) -> a + b + c + d
  ```
  
  This pass aims to remove those partial match failwiths by identifying matched variable and trying to merge them.
  If failwith expression still remain after this pass, the matching expression is not exhaustive. That will trigger an error

  TODO: It might be desirable to allow non-exhaustive patterns when users wants it
*)

open Errors
let fold_map_expression = Helpers.fold_map_expression
let fold_expression = Helpers.fold_expression
let map_expression = Helpers.map_expression
open Ast_typed
open Trace

type 'a self_res = ('a, self_ast_typed_error) result

let is_generated_partial_match : expression -> bool =
  fun exp ->
    match exp.expression_content with
    | E_constant {cons_name=C_FAILWITH ; arguments=[e]} -> (
      match get_a_string e with
      | Some fw -> String.equal fw (Ligo_string.extract Stage_common.Backends.fw_partial_match)
      | None -> false
    )
    | _ -> false

let rec do_while : (expression -> (bool * expression) self_res) -> expression -> expression self_res =
  fun f exp ->
    let%bind (has_been_simpl, exp) = f exp in
    if has_been_simpl then do_while f exp
    else ok exp

let merge_record_case : (expression_variable * matching_content_record) -> matching_content_record option self_res =
  fun (mvar, {fields=_ ; body ; tv }) ->
    let aux : (expression_variable * type_expression) label_map option -> expression ->
      (bool * (expression_variable * type_expression) label_map option * expression,_) result =
        fun prev exp ->
          let continue = ok (true,prev,exp) in
          let stop new_fields expression_content = ok (false,Some new_fields,{exp with expression_content}) in
          match exp.expression_content with
          | E_matching m -> (
            match get_variable m.matchee with
            | Some v when Var.equal v.wrap_content mvar.wrap_content && Var.is_generated v.wrap_content -> (
              match m.cases with
              | Match_record v -> stop v.fields v.body.expression_content
              | _ -> continue
            )
            | _ -> continue
          )
          | _ -> continue
    in
    let%bind (new_fields_opt , body') = fold_map_expression aux None body in
    match new_fields_opt with
    | Some fields ->
      let new_case : matching_content_record = { fields ; tv ; body=body' } in
      ok (Some new_case)
    | None -> ok None

let merge_variant_case : (expression_variable * matching_content_case) -> matching_content_case option self_res =
  fun (mvar , {constructor;pattern=_;body}) ->
    let aux : expression_variable option -> expression -> (bool * expression_variable option * expression,_) result =
      fun prev exp ->
        let continue = ok (true,prev,exp) in
        let stop new_pattern expression_content = ok (false,Some new_pattern,{exp with expression_content}) in
        match exp.expression_content with
        | E_matching m -> (
          match get_variable m.matchee with
          | Some v when Var.equal v.wrap_content mvar.wrap_content && Var.is_generated v.wrap_content -> (
            match m.cases with
            | Match_variant v -> (
              let (_fw,no_fw) = List.partition (fun (case:matching_content_case) -> is_generated_partial_match case.body) v.cases in
              match no_fw with
              | [] -> fail (corner_case "REMITODO: mmmmmhhhh ?")
              | lst -> (
                let x = List.find_opt (fun ({constructor=c;_}:matching_content_case) -> Compare.label c constructor = 0 ) lst in
                match x with
                | Some x ->
                  let { pattern ; body ; _ } : matching_content_case = x in
                  stop pattern body.expression_content
                | None -> fail (corner_case "REMITODO: NON EXHAU ?")
              )
            )
            | _ -> continue
          )
          | _ -> continue
        )
        | _ -> continue
    in
    let%bind (new_pattern_opt , body') = fold_map_expression aux None body in
    match new_pattern_opt with
    | Some pattern ->
      let new_case : matching_content_case = { constructor ; pattern ; body=body' } in
      ok (Some new_case)
    | None -> ok None

let top_level_simpl : expression -> expression self_res =
  fun exp ->
    let aux : bool -> expression -> (bool * bool * expression) self_res =
      fun has_been_simpl exp ->
        let continue = ok (true,has_been_simpl,exp) in
        let ret continue has_been_simpl expression_content = ok (continue,has_been_simpl,{exp with expression_content}) in
        match exp.expression_content with
        | E_matching m -> (
          match m.matchee.expression_content with
          | E_variable x when Var.is_generated x.wrap_content -> (
            match m.cases with
            | Match_variant v -> (
              let aux : (bool * matching_content_case list) -> matching_content_case -> (bool * matching_content_case list) self_res =
                fun (has_been_simpl,res) case ->
                  let%bind new_case_opt = merge_variant_case (x,case) in
                  match new_case_opt with
                  | Some case -> ok (true,case::res)
                  | None -> ok (has_been_simpl,case::res)
              in
              let%bind (has_been_simpl,cases) = bind_fold_list aux (false,[]) v.cases in
              if has_been_simpl then 
                ret false has_been_simpl (E_matching { m with cases = Match_variant { v with cases} })
              else
                continue
            )
            | Match_record r -> (
              let%bind new_ = merge_record_case (x,r) in
              match new_ with
              | Some r -> ret false true (E_matching { m with cases = Match_record r })
              | None -> continue
            )
            | _ -> continue
          )
          | _ -> continue
        )
        | _ -> continue
    in
    do_while (fold_map_expression aux false) exp

let exhaustiveness_check : expression -> unit self_res =
  fun exp ->
    let aux : unit -> expression -> unit self_res =
      fun () exp ->
        match exp.expression_content with
        | E_matching _ ->
          let contains_partial_match : expression -> expression self_res =
            fun exp' ->
              if is_generated_partial_match exp' then
                let s = Format.asprintf "%a" Location.pp exp.location in
                fail (corner_case @@ "not exhaustive case at "^s)
              else ok exp'
          in
          let%bind _ = map_expression contains_partial_match exp in
          ok ()
        | _ -> ok ()
    in
    fold_expression aux () exp

let peephole_expression exp =
  match exp.expression_content with
  | E_matching _ ->
    let%bind exp' = top_level_simpl exp in
    let%bind () = exhaustiveness_check exp' in
    ok exp'
  | _ -> ok exp