open Errors
open Ast_imperative
open Trace

open Stage_common.Constant

(* TODO: those checks should be moved to the (michelson) backend *)

let peephole_expression : expression -> (expression , self_ast_imperative_error) result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  | E_literal (Literal_key_hash s) as l -> (
    let open Tezos_crypto in
    match Signature.Public_key_hash.of_b58check_opt s with
    | None -> fail (bad_format e)
    | Some _ -> return l
    )
  | E_literal (Literal_address _) as l -> (
    return l
    )
  | E_literal (Literal_signature s) as l -> (
    let open Tezos_crypto in
    match Signature.of_b58check_opt s with
    | None -> fail (bad_format e)
    | Some _ -> return l
    )
  | E_literal (Literal_key s) as l -> (
    let open Tezos_crypto in
    match Signature.Public_key.of_b58check_opt s with
    | None -> fail (bad_format e)
    | Some _ -> return l
    )
  | E_application {lamb; args} as e_const -> (
    let lst = get_e_tuple args.expression_content in
    match get_e_variable lamb.expression_content with
    | Some v when Var.equal v.wrap_content ev_big_map_literal -> (
      let%bind elt =
        trace_option (bad_single_arity v e) @@
          match lst with
          | Some lst -> List.to_singleton lst
          | None -> None
      in
      let%bind lst =
        trace_option (bad_map_param_type v e) @@
          get_e_list elt.expression_content
      in
      let aux = fun (e : expression) ->
        trace_option (bad_map_param_type v e) @@
          Option.(get_e_tuple e.expression_content >>= fun t ->
                  List.to_pair t)
      in
      let%bind pairs = bind_map_list aux lst in
      return @@ E_big_map pairs
    )
    | Some v when Var.equal v.wrap_content ev_map_literal -> (
      let%bind elt =
        trace_option (bad_single_arity v e) @@
          match lst with
          | Some lst -> List.to_singleton lst
          | None -> None
      in
      let%bind lst =
        trace_option (bad_map_param_type v e) @@
          get_e_list elt.expression_content
      in
      let aux = fun (e : expression) ->
        trace_option (bad_map_param_type v e) @@
          Option.(get_e_tuple e.expression_content >>= fun t ->
                  List.to_pair t)
      in
      let%bind pairs = bind_map_list aux lst in
      return @@ E_map pairs
    )

    | Some v when Var.equal v.wrap_content ev_big_map_empty -> (
      let%bind () = match lst with
        | Some [] -> ok ()
        | _ -> fail (bad_empty_arity v e)
      in
      return @@ E_big_map []
    )
    | Some v when Var.equal v.wrap_content ev_map_empty -> (
      let%bind () = match lst with
        | Some [] -> ok ()
        | _ -> fail (bad_empty_arity v e)
      in
      return @@ E_map []
    )

    | Some v when Var.equal v.wrap_content ev_set_literal -> (
      let%bind elt =
        trace_option (bad_single_arity v e) @@
          match lst with
          | Some lst -> List.to_singleton lst
          | None -> None
      in
      let%bind lst =
        trace_option (bad_set_param_type v e) @@
          get_e_list elt.expression_content
      in
      return @@ E_set lst
    )
    | Some v when Var.equal v.wrap_content ev_set_empty -> (
      let%bind () = match lst with
        | Some [] -> ok ()
        | _ -> fail (bad_empty_arity v e)
      in
      return @@ E_set []
    )
    | _ -> return e_const
  )
  | e -> return e
