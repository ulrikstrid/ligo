open Errors
open Ast_typed.Types
open Trace

type contract_pass_data = {
  contract_type : Helpers.contract_type ;
  main_name : string ;
}

let check_entrypoint_annotation_format ep (exp: expression) =
  match String.split_on_char '%' ep with
    | [ "" ; ep'] ->
      let cap = String.capitalize_ascii ep' in
      if String.equal cap ep' then fail @@ Errors.bad_format_entrypoint_ann ep exp.location
      else ok cap
    | _ -> fail @@ Errors.bad_format_entrypoint_ann ep exp.location 


let self_typing : contract_pass_data -> expression -> (bool * contract_pass_data * expression , self_ast_typed_error) result = fun dat e ->
  let bad_self_err () = Errors.bad_self_type
    e.type_expression 
    {e.type_expression with
      type_content =
        T_constant {
          language=Stage_common.Backends.michelson;
          injection=Ligo_string.verbatim Stage_common.Constant.contract_name;
          parameters=[dat.contract_type.parameter]
        }
    }
    e.location
  in
  match e.expression_content , e.type_expression with
  | (E_constant {cons_name=C_SELF ; arguments=[entrypoint_exp]} , {type_content = T_constant {language=_;injection;parameters=[t]} ; _}) when String.equal (Ligo_string.extract injection) Stage_common.Constant.contract_name ->
    let%bind entrypoint =
      match entrypoint_exp.expression_content with
      | E_literal (Literal_string ep) -> check_entrypoint_annotation_format (Ligo_string.extract ep) entrypoint_exp
      | _ -> fail @@ Errors.entrypoint_annotation_not_literal entrypoint_exp.location
    in
    let%bind entrypoint_t =
      match dat.contract_type.parameter.type_content with
      | (T_sum _ as t) when String.equal "Default" entrypoint -> ok {dat.contract_type.parameter with type_content = t}
      | T_sum cmap ->
        let%bind {associated_type;_} = trace_option (Errors.unmatched_entrypoint entrypoint_exp.location) @@
          LMap.find_opt (Label entrypoint) cmap.content
        in
        ok associated_type
      | t -> ok {dat.contract_type.parameter with type_content = t}
    in
    let%bind () =
      trace_option (bad_self_err ()) @@
      Ast_typed.assert_type_expression_eq (entrypoint_t , t) in
    ok (true, dat, e)
  | _ -> ok (true,dat,e)
