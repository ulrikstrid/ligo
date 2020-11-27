(*
  this function is here to handle all the special cases
  where extra type annotations are needed after earlier passes transformations
*)
open Trace
open Typer_common.Errors
open Stage_common.Constant
module I = Ast_core
module O = Ast_typed
open O.Combinators

let cast_var = Location.map Var.todo_cast

let rec var_belongs : I.expression_variable -> I.expression_variable list -> bool = fun ev lst ->
  match lst with
  | hd::tl -> (Var.equal ev.wrap_content hd.wrap_content) || (var_belongs ev tl)
  | [] -> false

let get_args : I.expression -> I.expression list option = fun exp ->
  match exp.content with
  | E_record x -> Some (I.LMap.to_list x)
  | _ -> None

type type_exp_rec_call = O.environment -> ?tv_opt:O.type_expression -> I.expression -> (O.expression, typer_error) result

(* this function return Some .. if the input application falls into a special case *)
let special_cases :
    O.environment -> type_exp_rec_call -> O.type_expression option -> I.expression -> I.expression I.application -> (O.expression list option, typer_error) result = 
    fun e type_expression' tv_opt ae {lamb ; args} ->
  match lamb.content , get_args args with
  | E_variable v , Some [ f ; collect ; init_record ] when var_belongs v [ ev_list_fold ; ev_map_fold ; ev_set_fold ] -> (
    match f.content with
    | I.E_lambda { binder = { var = lname ; ascr = None } ; output_type = None ; result } ->
      let lname = cast_var lname in
      let%bind (v_col , v_initr ) = bind_map_pair (type_expression' e) (collect , init_record ) in
      let tv_col = get_type_expression v_col   in (* this is the type of the collection  *)
      let tv_out = get_type_expression v_initr in (* this is the output type of the lambda*)
      let%bind input_type = match tv_col.type_content with
        | O.T_constant {language=_ ; injection ; parameters=[t]}
            when String.equal (Ligo_string.extract injection) list_name
              || String.equal (Ligo_string.extract injection) set_name ->
          ok @@ make_t_ez_record (("0",tv_out)::[("1",t)])
        | O.T_constant {language=_ ; injection ; parameters=[k;v]}
          when String.equal (Ligo_string.extract injection) map_name
            || String.equal (Ligo_string.extract injection) big_map_name ->
          ok @@ make_t_ez_record (("0",tv_out)::[("1",make_t_ez_record [("0",k);("1",v)])])
        | _ -> fail @@ bad_collect_loop tv_col ae.location in
      let e' = O.Environment.add_ez_binder lname input_type e in
      let%bind body = type_expression' ?tv_opt:(Some tv_out) e' result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_function input_type output_type ()) in
      let lst' = [lambda'; v_col; v_initr] in
      ok @@ Some lst'
    | _ -> ok None
  )
  | E_variable v , Some [ f ; init_record ] when var_belongs v [ev_fold_while] -> (
    match f.content with
    | I.E_lambda { binder = { var = lname ; ascr = None } ; output_type = None ; result } ->
      let%bind v_initr = type_expression' e init_record in
      let tv_out = get_type_expression v_initr in
      let input_type  = tv_out in
      let lname = cast_var lname in
      let e' = O.Environment.add_ez_binder lname input_type e in
      let%bind body = type_expression' e' result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_function input_type output_type ()) in
      let lst' = [lambda';v_initr] in
      ok @@ Some lst'
    | _ -> ok None
  )
  | E_variable v , Some arguments when var_belongs v [ev_create_contract] -> (
    let%bind lst' = bind_list @@ List.map (type_expression' e) arguments in
    let%bind () = 
      match lst' with
      | { expression_content = O.E_lambda l ; _ } :: _ ->
        let open Ast_typed.Misc in
        let fvs = Free_variables.lambda [] l in
        if List.length fvs = 0 then ok ()
        else fail @@ fvs_in_create_contract_lambda ae (List.hd fvs)
      | _ -> fail @@ create_contract_lambda C_CREATE_CONTRACT ae
    in
    ok @@ Some lst'
  )
  | E_variable v , Some [ key ; set ] when var_belongs v [ev_set_add ; ev_cons] -> (
    let%bind key' =  type_expression' e key in
    let tv_key = get_type_expression key' in
    let tv = match tv_opt with
      | Some tv -> tv
      | None -> match v with
        | v when Var.equal v.wrap_content ev_set_add.wrap_content -> t_set tv_key
        | v when Var.equal v.wrap_content ev_cons.wrap_content -> t_list tv_key
        | _ -> failwith "Only C_SET_ADD and C_CONS are possible because those were the two cases matched above"
    in
    let%bind set' =  type_expression' e ~tv_opt:tv set in
    let lst' = [key' ; set'] in
    ok @@ Some lst'
  )
  | E_variable v , Some [ key ; value ; map ] when var_belongs v [ev_map_add] -> (
    let%bind key' = type_expression' e key in
    let%bind val' = type_expression' e value in
    let tv_key = get_type_expression key' in
    let tv_val = get_type_expression val' in
    let tv = match tv_opt with
        Some tv -> tv
      | None -> t_map_or_big_map tv_key tv_val
    in
    let%bind map' =  type_expression' e ~tv_opt:tv map in
    let lst' = [key';val';map'] in
    ok @@ Some lst'
  )
  | _ -> ok None