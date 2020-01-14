open Trace
open Mini_c
open Michelson
open Memory_proto_alpha.Protocol.Script_ir_translator
open Operators.Compiler

module Errors = struct
  let corner_case ~loc message =
    let title () = "corner case" in
    let content () = "we don't have a good error message for this case. we are
striving find ways to better report them and find the use-cases that generate
them. please report this to the developers." in
    let data = [
      ("location" , fun () -> loc) ;
      ("message" , fun () -> message) ;
    ] in
    error ~data title content

  let contract_entrypoint_must_be_literal ~loc =
    let title () = "contract entrypoint must be literal" in
    let content () = "For get_entrypoint, entrypoint must be given as a literal string" in
    let data =
      [ ("location", fun () -> loc) ;
      ] in
    error ~data title content

  let error_expression expr =
    let title = thunk "compiling expression" in
    let message = thunk @@ Format.asprintf  "\n- expr: %a\n- type: %a\n" PP.expression expr PP.type_variable expr.type_value in
    error title message

end
open Errors

(* This does not makes sense to me *)
let get_operator : constant -> type_value -> expression list -> predicate result = fun s ty lst ->
  match Operators.Compiler.get_operators s with
  | Ok (x,_) -> ok x
  | Error _ -> (
      match s with
      | C_NONE -> (
          let%bind ty' = Mini_c.get_t_option ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_constant @@ prim ~children:[m_ty] I_NONE

        )
      | C_NIL -> (
          let%bind ty' = Mini_c.get_t_list ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_unary @@ prim ~children:[m_ty] I_NIL
        )
      | C_SET_EMPTY -> (
          let%bind ty' = Mini_c.get_t_set ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_constant @@ prim ~children:[m_ty] I_EMPTY_SET
        )
      | C_BYTES_UNPACK -> (
          let%bind ty' = Mini_c.get_t_option ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_unary @@ prim ~children:[m_ty] I_UNPACK
        )
      | C_MAP_REMOVE ->
          let%bind v = match lst with
            | [ _ ; expr ] ->
                let%bind (_, v) = Mini_c.Combinators.(bind_map_or (get_t_map , get_t_big_map) (Expression.get_type expr)) in
                ok v
            | _ -> simple_fail "mini_c . MAP_REMOVE" in
          let%bind v_ty = Compiler_type.type_ v in
          ok @@ simple_binary @@ seq [dip (i_none v_ty) ; prim I_UPDATE ]
      | C_LEFT ->
          let%bind r = match lst with
            | [ _ ] -> get_t_right ty
            | _ -> simple_fail "mini_c . LEFT" in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ prim ~children:[r_ty] I_LEFT
      | C_RIGHT ->
          let%bind l = match lst with
            | [ _ ] -> get_t_left ty
            | _ -> simple_fail "mini_c . RIGHT" in
          let%bind l_ty = Compiler_type.type_ l in
          ok @@ simple_unary @@ prim ~children:[l_ty] I_RIGHT
      | C_CONTRACT ->
          let%bind r = get_t_contract ty in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ seq [
            prim ~children:[r_ty] I_CONTRACT ;
            i_assert_some_msg (i_push_string "bad address for get_contract") ;
          ]
      | C_CONTRACT_ENTRYPOINT ->
          let%bind r = get_t_contract ty in
          let%bind r_ty = Compiler_type.type_ r in
          let%bind entry = match lst with
            | [ { content = E_literal (D_string entry); type_value = _ } ; _addr ] -> ok entry
            | [ _entry ; _addr ] ->
               fail @@ contract_entrypoint_must_be_literal ~loc:__LOC__
            | _ ->
               fail @@ corner_case ~loc:__LOC__ "mini_c . CONTRACT_ENTRYPOINT" in
          ok @@ simple_binary @@ seq [
            i_drop ; (* drop the entrypoint... *)
            prim ~annot:[entry] ~children:[r_ty] I_CONTRACT ;
            i_assert_some_msg (i_push_string @@ Format.sprintf "bad address for get_entrypoint (%s)" entry) ;
          ]
      | x -> simple_fail (Format.asprintf "predicate \"%a\" doesn't exist" Stage_common.PP.constant x)
    )

type translate_value_t = value -> type_value -> michelson result

let val_bool : bool -> michelson result = fun b -> ok @@ prim (if b then D_True else D_False)
let val_int : int -> michelson result = fun n -> ok @@ int (Z.of_int n)
let val_nat : int -> michelson result = fun n -> ok @@ int (Z.of_int n)
let val_timestamp : int -> michelson result = fun n -> ok @@ int (Z.of_int n) 
let val_mutez : int -> michelson result = fun n -> ok @@ int (Z.of_int n)
let val_string : string -> michelson result = fun s -> ok @@ string s
let val_bytes : bytes -> michelson result = fun s -> ok @@ bytes s
let val_unit : unit -> michelson result = fun () -> ok @@ prim D_Unit
let val_none : unit -> michelson result = fun () -> ok @@ prim D_None
let val_operation : unit -> michelson result = fun () -> simple_fail "can't compile an operation"

let val_pair : translate_value_t -> value * value -> type_value -> michelson result =
  fun translate_value (a,b) ty -> 
    let%bind (a_ty , b_ty) = get_t_pair ty in
    let%bind a = translate_value a a_ty in
    let%bind b = translate_value b b_ty in
    ok @@ prim ~children:[a;b] D_Pair

let val_left : translate_value_t -> value -> type_value -> michelson result =
  fun translate_value a ty ->
    let%bind (a_ty , _) = get_t_or ty in
    let%bind a' = translate_value a a_ty in
    ok @@ prim ~children:[a'] D_Left

let val_right : translate_value_t -> value -> type_value -> michelson result =
  fun translate_value b ty ->
    let%bind (_ , b_ty) = get_t_or ty in
    let%bind b' = translate_value b b_ty in
    ok @@ prim ~children:[b'] D_Right

let val_some : translate_value_t -> value -> type_value -> michelson result =
  fun translate_value s ty ->
    let%bind s' = translate_value s ty in
    ok @@ prim ~children:[s'] D_Some

let val_map : translate_value_t -> (value * value) list -> type_value -> michelson result =
  fun translate_value lst ty ->
    let%bind (k_ty , v_ty) = get_t_map ty in
    let%bind lst' =
      let aux (k , v) = bind_pair (translate_value k k_ty , translate_value v v_ty) in
      bind_map_list aux lst in
    let sorted = List.sort (fun (x , _) (y , _) -> compare x y) lst' in
    let aux (a, b) = prim ~children:[a;b] D_Elt in
    ok @@ seq @@ List.map aux sorted

let val_big_map : translate_value_t -> (value * value) list -> type_value -> michelson result =
  fun translate_value lst ty ->
    let%bind (k_ty , v_ty) = get_t_big_map ty in
    let%bind lst' =
      let aux (k , v) = bind_pair (translate_value k k_ty , translate_value v v_ty) in
      bind_map_list aux lst in
    let sorted = List.sort (fun (x , _) (y , _) -> compare x y) lst' in
    let aux (a, b) = prim ~children:[a;b] D_Elt in
    ok @@ seq @@ List.map aux sorted

let val_list : translate_value_t -> value list -> type_value -> michelson result =
  fun translate_value lst ty ->
      let%bind e_ty = get_t_list ty in
      let%bind lst' = bind_map_list (fun x -> translate_value x e_ty) lst in
      ok @@ seq lst'

let val_set : translate_value_t -> value list -> type_value -> michelson result = 
  fun translate_value lst ty ->
    let%bind e_ty = get_t_set ty in
    let%bind lst' = bind_map_list (fun x -> translate_value x e_ty) lst in
    let sorted = List.sort compare lst' in
    ok @@ seq sorted

type translate_expression_t = expression -> environment -> michelson result

let translate_function_body_ (translate_expression: translate_expression_t) ({body ; binder} : anon_function) lst input : michelson result =
  let pre_env = Environment.of_list lst in
  let env = Environment.(add (binder , input) pre_env) in
  let%bind expr_code = translate_expression body env in
  let%bind unpack_closure_code = Compiler_environment.unpack_closure pre_env in
  ok @@ seq [
      i_comment "unpack closure env" ;
      unpack_closure_code ;
      i_comment "function result" ;
      expr_code ;
      i_comment "remove env" ;
      dip i_drop ;
      seq (List.map (Function.constant (dip i_drop)) lst) ;
    ]

let translate_function (translate_expression: translate_expression_t) anon env input_ty output_ty : michelson result =
  let fvs = Mini_c.Free_variables.lambda [] anon in
  let small_env = Mini_c.Environment.select fvs env in
  let%bind lambda_ty = Compiler_type.lambda_closure (small_env , input_ty , output_ty) in
  let%bind lambda_body_code = translate_function_body_ translate_expression anon small_env input_ty in
  match fvs with
  | [] -> ok @@ seq [ i_push lambda_ty lambda_body_code ]
  | _ :: _ ->
    let selector = List.map fst small_env in
    let%bind closure_pack_code = Compiler_environment.pack_closure env selector in
    ok @@ seq [
      closure_pack_code ;
      i_push lambda_ty lambda_body_code ;
      i_swap ;
      i_apply ;
    ]

let exp_skip : unit -> michelson result = fun () -> ok @@ i_push_unit

let exp_literal : translate_value_t -> value -> type_value -> michelson result =
  fun translate_value v ty ->
    let%bind v = translate_value v ty in
    let%bind t = Compiler_type.type_ ty in
    ok @@ i_push t v

let exp_closure : translate_expression_t -> anon_function ->
                  environment -> type_value -> michelson result =
  fun translate_expression anon env ty ->
    match ty with
    | T_function (input_ty , output_ty) ->
      translate_function translate_expression anon env input_ty output_ty
    | _ -> simple_fail "expected function type"

let exp_application : translate_expression_t -> (expression * expression) ->
                      environment -> michelson result =
  fun translate_expression (f,arg) env ->
    trace (simple_error "Compiling quote application") @@
    let%bind f = translate_expression f env in
    let%bind arg = translate_expression arg env in
    ok @@ seq [
      arg ;
      dip f ;
      prim I_EXEC ;
    ]

let exp_variable : var_name -> environment -> michelson result =
  fun x env -> Compiler_environment.get env x

let exp_sequence : translate_expression_t -> (expression * expression) ->
                   environment -> michelson result =
  fun translate_expression (a,b) env ->
    let%bind a' = translate_expression a env in
    let%bind b' = translate_expression b env in
    ok @@ seq [
      a' ;
      i_drop ;
      b' ;
    ]

let exp_constant : translate_expression_t -> (constant * expression list) ->
                   environment -> type_value -> michelson result =
  fun translate_expression (str,lst) env ty ->
    let module L = Logger.Stateful() in
    let%bind pre_code =
      let aux code expr =
        let%bind expr_code = translate_expression expr env in
        L.log @@ Format.asprintf "\n%a -> %a in %a\n"
          PP.expression expr
          Michelson.pp expr_code
          PP.environment env ;
        ok (seq [ expr_code ; dip code ]) in
      bind_fold_right_list aux (seq []) lst in
    let%bind predicate = get_operator str ty lst in
    let%bind code = match (predicate, List.length lst) with
      | Constant c, 0 -> ok @@ seq [
          pre_code ;
          c ;
        ]
      | Unary f, 1 -> ok @@ seq [
          pre_code ;
          f ;
        ]
      | Binary f, 2 -> ok @@ seq [
          pre_code ;
          f ;
        ]
      | Ternary f, 3 -> ok @@ seq [
          pre_code ;
          f ;
        ]
      | _ -> simple_fail (Format.asprintf "bad arity for %a"  Stage_common.PP.constant str)
    in
    let error =
      let title () = "error compiling constant" in
      let content () = L.get () in
      error title content in
    trace error @@ ok code

let exp_make_empty_map : (type_value * type_value) ->  michelson result = fun sd ->
  let%bind (src, dst) = bind_map_pair Compiler_type.type_ sd in
  ok @@ i_empty_map src dst

let exp_make_empty_big_map : (type_value * type_value) ->  michelson result = fun sd ->
  let%bind (src, dst) = bind_map_pair Compiler_type.type_ sd in
  ok @@ i_empty_big_map src dst

let exp_make_empty_list : type_value ->  michelson result = fun t ->
  let%bind t' = Compiler_type.type_ t in
  ok @@ i_nil t'

let exp_make_empty_set : type_value ->  michelson result = fun t ->
  let%bind t' = Compiler_type.type_ t in
  ok @@ i_empty_set t'

let exp_make_none : type_value ->  michelson result = fun o ->
  let%bind o' = Compiler_type.type_ o in
  ok @@ i_none o'

let exp_if_bool : translate_expression_t -> (expression * expression * expression) ->
                  environment -> michelson result =
  fun translate_expression (c , a , b) env ->
    let%bind c' = translate_expression c env in
    let%bind a' = translate_expression a env in
    let%bind b' = translate_expression b env in
    ok @@ seq [
        c' ;
        i_if a' b' ;
      ]

let exp_if_none : translate_expression_t ->
                  expression * expression * ((var_name * type_value) * expression) ->
                  environment -> michelson result =
  fun translate_expression (c, n, (ntv, s)) env ->
    let%bind c' = translate_expression c env in
    let%bind n' = translate_expression n env in
    let s_env = Environment.add ntv env in
    let%bind s' = translate_expression s s_env in
    ok @@ seq [
        c' ;
        i_if_none n' (seq [
            s' ;
            dip i_drop ;
          ]) ;
      ]

let exp_if_cons : translate_expression_t ->
                  expression * expression * (((var_name * type_value) * (var_name * type_value)) * expression) ->
                  environment -> michelson result =
  fun translate_expression (cond , nil , ((hd , tl) , cons)) env ->
    let%bind cond' = translate_expression cond env in
    let%bind nil' = translate_expression nil env in
    let s_env =
      Environment.add hd
      @@ Environment.add tl env
    in
    let%bind s' = translate_expression cons s_env in
    ok @@ seq [
        cond' ;
        i_if_cons (seq [
            s' ;
            dip (seq [ i_drop ; i_drop ]) ;
          ]) nil'
        ;
      ]

let exp_if_left : translate_expression_t ->
                  expression * ((var_name * type_value) * expression) * ((var_name * type_value) * expression) ->
                  environment -> michelson result =
  fun translate_expression (c, (l_ntv , l), (r_ntv , r)) env ->
    let%bind c' = translate_expression c env in
    let l_env = Environment.add l_ntv env in
    let%bind l' = translate_expression l l_env in
    let r_env = Environment.add r_ntv env in
    let%bind r' = translate_expression r r_env in
    ok @@ (seq [
        c' ;
        i_if_left (seq [
            l' ;
            i_comment "restrict left" ;
            dip i_drop ;
          ]) (seq [
            r' ;
            i_comment "restrict right" ;
            dip i_drop ;
          ])
        ;
      ])

let exp_let_in : translate_expression_t ->
                 (var_name * type_value) * expression * expression ->
                 environment -> michelson result =
  fun translate_expression (v, expr, body) env ->
    let%bind expr' = translate_expression expr env in
    let%bind body' = translate_expression body (Environment.add v env) in
    ok @@ seq [
        expr' ;
        body' ;
        i_comment "restrict let" ;
        dip i_drop ;
      ]

let exp_iterator : translate_expression_t ->
                   constant * ((var_name * type_value) * expression) * expression ->
                   environment -> michelson result =
  fun translate_expression (name, (v, body), expr) env ->
    let%bind expr' = translate_expression expr env in
    let%bind body' = translate_expression body (Environment.add v env) in
    match name with
    | C_ITER -> (
        ok @@ seq [
            expr' ;
            i_iter (seq [body' ; i_drop ; i_drop]) ;
            i_push_unit ;
          ] )
    | C_MAP -> (
        ok @@ seq [
            expr' ;
            i_map (seq [body' ; dip i_drop]) ;
          ] )
    | s ->
      let iter = Format.asprintf "iter %a" Stage_common.PP.constant s in
      let error = error (thunk "bad iterator") (thunk iter) in
      fail error

let exp_fold : translate_expression_t ->
               ((var_name * type_value) * expression) * expression * expression ->
               environment -> michelson result =
  fun translate_expression ((v , body) , collection , initial) env ->
    let%bind collection' = translate_expression collection env in
    let%bind initial' = translate_expression initial env in
    let%bind body' = translate_expression body (Environment.add v env) in
    ok @@ seq [
        collection' ;
        dip initial' ;
        i_iter (seq [
            i_swap ;
            i_pair ; body' ; dip i_drop ;
          ]) ;
    ]

let exp_assignment : translate_expression_t ->
                     var_name * [ `Right | `Left ] list * expression ->
                     environment -> michelson result =
  fun translate_expression (name , lrs , expr) env ->
    let%bind expr' = translate_expression expr env in
    let%bind get_code = Compiler_environment.get env name in
    let modify_code =
      let aux acc step = match step with
        | `Left -> seq [dip i_unpair ; acc ; i_pair]
        | `Right -> seq [dip i_unpiar ; acc ; i_piar]
      in
      let init = dip i_drop in
      List.fold_right' aux init lrs
    in
    let%bind set_code = Compiler_environment.set env name in
    let error =
      let title () = "michelson type-checking patch" in
      let content () =
        let aux ppf = function
          | `Left -> Format.fprintf ppf "left"
          | `Right -> Format.fprintf ppf "right" in
        Format.asprintf "Sub path: %a\n"
          PP_helpers.(list_sep aux (const " , ")) lrs
      in
      error title content in
    trace error @@
    ok @@ seq [
      i_comment "assign: start # env" ;
      expr' ;
      i_comment "assign: compute rhs # rhs : env" ;
      dip get_code ;
      i_comment "assign: get name # rhs : name : env" ;
      modify_code ;
      i_comment "assign: modify code # name+rhs : env" ;
      set_code ;
      i_comment "assign: set new # new_env" ;
      i_push_unit ;
    ]

let exp_update : translate_expression_t ->
                 expression * ([ `Right | `Left ] list * expression) list ->
                 environment -> michelson result =
  fun translate_expression (record, updates) env ->
    let%bind record' = translate_expression record env in
    let insts = [
      i_comment "r_update: start, move the record on top # env";
      record';] in 
    let aux (init :t list) (update,expr) = 
      let record_var = Var.fresh () in
      let env' = Environment.add (record_var, record.type_value) env in
      let%bind expr' = translate_expression expr env' in
      let modify_code =
        let aux acc step = match step with
          | `Left -> seq [dip i_unpair ; acc ; i_pair]
          | `Right -> seq [dip i_unpiar ; acc ; i_piar]
        in
        let init = dip i_drop in
        List.fold_right' aux init update
      in
      ok @@ init @ [
        expr';
        i_comment "r_updates : compute rhs # rhs:env";
        modify_code;
        i_comment "r_update: modify code # record+rhs : env";
        ]
    in
    let%bind insts = bind_fold_list aux insts updates in
    ok @@ seq insts

let exp_while : translate_expression_t ->
                expression * expression -> environment ->
                michelson result =
  fun translate_expression (expr, block) env ->
    let%bind expr' = translate_expression expr env in
    let%bind block' = translate_expression block env in
    ok @@ seq [
      expr' ;
      prim ~children:[seq [
          block' ;
          i_drop ;
          expr']] I_LOOP ;
      i_push_unit ;
    ]

type compiled_expression = {
  expr_ty : ex_ty ;
  expr : michelson ;
}

let rec translate_value (v:value) ty : michelson result = match v with
  | D_bool b -> val_bool b
  | D_int n -> val_int n
  | D_nat n -> val_nat n
  | D_timestamp n -> val_timestamp n
  | D_mutez n -> val_mutez n
  | D_string s -> val_string s
  | D_bytes s -> val_bytes s
  | D_unit -> val_unit ()
  | D_none -> val_none ()
  | D_pair (a, b) -> val_pair translate_value (a,b) ty
  | D_left a -> val_left translate_value a ty
  | D_right b -> val_right translate_value b ty
  | D_some s -> val_some translate_value s ty
  | D_map lst -> val_map translate_value lst ty
  | D_big_map lst -> val_big_map translate_value lst ty
  | D_list lst -> val_list translate_value lst ty
  | D_set lst -> val_set translate_value lst ty
  | D_operation _ -> val_operation ()

let rec translate_expression (expr:expression) (env:environment) : michelson result =
  trace (error_expression expr) @@
  match expr.content with
  | E_skip -> exp_skip ()
  | E_literal v -> exp_literal translate_value v expr.type_value
  | E_closure anon -> exp_closure translate_expression anon env expr.type_value
  | E_application (f , arg) -> exp_application translate_expression (f,arg) env
  | E_variable x -> exp_variable x env
  | E_sequence (a , b) -> exp_sequence translate_expression (a,b) env
  | E_constant(str, lst) -> exp_constant translate_expression (str,lst) env expr.type_value
  | E_make_empty_map sd -> exp_make_empty_map sd
  | E_make_empty_big_map sd -> exp_make_empty_big_map sd
  | E_make_empty_list t -> exp_make_empty_list t
  | E_make_empty_set t -> exp_make_empty_set t
  | E_make_none o -> exp_make_none o
  | E_if_bool (c,a,b) -> exp_if_bool translate_expression (c,a,b) env
  | E_if_none (c, n, (ntv , s)) -> exp_if_none translate_expression (c,n, (ntv, s)) env
  | E_if_cons (cond , nil , ((hd , tl) , cons)) -> exp_if_cons translate_expression (cond , nil , ((hd , tl) , cons)) env
  | E_if_left (c, (l_ntv , l), (r_ntv , r)) -> exp_if_left translate_expression (c, (l_ntv , l), (r_ntv , r)) env
  | E_let_in (v , expr , body) -> exp_let_in translate_expression (v, expr, body) env
  | E_iterator (name , (v , body) , expr) -> exp_iterator translate_expression (name , (v , body) , expr) env
  | E_fold ((v , body) , collection , initial) -> exp_fold translate_expression ((v , body) , collection , initial) env
  | E_assignment (name , lrs , expr) -> exp_assignment translate_expression (name, lrs, expr) env
  | E_update (record, updates) -> exp_update translate_expression (record, updates) env
  | E_while (expr , block) -> exp_while translate_expression (expr, block) env

let translate_function_body = translate_function_body_ translate_expression 