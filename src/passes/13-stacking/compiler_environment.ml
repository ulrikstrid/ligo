open Errors
open Trace
open Mini_c
open Environment
open Michelson

let empty : environment = []

let get : environment -> expression_variable -> (michelson, stacking_error) result = fun e s ->
  let%bind (_ , position) =
    generic_try (get_env s e) @@
    (fun () -> Environment.get_i s e) in
  let aux_dig = fun n -> seq [
      i_dig n ;
      i_dup ;
      i_dug (n + 1) ;
    ]
  in
  let code =
    if position < 1
    then i_dup
    else aux_dig position in

  ok code

let pack_closure : environment -> selector -> (michelson, stacking_error) result = fun e lst ->
  let%bind () = Assert.assert_true (corner_case ~loc:__LOC__ "pack closure") (e <> []) in

  (* Tag environment with selected elements. Only the first occurence
     of each name from the selector in the environment is kept. *)
  let e_lst =
    let e_lst = Environment.to_list e in
    let aux selector (s , _) =
      let var_compare = fun (a:var_name) (b:var_name) -> Var.compare a.wrap_content b.wrap_content in
      match List.mem ~compare:var_compare s selector with
      | true -> List.remove_element ~compare:var_compare s selector , true
      | false -> selector , false in
    let e_lst' = List.fold_map_right aux lst e_lst in
    let e_lst'' = List.combine e_lst e_lst' in
    e_lst''
  in

  let%bind (_ , code) =
    let dummy = { type_content = T_base TB_unit ; location = Location.generated } in
    (* extend environment with dummy var for pair accumulator *)
    let e' = (Environment.add (Var.fresh (), dummy) e) in
    let aux = fun (first , code) ((x, _) , b) ->
      match b with
      (* not in selector, ignore *)
      | false -> ok (first , code)
      (* in selector *)
      | true ->
        (* `get` the used variable (accounting for pair accumulator on
           top, after the first variable) *)
        let%bind get_code =
          get (if first then e else e') x in
        ok (false,
            let code = seq [code; get_code] in
            if first
            then code
            else seq [code; i_pair])
    in
    bind_fold_right_list aux (true , seq []) e_lst in

  ok code

let%expect_test _ =
  begin
    let bool = {type_content = T_base TB_bool; location = Location.generated} in
    match pack_closure
            [(Var.of_name "x", bool);
             (Var.of_name "y", bool);
             (Var.of_name "z", bool);
             (Var.of_name "w", bool)]
            [Var.of_name "x"; Var.of_name "z"; Var.of_name "w"] with
    | Ok (mich, _) -> Michelson.pp Format.std_formatter mich
    | _ -> Format.printf "ERROR"
  end;
  [%expect {|
    { { { { { {} ; { DIG 3 ; DUP ; DUG 4 } } ; { DIG 3 ; DUP ; DUG 4 } } ;
          PAIR } ;
        { DIG 1 ; DUP ; DUG 2 } } ;
      PAIR } |}]

let rec uncomb (env : 'a list) : michelson =
  match env with
  | [] -> seq []
  | [_] -> seq []
  | _ :: env ->
    seq [i_dup; i_cdr; uncomb env;
         i_dig (List.length env); i_car]

let unpack_closure : environment -> (michelson , stacking_error) result = fun e ->
  match e with
  | [] -> ok @@ seq []
  | env ->
    ok @@ seq [i_dup; i_car; uncomb env;
               i_dig (List.length env); i_cdr]

let%expect_test _ =
  begin
    let bool = {type_content = T_base TB_bool; location = Location.generated} in
    match unpack_closure
            [(Var.of_name "x", bool);
             (Var.of_name "y", bool);
             (Var.of_name "z", bool);
             (Var.of_name "w", bool)] with
    | Ok (mich, _) -> Michelson.pp Format.std_formatter mich
    | _ -> Format.printf "ERROR"
  end;
  [%expect {|
    { DUP ;
      CAR ;
      { DUP ;
        CDR ;
        { DUP ; CDR ; { DUP ; CDR ; {} ; DIG 1 ; CAR } ; DIG 2 ; CAR } ;
        DIG 3 ;
        CAR } ;
      DIG 4 ;
      CDR } |}]
