(*

This implements the pattern_matching compiler of `Peyton-Jones, S.L., The Implementation of Functional Programming Languages`, chapter 5.
By reduction, this algorithm transforms pattern matching expression into (nested) cases expressions.
`Sugar` match expres0sion being 'pattern matching' expression and `Core`/`Typed` being 'case expressions'.

List patterns are treated as the variant type `NIL | Cons of (hd , tl)` would be
"product patterns" (e.g. tuple & record) are considered variables, but an extra rule (product_rule) was necessary to handle them

*)

module I = Ast_core
module O = Ast_typed

open Trace
(* open Stage_common.Maps *)
open Typer_common.Errors

type matchees = O.expression_variable list
type pattern = I.type_expression I.pattern * O.type_expression (* REMITODO : rename to typed_pattern ?*)
type equations = (pattern list * (I.expression * O.environment)) list
type type_fun =
  O.environment -> ?tv_opt:O.type_expression -> I.expression -> (O.expression, typer_error) result
type rest = O.expression_content
  (* | Default of O.expression_content
  | Folded of O.expression_content *)

let is_var : _ I.pattern -> bool = fun p ->
  match p with
  | P_var _ -> true
  | P_tuple _ -> true
  | P_record _ -> true
  | P_unit -> true
  | _ -> false
let is_product : _ I.pattern -> bool = fun p ->
  match p with
  | P_tuple _ -> true
  | P_record _ -> true
  | _ -> false

type 'a pm_result = ('a, typer_error) result

let list_sep_x x = let open Simple_utils.PP_helpers in list_sep x (tag "@,")
let pp_matchees : Format.formatter -> O.expression_variable list -> unit =
  fun ppf lst ->
    let lst = List.map (fun (e:O.expression_variable) -> e.wrap_content) lst in
    Format.fprintf ppf "@[%a@]" (Simple_utils.PP_helpers.list_sep_d_par Var.pp) lst

let pp_patterns : Format.formatter -> pattern list -> unit =
  fun ppf lst ->
    let patterns = List.map fst lst in
    Format.fprintf ppf "@[ [ %a ]@]" (Simple_utils.PP_helpers.list_sep_d (Stage_common.PP.match_pattern I.PP.type_expression)) patterns

let pp_eq : Format.formatter ->  (pattern list * (I.expression * O.environment)) -> unit =
  fun ppf (pl,(body,_env)) ->
    Format.fprintf ppf "%a -> %a" pp_patterns pl I.PP.expression body

let pp_eqs : Format.formatter -> equations -> unit =
  fun ppf lst ->
    Format.fprintf ppf "@[<v>[@,%a@,] @]" (list_sep_x pp_eq) lst

let assert_body_t : body_t:O.type_expression option -> Location.t -> O.type_expression -> unit pm_result =
  fun ~body_t loc t ->
    match body_t with
    | Some prev_t ->
      let%bind () = Typer_common.Helpers.assert_type_expression_eq loc (prev_t,t) in
      ok ()
    | None -> ok ()

(* 
  REMITODO: move that to combinators.ml : get_sum_associated_type ; get_record_associated_type returning options,
  and being Trace_option (corner_case)'ed away because type_matchee is always called before those functions,
OR maybe we can simply give the matchee_t to split_equations and co
*)
let extract_variant_type : O.label -> O.type_expression -> O.type_expression pm_result =
  fun label t ->
  match t.type_content with
  | T_sum rows -> (
    match O.LMap.find_opt label rows.content with
    | Some t -> ok t.associated_type
    | _ -> failwith "REMITODO: Error label label do not belong to type t"
  )
  | _ ->
    let x = Format.asprintf "REMITODO: pattern p indicates a variant type but got another type : %a" O.PP.type_expression t in
    failwith x

let extract_record_type : O.label -> O.type_expression -> O.type_expression pm_result =
  fun label t ->
  match t.type_content with
  | T_record rows -> (
    match O.LMap.find_opt label rows.content with
    | Some t -> ok t.associated_type
    | _ -> failwith "REMITODO: Error label label do not belong to type t"
  )
  | _ ->
    let x = Format.asprintf "REMITODO: pattern p indicates a record type but got another type : %a" O.PP.type_expression t in
    failwith x

(*
get_matchee_type [ ( [ (p01,t) , .. , (p0n,t0n) ], body0 ) , .. , ( [ (pk1,t) , .. , (pkn,tkn) ], bodyk ) ]
makes sure that the left-most type/patterns pairs of the equations have the same type and return this type.
It also fails if the pattern do not conform to the type (T_sum with P_variant, T_record with P_tuple/P_record ..)
e.g.
  get_matchee_type [ ( [ (p0,t0) , ... ], body0 ) , .. , ( [ (pk,tk) , ... ], bodyk ) ]
  checks:
    - t0 = t1 = .. = tk 
    - conform p0 t0 && conform p1 t1 && conform pk tk
*)
let type_matchee : equations -> O.type_expression pm_result =
  fun eqs ->
    let pt1s = List.map (fun el -> List.hd @@ fst el) eqs in
    let conforms : pattern -> unit pm_result = fun (p,t) ->
      match p , t.type_content with
      | I.P_var _ , _ -> ok ()
      | I.P_variant _ , O.T_sum _ -> ok ()
      | (P_tuple _ | P_record _) , O.T_record _ -> ok ()
      | I.P_unit , O.T_constant { injection ; _ } when String.equal (Ligo_string.extract injection) Stage_common.Constant.unit_name -> ok ()
      | I.P_list _ , O.T_constant { injection ; _ } when String.equal (Ligo_string.extract injection) Stage_common.Constant.list_name -> ok ()
      | _ ->
        (* REMITODO: the location must be binded with the pattern somehow *)
        let s = Format.asprintf "REMITODO : pattern %a does not conform to type %a" (Stage_common.PP.match_pattern I.PP.type_expression) p O.PP.type_expression t in
        fail (corner_case s)
    in
    let aux : O.type_expression option -> pattern -> O.type_expression option pm_result = fun t_opt (p,t) ->
      let%bind () = conforms (p,t) in
      match t_opt with
      | None -> ok (Some t)
      | Some t' ->
        let%bind () = Typer_common.Helpers.assert_type_expression_eq Location.generated (t, t') in
        ok t_opt
    in
    let%bind t = bind_fold_list aux None pt1s in
    ok @@ Option.unopt_exn t

(*
  `substitute_var_in_body to_subst new_var body` replaces variables equal to `to_subst` with variable `new_var` in expression `body`.
  note that `new_var` here is never a user variable (always previously generated by the compiler)
*)
let rec substitute_var_in_body : I.expression_variable -> O.expression_variable -> I.expression -> I.expression pm_result =
  fun to_subst new_var body ->
    (* let () = Format.printf "substituting %a by %a in %a\n" I.PP.expression_variable to_subst I.PP.expression_variable new_var I.PP.expression body in *)
    let aux : unit -> I.expression -> (bool * unit * I.expression,_) result =
      fun () exp ->
        let ret continue exp = ok (continue,(),exp) in
        match exp.content with
        | I.E_variable var when Var.equal var.wrap_content to_subst.wrap_content -> ret true { exp with content = E_variable new_var }
        | I.E_let_in letin when Var.equal letin.let_binder.var.wrap_content to_subst.wrap_content -> ret false exp
        | I.E_lambda _ -> failwith "REMITODO: if to_subst as arg, do not substitute the body.."
        | I.E_matching m -> (
          let%bind matchee = substitute_var_in_body to_subst new_var m.matchee in
          let aux : bool -> _ I.pattern -> bool =
            fun b p ->
              match p with
              | P_var x when Var.equal x.var.wrap_content to_subst.wrap_content -> true
              | _ -> b
          in
          let%bind cases = bind_map_list
            (fun (case : _ I.match_case) ->
              match Stage_common.Helpers.fold_pattern aux false case.pattern with
              | true -> ok case
              | false -> ok case)
            m.cases
          in
          let m' = I.{matchee ; cases} in
          ret false { exp with content = I.E_matching m'}
        )
        | _ -> ret true exp
    in
    let%bind ((), res) = Self_ast_core.fold_map_expression aux () body in
    ok res

let make_var_pattern : O.expression_variable -> I.type_expression I.pattern =
  fun var -> P_var { var ; ascr = None }

let rec partition : ('a -> bool) -> 'a list -> 'a list list =
  fun f lst ->
    let add_inner x ll =
      match ll with
      | hdl::tll -> (x::hdl)::tll
      | _ -> assert false
    in
    match lst with
    | [] -> []
    | [x] -> [[x]]
    | x::x'::tl ->
      if f x = f x' then add_inner x (partition f (x'::tl))
      else [x] :: (partition f (x'::tl))

let split_equations : equations -> equations O.label_map pm_result =
  fun eqs ->
    let aux : equations O.label_map -> pattern list * (I.expression * O.environment) -> equations O.label_map pm_result =
      fun m (pl , (body , env)) ->
        let (phd,t) = List.hd pl in
        let ptl = List.tl pl in
        let dummy_p : unit -> pattern = fun () ->
          let var =  Location.wrap @@ Var.fresh ~name:"_" () in
          (make_var_pattern var, O.t_unit ())
        in
        match phd with
        | P_variant (label,p_opt) ->
          let%bind t = extract_variant_type label t in
          let upd : equations option -> equations option = fun kopt ->
            match kopt, p_opt with
            | Some eqs , None   -> Some ( (dummy_p ()::ptl , (body,env))::eqs )
            | None     , None   -> Some [ (dummy_p ()::ptl , (body,env)) ]
            | Some eqs , Some p ->
              let p = (p,t) in
              Some (( p::ptl , (body,env))::eqs)
            | None     , Some p ->
              let p = (p,t) in
              Some [ (p::ptl          , (body,env)) ]
          in
          ok @@ O.LMap.update label upd m
        | _ -> failwith "impossible: previously regarded as a 'constructor rule'"
    in
    bind_fold_right_list aux O.LMap.empty eqs

let rec match_ : type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t ms eqs def ->
  (* REMITODO, Invariant : for all eq in eqs : (List.length rules) = (List.length (fst eq)) *)
  (* let () = Format.printf "CALLING match_:\n matchees: %a\n eqs: %a\n-----\n" pp_matchees ms pp_eqs eqs in *)
  match ms , eqs with
  | [] , [([],(body,env))] ->
      let%bind body =
        (* let s = Format.asprintf "%a \n " I.PP.expression body in *)
        (* let s = s ^ (Format.asprintf "%a \n" O.PP.environment env) in *)
        (* trace_strong (corner_case ("REMITODO: could not compile body: "^s)) @@  *)
        type_f ?tv_opt:body_t env body in
      ok body
  | [] , eqs when List.for_all (fun (ps,_) -> List.length ps = 0) eqs -> failwith "Redundant product patterns"
  | _ ->
    let leq = partition (fun (pl,_) -> is_var (fst @@ List.hd pl)) eqs in
    (* let () =
      if List.length leq > 1 then
        let () = Format.printf "Mixture rule applies, partitions:\n" in
        List.iter (fun eq -> Format.printf "%a\n-\n" pp_eqs eq) leq
    in *)
    let aux = fun (prev_opt:O.expression option) part_eq ->
      let%bind r =
        match prev_opt with
        | None -> consvar ~type_f ~body_t ms part_eq def
        | Some prev -> consvar ~type_f ~body_t ms part_eq prev.expression_content
      in
      (* let () = Format.printf "returning from mixture rule with : %a \n" O.PP.expression r in *)
      ok (Some r)
    in
    let%bind r = bind_fold_right_list aux None leq in
    ok @@ Option.unopt_exn r

and consvar : type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t ms eqs def ->
  (* let () = Format.printf "CALLING CONSVAR WITH:\n matchees: %a\n eqs: %a\n-----\n" pp_matchees ms pp_eqs eqs in *)
  let p1s = List.map (fun el -> fst @@ List.hd @@ fst el) eqs in
    if List.for_all is_var p1s then
      let contain_product = List.exists is_product p1s in
      var_rule ~type_f ~body_t contain_product ms eqs def
    else
      ctor_rule ~type_f ~body_t ms eqs def

and var_rule : type_f:type_fun -> body_t:O.type_expression option -> bool -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t contain_product ms eqs def ->
  (* let () = Format.printf "\nVAR RULE applies\n" in *)
  match ms with
  | mhd::mtl ->
    if contain_product then
      product_rule ~type_f ~body_t ms eqs def
    else
      let aux : pattern list * (I.expression * O.environment) -> (pattern list * (I.expression * O.environment)) pm_result =
        fun (pl, (body,env)) ->
        match pl with
        | (P_var b , t)::ptl -> (
          let%bind body' = substitute_var_in_body b.var mhd body in
          (* let () = Format.printf "\n----- \n Sub %a by %a in :\n %a \n gave :\n %a \n----- \n"
            I.PP.expression_variable b.var
            I.PP.expression_variable mhd
            I.PP.expression body
            I.PP.expression body'
          in *)
          let env' = O.Environment.add_ez_binder mhd t env in
          (* REMITODO Also substitute b.var in env ?? should not *)
          ok (ptl , (body',env'))
        )
        | (P_unit, _t)::ptl -> (
          ok (ptl , (body,env))
        )
        | _ -> failwith "corner case1"
      in
      let%bind eqs' = bind_map_list aux eqs in
      match_ ~type_f ~body_t mtl eqs' def
  | [] -> failwith "corner case2"

and ctor_rule : type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t ms eqs def ->
  (* let () = Format.printf "\nCTOR RULE applies\n" in *)
  (* REMITODO Add a Const_list case which will be very similar to Constr but with a Match_list *)
  match ms with
  | mhd::mtl ->
    let aux_p : O.label * equations -> O.matching_content_case pm_result =
      fun (constructor,eq) ->
        let proj = Location.wrap @@ Var.fresh ~name:"ctor_proj" () in
        let new_ms = proj::mtl in
        let%bind nested = match_ ~type_f ~body_t new_ms eq def in
        ok @@ ({ constructor ; pattern = proj ; body = nested } : O.matching_content_case)
    in
    let aux_m : O.label * O.type_expression -> O.matching_content_case =
      fun (constructor,t) ->
        let proj = Location.wrap @@ Var.fresh ~name:"_" () in
        let body = O.make_e def t in
        { constructor ; pattern = proj ; body }
    in
    let%bind matchee_t = type_matchee eqs in
    let%bind eq_map = split_equations eqs in
    let%bind rows = trace_option (expected_variant Location.generated matchee_t) (O.get_t_sum matchee_t) in
    let eq_opt_map = O.LMap.mapi (fun label _ -> O.LMap.find_opt label eq_map) rows.content in
    let splitted_eqs = O.LMap.to_kv_list @@ eq_opt_map in
    let present = List.filter_map (fun (c,eq_opt) -> match eq_opt with Some eq -> Some (c,eq) | None -> None) splitted_eqs in
    let%bind present_cases = bind_map_list aux_p present in
    let matchee = O.make_e (O.e_variable mhd) matchee_t in
    let%bind body_t =
      let aux t_opt (c:O.matching_content_case) =
        let%bind () = assert_body_t ~body_t:t_opt c.body.location c.body.type_expression in
        match t_opt with
        | None -> ok (Some c.body.type_expression)
        | Some _ -> ok t_opt
      in
      let%bind t = bind_fold_list aux body_t present_cases in
      let t = Option.unopt_exn t in
      ok t
    in
    let missing = List.filter_map (fun (c,eq_opt) -> match eq_opt with Some _ -> None | None -> Some (c,body_t)) splitted_eqs in
    let missing_cases = List.map aux_m missing in
    let cases = O.Match_variant { cases = missing_cases @ present_cases ; tv = matchee_t } in
    ok @@ O.make_e (O.E_matching { matchee ; cases }) body_t
  | [] -> failwith "corner case"

and product_rule : type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t ms eqs def ->
  (* let () = Format.printf "Product rule applies\n" in *)
  match ms with
  | mhd::_ -> (
    let%bind matchee_t = type_matchee eqs in
    let product_shape =
      let eq_opt = List.find_opt (fun (pl,_) -> match pl with (p,_)::_ -> is_product p | _ -> false) eqs in
      match eq_opt with
      | Some (p::_,_) -> p
      | _ -> failwith "cornercase : at least one record/tuple pattern should appear here (product rule)"
    in
    let%bind filler =
      match product_shape with
      | P_tuple ps , t ->
        let aux i _ =
          let%bind field_t = extract_record_type (O.Label (string_of_int i)) t in
          let v = make_var_pattern (Location.wrap @@ Var.fresh ~name:"_" ()) in
          ok (v,field_t)
        in
        bind_mapi_list aux ps
      | P_record (labels,_) , t ->
        let aux l =
          let%bind field_t = extract_record_type l t in
          let v = make_var_pattern (Location.wrap @@ Var.fresh ~name:"_" ()) in
          ok (v,field_t)
        in
        bind_map_list aux labels
      | _ -> failwith "corner case4"
    in
    let aux : pattern list * (I.expression * O.environment) -> (pattern list * (I.expression * O.environment)) pm_result =
      fun (pl, (body,env)) ->
      match pl with
      | (prod,t)::ptl -> (
        match prod with
        | P_tuple ps ->
          let var_filler = (make_var_pattern (Location.wrap @@ Var.fresh ~name:"_" ()) , t) in
          let aux i p =
            let%bind field_t = extract_record_type (O.Label (string_of_int i)) t in
            ok (p,field_t)
          in
          let%bind tps = bind_mapi_list aux ps in
          ok (tps @ var_filler::ptl , (body,env))
        | P_record (labels,ps) ->
          let var_filler = (make_var_pattern (Location.wrap @@ Var.fresh ~name:"_" ()) , t) in
          let aux (label,p) =
            let%bind field_t = extract_record_type label t in
            ok (p,field_t)
          in
          let%bind tps = bind_map_list aux (List.combine labels ps) in
          ok (tps @ var_filler::ptl , (body,env))
        | P_var _ ->
          ok (filler @ pl , (body,env))
        | _ -> failwith "corner case5"
      )
      | [] -> failwith "corner case6"
    in
    let%bind eqs' = bind_map_list aux eqs in
    let%bind lb =
      match product_shape with
      | P_tuple ps , t ->
        let aux : int -> _ -> (O.label * (O.expression_variable * O.type_expression)) pm_result =
          fun i _ ->
            let l = (O.Label (string_of_int i)) in
            let%bind field_t = extract_record_type l t in
            let v = Location.wrap @@ Var.fresh ~name:"tuple_proj" () in
            ok @@ (l, (v,field_t))
        in
        bind_mapi_list aux ps
      | P_record (labels,_) , t ->
        let aux : O.label -> (O.label * (O.expression_variable * O.type_expression)) pm_result  =
          fun l ->
            let v = Location.wrap @@ Var.fresh ~name:"record_proj" () in
            let%bind field_t = extract_record_type l t in
            ok @@ (l , (v,field_t))
        in
        bind_map_list aux labels
      | _ -> failwith "corner case (impossible)"
    in
    let fields = O.LMap.of_list lb in
    let new_matchees = List.map (fun (_,((x:O.expression_variable),_)) -> x) lb in
    let%bind body = match_ ~type_f ~body_t (new_matchees @ ms) eqs' def in
    let record_type : O.rows =
      match (snd product_shape).type_content with
      | O.T_record rows -> rows
      | _ -> failwith "corner_case 7"
    in
    let cases = O.Match_record { fields; body ; record_type } in
    let matchee = O.make_e (O.e_variable mhd) matchee_t in
    ok @@ O.make_e (O.E_matching { matchee ; cases }) body.type_expression
  )
  | [] -> failwith "corner case 8"

and compile_matching ~type_f ~body_t matchee (eqs:equations) =
    let p1s = List.map (fun el -> fst @@ List.hd @@ fst el) eqs in
    let f =
      if List.exists is_product p1s then
        product_rule ~type_f ~body_t
      else
        match_ ~type_f ~body_t
    in
    (* let () = Format.printf "STARTING WITH:\n matchees: %a\n eqs: %a\n-----\n" pp_matchees [matchee] pp_eqs eqs in *)
    (*REMITODO failwith "impossible", to be optimized later ?*)
    let def =
      let fs = O.make_e (O.E_literal (O.Literal_string (Ligo_string.verbatim "PARTIAL MATCH"))) (O.t_string ()) in
      O.e_failwith fs
    in
    let%bind res = f [matchee] eqs def in
    let%bind res = top_level_simpl res in
    (* let () = Format.printf "\n\n--SIMPL--\n%a\n----\n\n" O.PP.expression res_simpl in *)
    ok res

and is_generated_partial_match : O.expression -> bool =
  fun exp ->
    match exp.expression_content with
    | O.E_constant {cons_name=C_FAILWITH ; arguments=[e]} -> (
      match O.get_a_string e with
      | Some fw -> String.equal fw "PARTIAL MATCH"
      | None -> false
    )
    | _ -> false

and merge_record_case : (O.expression_variable * O.matching_content_record) -> O.matching_content_record option pm_result =
  fun (mvar, {fields=_ ; body ; record_type }) ->
    let aux : (O.expression_variable * O.type_expression) O.label_map option -> O.expression ->
      (bool * (O.expression_variable * O.type_expression) O.label_map option * O.expression,_) result =
        fun prev exp ->
          let continue = ok (true,prev,exp) in
          let stop new_fields expression_content = ok (false,Some new_fields,{exp with expression_content}) in
          match exp.expression_content with
          | O.E_matching m -> (
            match O.get_variable m.matchee with
            | Some v when Var.equal v.wrap_content mvar.wrap_content && Var.is_generated v.wrap_content -> (
              match m.cases with
              | O.Match_record v -> stop v.fields v.body.expression_content
              | _ -> continue
            )
            | _ -> continue
          )
          | _ -> continue
    in
    let%bind (new_fields_opt , body') = Self_ast_typed.fold_map_expression aux None body in
    match new_fields_opt with
    | Some fields ->
      let new_case : O.matching_content_record = { fields ; record_type ; body=body' } in
      ok (Some new_case)
    | None -> ok None

and merge_variant_case : (O.expression_variable * O.matching_content_case) -> O.matching_content_case option pm_result =
  fun (mvar , {constructor;pattern=_;body}) ->
    let aux : O.expression_variable option -> O.expression -> (bool * O.expression_variable option * O.expression,_) result =
      fun prev exp ->
        let continue = ok (true,prev,exp) in
        let stop new_pattern expression_content = ok (false,Some new_pattern,{exp with expression_content}) in
        match exp.expression_content with
        | O.E_matching m -> (
          match O.get_variable m.matchee with
          | Some v when Var.equal v.wrap_content mvar.wrap_content && Var.is_generated v.wrap_content -> (
            match m.cases with
            | O.Match_variant v -> (
              let (_fw,no_fw) = List.partition (fun (case:O.matching_content_case) -> is_generated_partial_match case.body) v.cases in
              match no_fw with
              | [] -> fail (corner_case "REMITODO: mmmmmhhhh ?")
              | lst -> (
                let x = List.find_opt (fun ({constructor=c;_}:O.matching_content_case) -> O.Compare.label c constructor = 0 ) lst in
                match x with
                | Some x ->
                  let { pattern ; body ; _ } : O.matching_content_case = x in
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
    let%bind (new_pattern_opt , body') = Self_ast_typed.fold_map_expression aux None body in
    match new_pattern_opt with
    | Some pattern ->
      let new_case : O.matching_content_case = { constructor ; pattern ; body=body' } in
      ok (Some new_case)
    | None -> ok None

and top_level_simpl : O.expression -> O.expression pm_result =
  fun exp ->
    let aux : bool -> O.expression -> (bool * bool * O.expression,_) result =
      fun has_been_simpl exp ->
        let continue = ok (true,has_been_simpl,exp) in
        let ret continue has_been_simpl expression_content = ok (continue,has_been_simpl,{exp with expression_content}) in
        match exp.expression_content with
        | O.E_matching m -> (
          match m.matchee.expression_content with
          | O.E_variable x when Var.is_generated x.wrap_content -> (
            match m.cases with
            | O.Match_variant v -> (
              let aux : (bool * O.matching_content_case list) -> O.matching_content_case -> (bool * O.matching_content_case list) pm_result =
                fun (has_been_simpl,res) case ->
                  let%bind new_case_opt = merge_variant_case (x,case) in
                  match new_case_opt with
                  | Some case -> ok (true,case::res)
                  | None -> ok (has_been_simpl,case::res)
              in
              let%bind (has_been_simpl,cases) = bind_fold_list aux (false,[]) v.cases in
              if has_been_simpl then 
                ret false has_been_simpl (O.E_matching { m with cases = O.Match_variant { v with cases} })
              else
                continue
            )
            | O.Match_record r -> (
              let%bind new_ = merge_record_case (x,r) in
              match new_ with
              | Some r -> ret false true (O.E_matching { m with cases = O.Match_record r })
              | None -> continue
            )
            | _ -> continue
          )
          | _ -> continue
        )
        | _ -> continue
    in
    (* let%bind (_has_been_simpl, res) = Self_ast_typed.fold_map_expression aux false exp in *)
    do_while (Self_ast_typed.fold_map_expression aux false) exp

and do_while : (O.expression -> (bool * O.expression) pm_result) -> O.expression -> O.expression pm_result =
  fun f exp ->
    let%bind (has_been_simpl, exp) = f exp in
    if has_been_simpl then
      (* let () = Format.printf "Has been simpl\n" in *)
      do_while f exp
    else
      (* let () = Format.printf "Has not been simpl\n" in *)
      ok exp