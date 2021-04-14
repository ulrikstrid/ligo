open Ast_core
open Trace
open Typer_common.Errors

let rec assert_type_expression_eq (a, b: (type_expression * type_expression)) : (unit, typer_error) result = match (a.type_content, b.type_content) with
  | T_app {type_operator=ta;arguments=lsta}, T_app {type_operator=tb;arguments=lstb} -> (
    let%bind () = Assert.assert_true (different_types a b) (Var.equal ta tb) in
    if List.length lsta <> List.length lstb then
      fail @@ different_types a b
    else
      trace (fun _ -> different_types a b)
      @@ bind_list_iter assert_type_expression_eq (List.combine lsta lstb)
  )
  | T_app _, _ -> fail @@ different_types a b
  | T_sum sa, T_sum sb -> (
      let sa' = LMap.to_kv_list_rev sa.fields in
      let sb' = LMap.to_kv_list_rev sb.fields in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let%bind _ =
          Assert.assert_true (corner_case "different keys in sum types")
          @@ (ka = kb) in
        assert_type_expression_eq (va, vb)
      in
      let%bind _ =
        Assert.assert_list_same_size (different_types a b)
        sa' sb'
      in
      trace (fun _ -> different_types a b) @@
      bind_list_iter aux (List.combine sa' sb')
    )
  | T_sum _, _ -> fail @@ different_types a b
  | T_record ra, T_record rb
       when Helpers.is_tuple_lmap ra.fields <> Helpers.is_tuple_lmap rb.fields -> (
    fail @@ different_types a b
  )
  | T_record ra, T_record rb -> (
      let sort_lmap r' = List.sort (fun (Label a,_) (Label b,_) -> String.compare a b) r' in
      let ra' = sort_lmap @@ LMap.to_kv_list_rev ra.fields in
      let rb' = sort_lmap @@ LMap.to_kv_list_rev rb.fields in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let%bind _ =
          trace (fun _ -> different_types a b) @@
          let Label ka = ka in
          let Label kb = kb in
          Assert.assert_true (different_types a b) (ka = kb) in
        assert_type_expression_eq (va, vb)
      in
      let%bind _ =
        Assert.assert_true (different_types a b) @@
          Option.equal Misc.layout_eq ra.layout rb.layout in
      let%bind _ =
        Assert.assert_list_same_size (different_types a b) ra' rb' in
      trace (fun _ -> different_types a b)
      @@ bind_list_iter aux (List.combine ra' rb')

    )
  | T_record _, _ -> fail @@ different_types a b
  | T_arrow {type1;type2}, T_arrow {type1=type1';type2=type2'} ->
      let%bind _ = assert_type_expression_eq (type1, type1') in
      let%bind _ = assert_type_expression_eq (type2, type2') in
      ok ()
  | T_arrow _, _ -> fail @@ different_types a b
  | T_variable x, T_variable y -> let _ = (x = y) in failwith "TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding"
  | T_variable _, _ -> fail @@ different_types a b
  | T_module_accessor {module_name=mna;element=a}, T_module_accessor {module_name=mnb;element=b} when String.equal mna mnb -> (
      let%bind _ = assert_type_expression_eq (a, b) in
      ok ()
  )
  | T_module_accessor _,_ -> fail @@ different_types a b
  | T_singleton _ , _ -> failwith "TODO: mmmh, not sure comparing singleton should happen (?)"

(* No information about what made it fail *)
let type_expression_eq ab = Trace.to_bool @@ assert_type_expression_eq ab
