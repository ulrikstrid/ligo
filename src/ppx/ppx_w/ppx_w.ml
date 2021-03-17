open Ppxlib

let postfix (s : string) : string option =
  let s = s |> String.to_seq |> List.of_seq in
  let s = List.rev s in
  let rec g acc xs = match xs with
    | [] -> None
    | '_' :: _xs -> Some acc
    | a :: xs -> g (a :: acc) xs in
  g [] s |> Option.map List.to_seq |> Option.map String.of_seq
  

let map_exprs = object
  inherit Ast_traverse.map as super

  method! value_binding expr =
    let rec postfix_pat pat = match pat with
      | Ppat_var sl -> postfix sl.txt
      | Ppat_constraint (pat, _) -> postfix_pat pat.ppat_desc
      | _ -> None in
    let loc = expr.pvb_loc in
    let expr = super#value_binding expr in
    let pat = expr.pvb_pat  in
    match postfix_pat pat.ppat_desc with
    | Some "w" ->
        let expr' = [%expr fun (warn : _ -> unit) -> [%e expr.pvb_expr] ] in
        { expr with pvb_expr = expr' }
    | _ -> expr

  method! expression_desc expr =
    let postfix_longident l = match l with
      | Lident id -> postfix id
      | Ldot (_, id) -> postfix id
      | Lapply _ -> None in
    let expr = super#expression_desc expr in
    match expr with
    | Pexp_apply (n, b) ->
       (match n.pexp_desc with
        | Pexp_ident id ->
           (match postfix_longident id.txt with
            | Some "w" ->
               let loc = id.loc in
               let expr' = [%expr [%e n] warn] in
               Pexp_apply (expr', b)
            | _ -> expr)
        | _ -> expr)
    | _ -> expr
end

let () =
  Ppxlib.Driver.register_transformation
    "ppx_w"
    ~impl:map_exprs#structure
    ~intf:map_exprs#signature
