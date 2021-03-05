module type S =
  sig
    type token
    val process : token list -> token list
  end

type token = Token.t

let automatic_semicolon_insertion tokens = 
  let open Token in
  let rec inner result = function
    (SEMI _ as semi) :: (Let _ as t)  :: rest
  | (SEMI _ as semi) :: (Const _ as t)  :: rest
  | (SEMI _ as semi) :: (Type _ as t)  :: rest
  | (SEMI _ as semi) :: (Return _ as t)  :: rest
  | (LBRACE _ as semi) :: (Let _ as t)  :: rest
  | (LBRACE _ as semi) :: (Const _ as t)  :: rest
  | (LBRACE _ as semi) :: (Type _ as t)  :: rest
  | (LBRACE _ as semi) :: (Return _ as t)  :: rest -> 
    inner (t:: semi :: result) rest
  | token :: (Const _ as t) :: rest
  | token :: (Type _ as t) :: rest
  | token :: (Return _ as t) :: rest
  | token :: (Let _ as t) :: rest ->
  (* | token :: (Lident _ as t) :: rest
  | token :: (RBRACE _ as t) :: rest -> *)
    let (r, _) = Token.proj_token token in
    let (r2, _) = Token.proj_token t in
    if r#stop#line < r2#start#line  then (
      inner (t :: SEMI Region.ghost :: token :: result) rest 
    )
    else (
      match token with 
        RBRACE _ as t -> 
        inner (t :: SEMI Region.ghost :: token :: result) rest 
      | _ ->
        inner (t :: token :: result) rest 
    )
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in 
  inner [] tokens


let process (tokens: token list) = 
  match tokens with 
    hd :: tl -> 
      let result = hd :: automatic_semicolon_insertion tl in
      result
  | _ -> tokens