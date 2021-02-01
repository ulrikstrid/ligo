(* External dependencies *)

module Region = Simple_utils.Region

(* Redefinitions *)

module type S =
  sig
    type token
    val process : token list -> token list
  end

type token = Token.t

let es6fun = Token.ES6FUN Region.ghost

(* Inserting the ES6FUN virtual token

   The parameter [par_level] counts unmatched open parentheses. *)

let insert_es6fun_token tokens =
  let open Token in
  let rec inner result par_level = function
      (* Balancing parentheses *)

    | (RPAR _ as hd) :: rest ->
        inner (hd :: result) (par_level + 1) rest

      (* let foo = (b: (int, int) => int) => ... *)

    | (LPAR _ as hd)::(COLON _ as c)::(Ident _ as i)::(LPAR _ as l)::rest
      when par_level = 1 ->
        List.rev_append (l :: i :: c :: es6fun :: hd :: result) rest

      (* let a = (x:int) => x *)

    | (LPAR _ as hd) :: (ARROW _ as a) :: rest when par_level = 1 ->
        List.rev_append (a :: es6fun :: hd :: result) rest

    | (DOT _ as dot) :: (UIdent _ as hd) :: rest ->
        inner (hd :: dot :: result) par_level rest

    | hd :: (UIdent _ as c) :: rest ->
        List.rev_append (c :: hd :: result) rest

      (* let foo = (a: int) => (b: int) => a + b *)

    | hd :: (ARROW _ as a) :: rest when par_level = 0 ->
        List.rev_append (a :: es6fun :: hd :: result) rest

      (* ((a: int) => a *)

    | (LPAR _ as hd) :: (LPAR _ as a) :: rest when par_level = 1 ->
        List.rev_append (a :: es6fun :: hd :: result) rest

      (* let x : (int => int) *)

    | (LPAR _ as hd) :: rest when par_level = 0 ->
        List.rev_append (hd :: es6fun :: result) rest

      (* Balancing parentheses *)

    | (LPAR _ as hd) :: rest ->
        inner (hd :: result) (par_level - 1) rest

      (* When the arrow '=>' is not part of a function: *)

    | (RBRACKET _ | Ctor_Some _ | Ctor_None _ as hd) :: rest ->
        List.rev_append (hd :: result) rest

      (* let foo : int => int = (i: int) => ...  *)

    | (COLON _ as hd) :: (Ident _ as i) :: (Let _ as l) :: rest
      when par_level = 0 ->
        List.rev_append (l :: i :: hd :: es6fun :: result) rest

    | (EQ _ as hd) :: rest ->
        List.rev_append (hd :: es6fun :: result) rest

    | hd :: rest -> inner (hd :: result) par_level rest

    | []  -> List.rev result
  in inner [] 0 tokens

(* The self-pass proper *)

let process tokens =
  let open Token in
  let rec inner result = function
    (ARROW _ as a) :: rest ->
      inner (insert_es6fun_token (a :: result)) rest
  | hd :: rest -> inner (hd :: result) rest
  | [] -> List.rev result
  in inner [] tokens
