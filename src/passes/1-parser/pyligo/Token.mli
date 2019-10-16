type t =
  | ASS       (* := *)
  | DOT       (* . *)
  | LCBRACE   (* { *)
  | RCBRACE   (* } *)
  | LSBRACKET (* [ *)
  | RSBRACKET (* ] *)
  | LPAREN    (* ( *)
  | RPAREN    (* ) *)
  | START     (* VTOKEN *)
  | END       (* VTOKEN *)
  | EQ        (* = *)
  | COLON     (* : *)
  | COMMA     (* , *)
  | SEMICOLON (* ; *)
  | IMPLIES   (* -> *)
  | LT        (* <  *)
  | PLUS      (* + *)
  | MINUS     (* - *)
  | EOF
  | Ident of string
  | Int of int
  | Str of string
  | Def
  | Elif
  | Else
  | For
  | If
  | In
  | Raise
  | Return
      

type token = t

val to_string: t -> string
