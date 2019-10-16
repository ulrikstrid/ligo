type lexeme = string

type t =
  | ASS
  | ARROW 
  | DOT
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | LPAR
  | RPAR
  | START
  | END
  | EQ
  | GEQ
  | LEQ
  | NEQ
  | COLON
  | COMMA
  | SEMI
  | IMPLIES
  | GT
  | LT
  | PLUS
  | MINUS
  | TIMES
  | SLASH
  | MOD
  | EOF
  | Ident of string
  | Uident of string
  | Int of (* int *) string
  | Float of (* float *) string
  | Mtz of string
  | Str of string
  | Bytes of (* bytes *) string
  | Def
  | Elif
  | Else
  | For
  | If
  | In
  | Not
  | Pass
  | Raise
  | Return
  | While
      
type token = t

let to_string = function
  | ASS -> ":="
  | ARROW -> "->"
  | DOT -> "."
  | LBRACE -> "{"
  | RBRACE -> "}"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | LPAR -> "("
  | RPAR -> ")"
  (* Unsure if having these is a good idea... *)
  | START  -> "START"
  | END    ->  "END"
  | EQ -> "="
  | GEQ -> ">="
  | LEQ -> "<="
  | NEQ -> "!="
  | COLON -> ":"
  | COMMA -> ","
  | SEMI -> ";"
  | IMPLIES -> "->"
  | GT -> ">"
  | LT -> "<"
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | SLASH -> "/"
  | MOD -> "%"
  | EOF -> "End of File"
  | Ident _ -> "Ident" (* TODO: Include actual token text *)
  | Uident _ -> "Uident"
  | Int _ -> "Integer"
  | Float _ -> "Float"
  | Mtz _ -> "Mtz"
  | Str _ -> "String"
  | Bytes _ -> "Bytes"
  | Def -> "def"
  | Elif -> "elif"
  | Else -> "else"
  | For -> "for"
  | If -> "if"
  | In -> "in"
  | Not -> "not"
  | Pass -> "pass"
  | Raise -> "raise"
  | Return -> "return"
  | While -> "while"
