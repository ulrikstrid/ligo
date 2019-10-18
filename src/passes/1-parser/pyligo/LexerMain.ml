let rec driver reader lexbuf =
  let token = reader lexbuf in 
  let () = print_string (Lexer.to_string token) ; print_string "\n" in
  match token with
  | Lexer.EOF -> exit 0
  |  _ -> driver reader lexbuf

let lexbuf = (Lexing.from_channel stdin)
let () = let read = Lexer.get_token in (driver read lexbuf)
