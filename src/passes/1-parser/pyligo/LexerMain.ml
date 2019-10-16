let rec driver reader lexbuf = 
   match reader lexbuf with
     | LexToken.EOF -> exit 0
     | _ -> (print_string (LexToken.to_string (reader lexbuf)) ; print_string "\n" ; driver reader lexbuf)

let lexbuf = (Lexing.from_channel stdin)
let () = let read = Lexer.get_token in (driver read lexbuf)
