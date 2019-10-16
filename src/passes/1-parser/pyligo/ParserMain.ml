let lexbuf = Lexing.from_channel stdin in 
let read = Lexer.get_token in
Parser.contract read lexbuf
