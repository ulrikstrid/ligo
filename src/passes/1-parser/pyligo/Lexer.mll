{

module Keywords = Map.Make(String)

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
  | Assert
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

let to_string =
  let param_token (name: string) (s: string) : string =
  String.concat " " [name ; "(" ; s ; ")" ] in
  function
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
  | Ident s -> param_token "Ident" s
  | Uident s -> param_token "Uident" s
  | Int s -> param_token "Integer" s
  | Float s -> param_token "Float" s
  | Mtz _ -> "Mtz"
  | Str s -> param_token "String" s
  | Bytes _ -> "Bytes"
  | Assert -> "assert"
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

let keyword_list = [
  ("def", Def);
  ("if",  If);
  ("elif", Elif);
  ("else", Else);
  ("for",  For);
  ("in",  In);
  ("raise", Raise);
  ("return", Return);
]

exception NonPair of string

let keyword_map =
  let add_pair
      (kwd_map: t Map.Make(String).t)
      (pair: (Keywords.key * t)) =
    let (a, b) = pair in 
    Keywords.add a b kwd_map
  in List.fold_left add_pair Keywords.empty keyword_list


let reset_file ~file buffer =
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname = file}

let reset_line ~line buffer =
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_lnum = line}

let reset_offset ~offset buffer =
  assert (offset >= 0);
  let open Lexing in
  let bol = buffer.lex_curr_p.pos_bol in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum = bol + offset }

let reset ?file ?line ?offset buffer =
  let () =
    match file with
      Some file -> reset_file ~file buffer
    |      None -> () in
  let () =
    match line with
      Some line -> reset_line ~line buffer
    |      None -> () in
  match offset with
    Some offset -> reset_offset ~offset buffer
  |        None -> ()


(* State type to record the balance of a parenthetical, bracket, etc expression,
   as well as the last token seen. *)

type lex_state = {
  mutable paren: int;
  mutable bracket: int;
  mutable brace: int;
  mutable indent_level: int;
  mutable last_token: t;
}

exception Whitespace_Error
exception Unexpected_Indent
exception Unterminated_String

let track_paren (state: lex_state) : lex_state =
  let () = assert (state.paren >= 0) in
  let () = assert (state.bracket >= 0) in
  let () = assert (state.brace >= 0) in
  state 

let is_paren_expr state =
  if state.paren >= 1 then true
  else if state.bracket >= 1 then true
  else if state.brace >= 1 then true
  else false

let indent_level indent_str =
  if (String.length indent_str) mod 4 != 0
  then raise Whitespace_Error
  else (String.length indent_str) / 4

let simple_update (state: lex_state) (t: t) : lex_state * t list =
  {state with last_token = t;}, [t] 

let paren_update (state: lex_state) (t: t) : lex_state * t list =
  simple_update (track_paren state) t

let expected_indent indent_str state = 
  if indent_level indent_str = (state.indent_level + 1)
  then {state with indent_level = state.indent_level + 1; 
                   last_token = START; }, [START]
  else raise Unexpected_Indent

let line_terminator indent_str state = 
  let rec list_of value n lst = 
    if n <= 0 then lst else list_of value (n - 1) (value :: lst)
  in let deindent_level = (state.indent_level - (indent_level indent_str)) in
  if state.last_token = END 
  then state, []
  else if indent_level indent_str > state.indent_level
  then raise Unexpected_Indent
  else if indent_level indent_str = state.indent_level
  then simple_update state SEMI
  else {state with indent_level = indent_level indent_str;
                   last_token = END; }, [SEMI] @ (list_of END deindent_level [])

}

let newline = ['\n' '\r']
let white = [' ' '\t']
(* TODO: Allow the lexer to recognize more diverse kinds of indent *)
let indent = ' ' ' ' ' ' ' '
let line_term = newline indent*
let digit = ['0'-'9']
let natnum = digit | digit (digit | '_')* digit
let integer = '-'? natnum
let decimal = digit+ '.' digit+

let small = ['a'-'z']
let capital = ['A'-'Z']
let letter = small | capital

let ichar = letter | digit | '_'
let ident = ichar ichar* | '_' ichar+
let uident = capital (capital | '_')* 


rule scan state =
  (* In python, we want to insert a line terminator only if a line is not part
     of a parenthesized expression *)
  parse line_term {
                   let indent_str = String.sub (Lexing.lexeme lexbuf) 1 
                            ((String.length (Lexing.lexeme lexbuf)) - 1)
                   in if is_paren_expr state 
                      then (Lexing.new_line lexbuf; scan state lexbuf)
                      else 
                        match state.last_token with 
                        | COLON -> expected_indent indent_str state
                        | SEMI -> (Lexing.new_line lexbuf; scan state lexbuf)
                        | _ -> line_terminator indent_str state }
      | white+ {scan state lexbuf }
      | "{"  { paren_update {state with brace = state.brace + 1;} LBRACE }
      | "}"  { paren_update {state with brace = state.brace - 1;} RBRACE }
      | "["  { paren_update {state with bracket = state.bracket + 1;} LBRACKET }
      | "]"  { paren_update {state with bracket = state.bracket - 1;} RBRACKET }
      | "("  { paren_update {state with paren = state.paren + 1;} LPAR }
      | ")"  { paren_update {state with paren = state.paren - 1;} RPAR }
      | "."  { simple_update state DOT }
      | "="  { simple_update state EQ }
      | ":=" { simple_update state ASS }
      | ":"  { simple_update state COLON }
      | ","  { simple_update state COMMA }
      | ";"  { simple_update state SEMI }
      | "->" { simple_update state IMPLIES }
      | "<"  { simple_update state LT }
      | "+"  { simple_update state PLUS }
      | "-"  { simple_update state MINUS }
      | eof  { simple_update state EOF }
          (* TODO: Fix types here *)
      | integer as n { {state with last_token = Int ((*int_of_string*) n);},
                       [Int ((*int_of_string*) n)] }
      | uident as var { simple_update state (Uident var) }
      | ident as var {
          let token = 
            match Keywords.find var keyword_map with
            | exception Not_found -> Ident var
            | kwd -> kwd
          in simple_update state token
        }
      | "#" { scan_comment state lexbuf }
      | '"' '"' '"' { simple_update state (scan_string_block [] lexbuf) }
      | '"' { simple_update state (scan_string [] lexbuf) }

and scan_comment state = parse
  newline { scan state lexbuf }
  | _ { scan_comment state lexbuf }

and scan_string_block string_list = parse
  | eof   { raise Unterminated_String }
  | '"' '"' '"' { Str (String.concat "" (List.rev string_list)) }
  (* TODO: Implement escape sequences *)
  | _ as char { scan_string_block ((String.make 1 char) :: string_list) lexbuf } 

and scan_string string_list = parse
  newline { raise Unterminated_String }       
  | eof   { raise Unterminated_String }
  | '"'   { Str (String.concat "" (List.rev string_list)) }
  (* TODO: Implement escape sequences *)
  | _ as char { scan_string ((String.make 1 char) :: string_list) lexbuf } 




{

let get_token : Lexing.lexbuf -> t =
  let state = ref {paren = 0;
                   bracket = 0;
                   brace = 0;
                   indent_level = 0;
                   last_token = EOF} (* Initial state *) in
  let tokens = ref ([] : t list) in (* Buffer of tokens *)
  let rec read (buffer: Lexing.lexbuf) =
    match !tokens with
      [] -> let state', tokens' = scan !state buffer in
           begin
             state := state';
             tokens := tokens';
             read buffer
           end
    | token :: others -> tokens := others; token
  in read

exception Eof


}
