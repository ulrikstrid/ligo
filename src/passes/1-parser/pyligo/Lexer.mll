{

module Keywords = Map.Make(String)
module Token = LexToken


let keyword_list = [
  ("def", Token.Def);
  ("if",  Token.If);
  ("elif", Token.Elif);
  ("else", Token.Else);
  ("for",  Token.For);
  ("in",  Token.In);
  ("raise", Token.Raise);
  ("return", Token.Return);
]

exception NonPair of string

let keyword_map =
  let add_pair
      (kwd_map: Token.t Map.Make(String).t)
      (pair: (Keywords.key * Token.t)) =
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
  mutable last_token: Token.t;
}

exception Whitespace_Error
exception Unexpected_Indent
exception Unterminated_String

let is_paren_expr state =
  if state.paren >= 1 then true
  else if state.bracket >= 1 then true
  else if state.brace >= 1 then true
  else false

let indent_level indent_str =
  if (String.length indent_str) mod 4 != 0
  then raise Whitespace_Error
  else (String.length indent_str) / 4

let expected_indent indent_str state = 
  if indent_level indent_str = (state.indent_level + 1)
  then {state with indent_level = state.indent_level + 1; 
                   last_token = Token.START; }, [Token.START]
  else raise Unexpected_Indent

let line_terminator indent_str state = 
  let rec list_of value n lst = 
    if n <= 0 then lst else list_of value (n - 1) (value :: lst)
  in let deindent_level = (state.indent_level - (indent_level indent_str)) in
  if state.last_token = Token.END 
  then state, []
  else if indent_level indent_str > state.indent_level
  then raise Unexpected_Indent
  else if state.last_token = Token.SEMI
  then {state with last_token = Token.END;}, [Token.END]
  else if indent_level indent_str = state.indent_level
  then {state with last_token = Token.SEMI}, [Token.SEMI]
  else {state with indent_level = indent_level indent_str;
                   last_token = Token.END; }, [Token.SEMI] @ (list_of Token.END deindent_level [])

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
                      else if state.last_token = Token.COLON 
                      then expected_indent indent_str state
                      else line_terminator indent_str state }
      | white+ {scan state lexbuf }
      | "{"  { {state with last_token = Token.LBRACE;
                           brace = state.brace + 1;}, [Token.LBRACE]}
      | "}"  { {state with last_token = Token.RBRACE;
                           brace = state.brace - 1;}, [Token.RBRACE]}
      | "["  { {state with last_token = Token.LBRACKET;
                           bracket = state.bracket + 1;}, [Token.LBRACKET]}
      | "]"  { {state with last_token = Token.RBRACKET;
                           bracket = state.bracket - 1;}, [Token.RBRACKET]}
      | "("  { {state with last_token = Token.LPAR; 
                           paren = state.paren + 1;}, [Token.LPAR]}
      | ")"  { {state with last_token = Token.RPAR;
                           paren = state.paren - 1;}, [Token.RPAR]}
      | "."  { {state with last_token = Token.DOT;}, [Token.DOT]}
      | "="  { {state with last_token = Token.EQ;}, [Token.EQ]}
      | ":=" { {state with last_token = Token.ASS;}, [Token.ASS]}
      | ":"  { {state with last_token = Token.COLON;}, [Token.COLON]}
      | ","  { {state with last_token = Token.COMMA;}, [Token.COMMA]}
      | ";"  { {state with last_token = Token.SEMI;}, [Token.SEMI]}
      | "->" { {state with last_token = Token.IMPLIES;}, [Token.IMPLIES]}
      | "<"  { {state with last_token = Token.LT;}, [Token.LT]}
      | "+"  { {state with last_token = Token.PLUS;}, [Token.PLUS]}
      | "-"  { {state with last_token = Token.MINUS;}, [Token.MINUS]}
      | eof  { {state with last_token = Token.EOF;}, [Token.EOF]}
          (* TODO: Fix types here *)
      | integer as n { {state with last_token = Token.Int ((*int_of_string*) n);},
                       [Token.Int ((*int_of_string*) n)] }
      | uident as var { { state with last_token = Token.Ident var;}, [Token.Uident var] }
      | ident as var {
          match Keywords.find var keyword_map with
          | exception Not_found -> {state with last_token = Token.Ident var;}, [Token.Ident var]
          | kwd -> {state with last_token = kwd;}, [kwd]
        }
      | "#" { scan_comment state lexbuf }
      | '"' '"' '"' { state, scan_string_block [] lexbuf }
      | '"' { state, scan_string [] lexbuf }

and scan_comment state = parse
  newline { scan state lexbuf }
  | _ { scan_comment state lexbuf }

and scan_string string_list = parse
  newline { raise Unterminated_String }       
  | eof   { raise Unterminated_String }
  | '"'   { [Token.Str (String.concat "" string_list)] }
  (* TODO: Implement escape sequences *)
  | _ as char { scan_string ((String.make 1 char) :: string_list) lexbuf } 

and scan_string_block string_list = parse
  | eof   { raise Unterminated_String }
  | '"' '"' '"' { [Token.Str (String.concat "" string_list)] }
  (* TODO: Implement escape sequences *)
  | _ as char { scan_string_block ((String.make 1 char) :: string_list) lexbuf } 


{

let get_token : Lexing.lexbuf -> Token.t =
  let state = ref {paren = 0;
                   bracket = 0;
                   brace = 0;
                   indent_level = 0;
                   last_token = Token.EOF} (* Initial state *) in
  let tokens = ref ([] : Token.t list) in (* Buffer of tokens *)
  let rec read (buffer: Lexing.lexbuf) =
    match !tokens with
      [] -> let state', tokens' = scan !state buffer in
           begin
             state := state';
             tokens := tokens';
             read buffer
           end
    | token::others -> tokens := others; token
  in read

exception Eof


}
