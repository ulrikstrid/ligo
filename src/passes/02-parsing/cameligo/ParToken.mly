%{
module Token = Lexing_cameligo.Token
%}

(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>"
%token                  <string ExtRegion.reg> String    "<string>"
%token                  <string ExtRegion.reg> Verbatim  "<verbatim>"
%token  <(Token.lexeme * Hex.t) ExtRegion.reg> Bytes     "<bytes>"
%token          <(string * Z.t) ExtRegion.reg> Int       "<int>"
%token          <(string * Z.t) ExtRegion.reg> Nat       "<nat>"
%token          <(string * Z.t) ExtRegion.reg> Mutez     "<mutez>"
%token                  <string ExtRegion.reg> Ident     "<ident>"
%token                  <string ExtRegion.reg> Constr    "<constr>"
%token                  <string ExtRegion.reg> Attr      "[@attr]"
%token <Token.lexeme ExtRegion.reg ExtRegion.reg> Lang      "[%lang"

  (* Symbols *)

%token <ExtRegion.t> MINUS   "-"
%token <ExtRegion.t> PLUS    "+"
%token <ExtRegion.t> SLASH   "/"
%token <ExtRegion.t> TIMES   "*"

%token <ExtRegion.t> LPAR     "("
%token <ExtRegion.t> RPAR     ")"
%token <ExtRegion.t> LBRACKET "["
%token <ExtRegion.t> RBRACKET "]"
%token <ExtRegion.t> LBRACE   "{"
%token <ExtRegion.t> RBRACE   "}"

%token <ExtRegion.t> ARROW "->"
%token <ExtRegion.t> CONS  "::"
%token <ExtRegion.t> CARET "^"
(*%token <ExtRegion.t> APPEND "@" *)
%token <ExtRegion.t> DOT   "."

%token <ExtRegion.t> COMMA ","
%token <ExtRegion.t> SEMI  ";"
%token <ExtRegion.t> COLON ":"
%token <ExtRegion.t> VBAR  "|"

%token <ExtRegion.t> WILD  "_"

%token <ExtRegion.t> EQ "="
%token <ExtRegion.t> NE "<>"
%token <ExtRegion.t> LT "<"
%token <ExtRegion.t> GT ">"
%token <ExtRegion.t> LE "<="
%token <ExtRegion.t> GE ">="

%token <ExtRegion.t> BOOL_OR  "||"
%token <ExtRegion.t> BOOL_AND "&&"

 (* Keywords *)

(*%token And*)
%token <ExtRegion.t> Begin  "begin"
%token <ExtRegion.t> Else   "else"
%token <ExtRegion.t> End    "end"
%token <ExtRegion.t> False  "false"
%token <ExtRegion.t> Fun    "fun"
%token <ExtRegion.t> Rec    "rec"
%token <ExtRegion.t> If     "if"
%token <ExtRegion.t> In     "in"
%token <ExtRegion.t> Let    "let"
%token <ExtRegion.t> Match  "match"
%token <ExtRegion.t> Mod    "mod"
%token <ExtRegion.t> Not    "not"
%token <ExtRegion.t> Of     "of"
%token <ExtRegion.t> Or     "or"
%token <ExtRegion.t> Then   "then"
%token <ExtRegion.t> True   "true"
%token <ExtRegion.t> Type   "type"
%token <ExtRegion.t> With   "with"
%token <ExtRegion.t> Module "module"
%token <ExtRegion.t> Struct "struct"

  (* Data constructors *)

%token <ExtRegion.t> C_None "None"
%token <ExtRegion.t> C_Some "Some"

  (* Virtual tokens *)

%token <ExtRegion.t> EOF

%%
