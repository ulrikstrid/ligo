%{
%}

(* Tokens (mirroring thise defined in module LexToken) *)

  (* Literals *)

%token           <LexToken.lexeme> Str
%token (* <(LexToken.lexeme * Hex.t)> *) <LexToken.lexeme> Bytes
%token (*  <(LexToken.lexeme * Z.t)> *)  <LexToken.lexeme> Int
%token (*  <(LexToken.lexeme * Z.t)> *)  <LexToken.lexeme> Float
%token (*  <(LexToken.lexeme * Z.t)> *)  <LexToken.lexeme> Mtz
%token           <LexToken.lexeme> Ident
%token           <LexToken.lexeme> Uident
%token           <LexToken.lexeme> Constr

  (* Symbols *)

%token SEMI        (* ";"   *)
%token COMMA       (* ","   *)
%token LPAR        (* "("   *)
%token RPAR        (* ")"   *)
%token LBRACE      (* "{"   *)
%token RBRACE      (* "}"   *)
%token LBRACKET    (* "["   *)
%token RBRACKET    (* "]"   *)
%token VBAR        (* "|"   *)
%token ARROW       (* "->"  *)
%token ASS         (* ":="  *)
%token EQ          (* "="   *)
%token COLON       (* ":"   *)
%token LT          (* "<"   *)
%token LEQ         (* "<="  *)
%token GT          (* ">"   *)
%token GEQ         (* ">="  *)
%token NEQ         (* "=/=" *)
%token PLUS        (* "+"   *)
%token MINUS       (* "-"   *)
%token MOD         (* "%"   *)
%token SLASH       (* "/"   *)
%token TIMES       (* "*"   *)
%token DOT         (* "."   *)
%token WILD        (* "_"   *)
%token CAT         (* "^"   *)

  (* Keywords *)

%token And         (* "and"        *)
%token Block       (* "block"      *)
%token Def         (* "def"        *)
%token Down        (* "down"       *)
%token Elif        (* "elif"       *)
%token Else        (* "else"       *)
%token Entrypoint  (* "entrypoint" *)
%token Fail        (* "fail"       *)
%token For         (* "for"        *)
%token From        (* "from"       *)
%token If          (* "if"         *)
%token In          (* "in"         *)
%token Is          (* "is"         *)
%token List        (* "list"       *)
%token Nil         (* "nil"        *)
%token Not         (* "not"        *)
%token Of          (* "of"         *)
%token Or          (* "or"         *)
%token Raise       (* "raise"      *)
%token Pass        (* "pass"       *)
%token While       (* "while"      *)
%token With        (* "with"       *)

  (* Data constructors *)
(*
%token C_False     (* "False" *)
%token C_None      (* "None"  *)
%token C_Some      (* "Some"  *)
%token C_True      (* "True"  *)
%token C_Unit      (* "Unit"  *)
*)
  (* Virtual tokens *)

%token START       (* "begin"      *)
%token END         (* "end"        *)
%token EOF

%%
