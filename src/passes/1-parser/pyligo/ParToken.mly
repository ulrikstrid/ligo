%{

%}

(* Tokens (mirroring those defined in module Lexer) *)

  (* Literals *)

%token           <Lexer.lexeme> Str
%token (* <(Lexer.lexeme * Hex.t)> *) <Lexer.lexeme> Bytes
%token (*  <(Lexer.lexeme * Z.t)> *)  <Lexer.lexeme> Int
%token (*  <(Lexer.lexeme * Z.t)> *)  <Lexer.lexeme> Float
%token (*  <(Lexer.lexeme * Z.t)> *)  <Lexer.lexeme> Mtz
%token           <Lexer.lexeme> Ident
%token           <Lexer.lexeme> Uident

  (* Symbols *)

%token SEMI        (* ";"   *)
%token COMMA       (* ","   *)
%token LPAR        (* "("   *)
%token RPAR        (* ")"   *)
%token LBRACE      (* "{"   *)
%token RBRACE      (* "}"   *)
%token LBRACKET    (* "["   *)
%token RBRACKET    (* "]"   *)
(* %token VBAR        (* "|"   *) *)
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
(* %token WILD        (* "_"   *) *)
(* %token CAT         (* "^"   *) *)

  (* Keywords *)

%token And         (* "and"        *)
%token Assert      (* "assert"     *)
%token Def         (* "def"        *)
%token Elif        (* "elif"       *)
%token Else        (* "else"       *)
%token For         (* "for"        *)
(* %token From        (* "from"       *) // No imports yet *)
%token If          (* "if"         *)
%token In          (* "in"         *)
%token Is          (* "is"         *)
%token Not         (* "not"        *)
%token Of          (* "of"         *)
%token Or          (* "or"         *)
%token Raise       (* "raise"      *)
%token Pass        (* "pass"       *)
%token While       (* "while"      *)
(* %token With        (* "with"       *) *)

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
