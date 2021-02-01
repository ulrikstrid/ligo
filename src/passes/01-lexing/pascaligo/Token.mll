(* ocamllex specification for PascaLIGO *)

{
(* START HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Markup = LexerLib.Markup

(* Utility modules *)

module SMap = Map.Make (String)
module SSet = Set.Make (String)

(* TOKENS *)

type lexeme = string

module T =
  struct
    type t =
      (* Literals *)

      String   of lexeme Region.reg
    | Verbatim of lexeme Region.reg
    | Bytes    of (lexeme * Hex.t) Region.reg
    | Int      of (lexeme * Z.t) Region.reg
    | Nat      of (lexeme * Z.t) Region.reg
    | Mutez    of (lexeme * Z.t) Region.reg
    | Ident    of lexeme Region.reg
    | UIdent   of lexeme Region.reg
    | Lang     of lexeme Region.reg Region.reg
    | Attr     of string Region.reg

    (* Symbols *)

    | SEMI     of Region.t  (* ";"   *)
    | COMMA    of Region.t  (* ","   *)
    | LPAR     of Region.t  (* "("   *)
    | RPAR     of Region.t  (* ")"   *)
    | LBRACE   of Region.t  (* "{"   *)
    | RBRACE   of Region.t  (* "}"   *)
    | LBRACKET of Region.t  (* "["   *)
    | RBRACKET of Region.t  (* "]"   *)
    | CONS     of Region.t  (* "#"   *)
    | VBAR     of Region.t  (* "|"   *)
    | ARROW    of Region.t  (* "->"  *)
    | ASS      of Region.t  (* ":="  *)
    | EQ       of Region.t  (* "="   *)
    | COLON    of Region.t  (* ":"   *)
    | LT       of Region.t  (* "<"   *)
    | LE       of Region.t  (* "<="  *)
    | GT       of Region.t  (* ">"   *)
    | GE       of Region.t  (* ">="  *)
    | NE       of Region.t  (* "=/=" *)
    | PLUS     of Region.t  (* "+"   *)
    | MINUS    of Region.t  (* "-"   *)
    | SLASH    of Region.t  (* "/"   *)
    | TIMES    of Region.t  (* "*"   *)
    | DOT      of Region.t  (* "."   *)
    | WILD     of Region.t  (* "_"   *)
    | CARET    of Region.t  (* "^"   *)

    (* Keywords *)

    | And       of Region.t  (* and        *)
    | Begin     of Region.t  (* begin      *)
    | BigMap    of Region.t  (* big_map    *)
    | Block     of Region.t  (* block      *)
    | Case      of Region.t  (* case       *)
    | Const     of Region.t  (* const      *)
    | Contains  of Region.t  (* contains   *)
    | Else      of Region.t  (* else       *)
    | End       of Region.t  (* end        *)
    | False     of Region.t  (* False      *)
    | For       of Region.t  (* for        *)
    | From      of Region.t  (* from       *)
    | Function  of Region.t  (* function   *)
    | If        of Region.t  (* if         *)
    | In        of Region.t  (* in         *)
    | Is        of Region.t  (* is         *)
    | List      of Region.t  (* list       *)
    | Map       of Region.t  (* map        *)
    | Mod       of Region.t  (* mod        *)
    | Module    of Region.t  (* module     *)
    | Nil       of Region.t  (* nil        *)
    | Ctor_None of Region.t  (* None       *)
    | Not       of Region.t  (* not        *)
    | Of        of Region.t  (* of         *)
    | Or        of Region.t  (* or         *)
    | Patch     of Region.t  (* patch      *)
    | Record    of Region.t  (* record     *)
    | Recursive of Region.t  (* recursive  *)
    | Remove    of Region.t  (* remove     *)
    | Set       of Region.t  (* set        *)
    | Ctor_Some of Region.t  (* Some       *)
    | Skip      of Region.t  (* skip       *)
    | Step      of Region.t  (* step       *)
    | Then      of Region.t  (* then       *)
    | To        of Region.t  (* to         *)
    | True      of Region.t  (* True       *)
    | Type      of Region.t  (* type       *)
    | Unit      of Region.t  (* Unit       *)
    | Var       of Region.t  (* var        *)
    | While     of Region.t  (* while      *)
    | With      of Region.t  (* with       *)

    (* Virtual tokens *)

    | EOF of Region.t

    (* Unlexing the tokens *)

    let gen_sym prefix =
      let count = ref 0 in
      fun () -> incr count; prefix ^ string_of_int !count

    let id_sym  = gen_sym "id"
    and uid_sym = gen_sym "U"

    let concrete = function
      (* Literals *)

        "Ident"    -> id_sym ()
      | "UIdent"   -> uid_sym ()
      | "Int"      -> "1"
      | "Nat"      -> "1n"
      | "Mutez"    -> "1mutez"
      | "String"   -> "\"a string\""
      | "Verbatim" -> "{|verbatim|}"
      | "Bytes"    -> "0xAA"
      | "Attr"     -> "[@attr]"
      | "Lang"     -> "[%Michelson"

      (* Symbols *)

      | "SEMI"     -> ";"
      | "COMMA"    -> ","
      | "LPAR"     -> "("
      | "RPAR"     -> ")"
      | "LBRACE"   -> "{"
      | "RBRACE"   -> "}"
      | "LBRACKET" -> "["
      | "RBRACKET" -> "]"
      | "CONS"     -> "#"
      | "VBAR"     -> "|"
      | "ARROW"    -> "->"
      | "ASS"      -> ":="
      | "EQ"       -> "="
      | "COLON"    -> ":"
      | "LT"       -> "<"
      | "LE"       -> "<="
      | "GT"       -> ">"
      | "GE"       -> ">="
      | "NE"       -> "=/="
      | "PLUS"     -> "+"
      | "MINUS"    -> "-"
      | "SLASH"    -> "/"
      | "TIMES"    -> "*"
      | "DOT"      -> "."
      | "WILD"     -> "_"
      | "CARET"    -> "^"

      (* Keywords *)

      | "And"       -> "and"
      | "Begin"     -> "begin"
      | "BigMap"    -> "big_map"
      | "Block"     -> "block"
      | "Case"      -> "case"
      | "Const"     -> "const"
      | "Contains"  -> "contains"
      | "Else"      -> "else"
      | "End"       -> "end"
      | "False"     -> "False"
      | "For"       -> "for"
      | "From"      -> "from"
      | "Function"  -> "function"
      | "If"        -> "if"
      | "In"        -> "in"
      | "Is"        -> "is"
      | "List"      -> "list"
      | "Map"       -> "map"
      | "Mod"       -> "mod"
      | "Module"    -> "module"
      | "Nil"       -> "nil"
      | "Ctor_None" -> "None"
      | "Not"       -> "not"
      | "Of"        -> "of"
      | "Or"        -> "or"
      | "Patch"     -> "patch"
      | "Record"    -> "record"
      | "Recursive" -> "recursive"
      | "Remove"    -> "remove"
      | "Set"       -> "set"
      | "Ctor_Some" -> "Some"
      | "Skip"      -> "skip"
      | "Step"      -> "step"
      | "Then"      -> "then"
      | "To"        -> "to"
      | "True"      -> "True"
      | "Type"      -> "type"
      | "Unit"      -> "Unit"
      | "Var"       -> "var"
      | "While"     -> "while"
      | "With"      -> "with"

      (* Virtual tokens *)

      | "EOF" -> ""

      (* This case should not happen! *)

      | _  -> "\\Unknown" (* Backslash meant to trigger an error *)

    (* Projections *)

    let sprintf = Printf.sprintf

    type token = t

    let proj_token = function
        (* Literals *)

      String Region.{region; value} ->
        region, sprintf "String %S" value
    | Verbatim Region.{region; value} ->
        region, sprintf "Verbatim %S" value
    | Bytes Region.{region; value = s,b} ->
        region,
        sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
    | Int Region.{region; value = s,n} ->
        region, sprintf "Int (%S, %s)" s (Z.to_string n)
    | Nat Region.{region; value = s,n} ->
        region, sprintf "Nat (%S, %s)" s (Z.to_string n)
    | Mutez Region.{region; value = s,n} ->
        region, sprintf "Mutez (%S, %s)" s (Z.to_string n)
    | Ident Region.{region; value} ->
        region, sprintf "Ident %S" value
    | UIdent Region.{region; value} ->
        region, sprintf "UIdent %S" value
    | Attr Region.{region; value} ->
        region, sprintf "Attr %S" value
    | Lang Region.{region; value} ->
        region, sprintf "Lang %S" (value.Region.value)

    (* Symbols *)

    | SEMI     region -> region, "SEMI"
    | COMMA    region -> region, "COMMA"
    | LPAR     region -> region, "LPAR"
    | RPAR     region -> region, "RPAR"
    | LBRACE   region -> region, "LBRACE"
    | RBRACE   region -> region, "RBRACE"
    | LBRACKET region -> region, "LBRACKET"
    | RBRACKET region -> region, "RBRACKET"
    | CONS     region -> region, "CONS"
    | VBAR     region -> region, "VBAR"
    | ARROW    region -> region, "ARROW"
    | ASS      region -> region, "ASS"
    | EQ       region -> region, "EQ"
    | COLON    region -> region, "COLON"
    | LT       region -> region, "LT"
    | LE       region -> region, "LE"
    | GT       region -> region, "GT"
    | GE       region -> region, "GE"
    | NE       region -> region, "NE"
    | PLUS     region -> region, "PLUS"
    | MINUS    region -> region, "MINUS"
    | SLASH    region -> region, "SLASH"
    | TIMES    region -> region, "TIMES"
    | DOT      region -> region, "DOT"
    | WILD     region -> region, "WILD"
    | CARET    region -> region, "CARET"

    (* Keywords *)

    | And        region -> region, "And"
    | Begin      region -> region, "Begin"
    | BigMap     region -> region, "BigMap"
    | Block      region -> region, "Block"
    | Case       region -> region, "Case"
    | Const      region -> region, "Const"
    | Contains   region -> region, "Contains"
    | Else       region -> region, "Else"
    | End        region -> region, "End"
    | False      region -> region, "False"
    | For        region -> region, "For"
    | From       region -> region, "From"
    | Function   region -> region, "Function"
    | Recursive  region -> region, "Recursive"
    | If         region -> region, "If"
    | In         region -> region, "In"
    | Is         region -> region, "Is"
    | List       region -> region, "List"
    | Map        region -> region, "Map"
    | Mod        region -> region, "Mod"
    | Module     region -> region, "Module"
    | Nil        region -> region, "Nil"
    | Ctor_None  region -> region, "Ctor_None"
    | Not        region -> region, "Not"
    | Of         region -> region, "Of"
    | Or         region -> region, "Or"
    | Patch      region -> region, "Patch"
    | Record     region -> region, "Record"
    | Remove     region -> region, "Remove"
    | Set        region -> region, "Set"
    | Ctor_Some  region -> region, "Ctor_Some"
    | Skip       region -> region, "Skip"
    | Step       region -> region, "Step"
    | Then       region -> region, "Then"
    | To         region -> region, "To"
    | True       region -> region, "True"
    | Type       region -> region, "Type"
    | Unit       region -> region, "Unit"
    | Var        region -> region, "Var"
    | While      region -> region, "While"
    | With       region -> region, "With"

    (* Virtual tokens *)

    | EOF region -> region, "EOF"

    (* From tokens to lexemes *)

    let to_lexeme = function
      (* Literals *)

      String s   -> sprintf "%S" (String.escaped s.Region.value)
    | Verbatim v -> String.escaped v.Region.value
    | Bytes b    -> fst b.Region.value
    | Int i
    | Nat i
    | Mutez i    -> fst i.Region.value
    | Ident id
    | UIdent id  -> id.Region.value
    | Attr a     -> a.Region.value
    | Lang lang  -> Region.(lang.value.value)

    (* Symbols *)

    | SEMI     _ -> ";"
    | COMMA    _ -> ","
    | LPAR     _ -> "("
    | RPAR     _ -> ")"
    | LBRACE   _ -> "{"
    | RBRACE   _ -> "}"
    | LBRACKET _ -> "["
    | RBRACKET _ -> "]"
    | CONS     _ -> "#"
    | VBAR     _ -> "|"
    | ARROW    _ -> "->"
    | ASS      _ -> ":="
    | EQ       _ -> "="
    | COLON    _ -> ":"
    | LT       _ -> "<"
    | LE       _ -> "<="
    | GT       _ -> ">"
    | GE       _ -> ">="
    | NE       _ -> "=/="
    | PLUS     _ -> "+"
    | MINUS    _ -> "-"
    | SLASH    _ -> "/"
    | TIMES    _ -> "*"
    | DOT      _ -> "."
    | WILD     _ -> "_"
    | CARET    _ -> "^"

    (* Keywords *)

    | And        _ -> "and"
    | Begin      _ -> "begin"
    | BigMap     _ -> "big_map"
    | Block      _ -> "block"
    | Case       _ -> "case"
    | Const      _ -> "const"
    | Contains   _ -> "contains"
    | Else       _ -> "else"
    | End        _ -> "end"
    | False      _ -> "False"
    | For        _ -> "for"
    | From       _ -> "from"
    | Function   _ -> "function"
    | If         _ -> "if"
    | In         _ -> "in"
    | Is         _ -> "is"
    | List       _ -> "list"
    | Map        _ -> "map"
    | Mod        _ -> "mod"
    | Module     _ -> "module"
    | Nil        _ -> "nil"
    | Ctor_None  _ -> "None"
    | Not        _ -> "not"
    | Of         _ -> "of"
    | Or         _ -> "or"
    | Patch      _ -> "patch"
    | Record     _ -> "record"
    | Recursive  _ -> "recursive"
    | Remove     _ -> "remove"
    | Set        _ -> "set"
    | Ctor_Some  _ -> "Some"
    | Skip       _ -> "skip"
    | Step       _ -> "step"
    | Then       _ -> "then"
    | To         _ -> "to"
    | True       _ -> "True"
    | Type       _ -> "type"
    | Unit       _ -> "Unit"
    | Var        _ -> "var"
    | While      _ -> "while"
    | With       _ -> "with"

    (* Virtual tokens *)

    | EOF _ -> ""

    (* Converting a token to a string *)

    let to_string ~offsets mode token =
      let region, val_str = proj_token token in
      let reg_str = region#compact ~offsets mode
      in sprintf "%s: %s" reg_str val_str

    (* Extracting the source region of a token *)

    let to_region token = proj_token token |> fst

    (* Keywords *)

    let keywords = [
      (fun reg -> And        reg); (* Boolean operator *)
      (fun reg -> Begin      reg);
      (fun reg -> BigMap     reg);
      (fun reg -> Block      reg);
      (fun reg -> Case       reg);
      (fun reg -> Const      reg);
      (fun reg -> Contains   reg);
      (fun reg -> Else       reg);
      (fun reg -> End        reg);
      (fun reg -> For        reg);
      (fun reg -> From       reg);
      (fun reg -> Function   reg);
      (fun reg -> False      reg); (* Data constructor *)
      (fun reg -> If         reg);
      (fun reg -> In         reg);
      (fun reg -> Is         reg);
      (fun reg -> List       reg);
      (fun reg -> Map        reg);
      (fun reg -> Mod        reg); (* Boolean operator *)
      (fun reg -> Module     reg);
      (fun reg -> Nil        reg); (* Data constructor *)
      (fun reg -> Not        reg); (* Boolean operator *)
      (fun reg -> Ctor_None  reg); (* Data constructor *)
      (fun reg -> Of         reg);
      (fun reg -> Or         reg); (* Boolean operator *)
      (fun reg -> Patch      reg);
      (fun reg -> Record     reg);
      (fun reg -> Recursive  reg);
      (fun reg -> Remove     reg);
      (fun reg -> Set        reg);
      (fun reg -> Skip       reg); (* Instruction *)
      (fun reg -> Step       reg);
      (fun reg -> Then       reg);
      (fun reg -> To         reg);
      (fun reg -> True       reg); (* Data constructor *)
      (fun reg -> Type       reg);
      (fun reg -> Unit       reg); (* Data constructor *)
      (fun reg -> Ctor_Some  reg); (* Data constructor *)
      (fun reg -> Var        reg);
      (fun reg -> While      reg);
      (fun reg -> With       reg)
    ]

    (* Reserved identifiers *)

    let reserved = SSet.empty

    (* Making the lexicon up *)

    let add map (key, value) = SMap.add key value map

    let mk_map mk_key list =
      let apply map value = add map (mk_key value, value)
      in List.fold_left apply SMap.empty list

    type lexis = {
      kwd : (Region.t -> token) SMap.t;
      res : SSet.t
    }

    let lexicon : lexis =
      let build = mk_map (fun f -> to_lexeme (f Region.ghost))
      in {kwd = build keywords; res = reserved}

    (* Keywords *)

    type kwd_err = Invalid_keyword

    let mk_kwd ident region =
      match SMap.find_opt ident lexicon.kwd with
        Some mk_kwd -> Ok (mk_kwd region)
      |        None -> Error Invalid_keyword

    (* Identifiers *)

    type ident_err = Reserved_name

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let small   = ['a'-'z']
let capital = ['A'-'Z']
let letter  = small | capital
let digit   = ['0'-'9']
let ident   = small (letter | '_' | digit)*
let uident  = capital (letter | '_' | digit)*

(* Rules *)

rule scan_ident region lexicon = parse
  (ident as value) eof {
    if   SSet.mem value lexicon.res
    then Error Reserved_name
    else Ok (match SMap.find_opt value lexicon.kwd with
               Some mk_kwd -> mk_kwd region (* Ident which are keywords *)
             |        None -> Ident Region.{region; value}) }

and scan_uident region lexicon = parse
  (uident as value) eof {
    match SMap.find_opt value lexicon.kwd with
      Some mk_kwd -> mk_kwd region (* UIdent which are keywords *)
    |        None -> UIdent Region.{region; value} }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

    (* Smart constructors (injections) *)

    let mk_string lexeme region =
      String Region.{region; value=lexeme}

    let mk_verbatim lexeme region =
      Verbatim Region.{region; value=lexeme}

    let mk_bytes lexeme region =
      let norm = Str.(global_replace (regexp "_") "" lexeme) in
      let value = lexeme, `Hex norm
      in Bytes Region.{region; value}

    type int_err = Non_canonical_zero

    let mk_int lexeme region =
      let z =
        Str.(global_replace (regexp "_") "" lexeme) |> Z.of_string
      in if   Z.equal z Z.zero && lexeme <> "0"
         then Error Non_canonical_zero
         else Ok (Int Region.{region; value = lexeme,z})

    type nat_err =
      Invalid_natural
    | Non_canonical_zero_nat

    let mk_nat lexeme region =
      match String.index_opt lexeme 'n' with
        None -> Error Invalid_natural
      | Some _ ->
          let z =
            Str.(global_replace (regexp "_") "" lexeme) |>
              Str.(global_replace (regexp "n") "") |>
              Z.of_string in
          if   Z.equal z Z.zero && lexeme <> "0n"
          then Error Non_canonical_zero_nat
          else Ok (Nat Region.{region; value = lexeme,z})

    let mk_mutez lexeme region =
      let z = Str.(global_replace (regexp "_") "" lexeme) |>
                Str.(global_replace (regexp "mutez") "") |>
                Z.of_string in
      if   Z.equal z Z.zero && lexeme <> "0mutez"
      then Error Non_canonical_zero
      else Ok (Mutez Region.{region; value = lexeme, z})

    let eof region = EOF region

    type sym_err = Invalid_symbol

    let mk_sym lexeme region =
      match lexeme with
        (* Lexemes in common with all concrete syntaxes *)

        ";"   -> Ok (SEMI     region)
      | ","   -> Ok (COMMA    region)
      | "("   -> Ok (LPAR     region)
      | ")"   -> Ok (RPAR     region)
      | "["   -> Ok (LBRACKET region)
      | "]"   -> Ok (RBRACKET region)
      | "{"   -> Ok (LBRACE   region)
      | "}"   -> Ok (RBRACE   region)
      | "="   -> Ok (EQ       region)
      | ":"   -> Ok (COLON    region)
      | "|"   -> Ok (VBAR     region)
      | "."   -> Ok (DOT      region)
      | "_"   -> Ok (WILD     region)
      | "+"   -> Ok (PLUS     region)
      | "-"   -> Ok (MINUS    region)
      | "*"   -> Ok (TIMES    region)
      | "/"   -> Ok (SLASH    region)
      | "<"   -> Ok (LT       region)
      | "<="  -> Ok (LE       region)
      | ">"   -> Ok (GT       region)
      | ">="  -> Ok (GE       region)

      (* Lexemes specific to PascaLIGO *)

      | "^"   -> Ok (CARET    region)
      | "->"  -> Ok (ARROW    region)
      | "=/=" -> Ok (NE       region)
      | "#"   -> Ok (CONS     region)
      | ":="  -> Ok (ASS      region)

      (* Invalid symbols *)

      | _ -> Error Invalid_symbol

    (* Identifiers (starting with a smallcase letter) *)

    let mk_ident lexeme region =
      Lexing.from_string lexeme |> scan_ident region lexicon

    (* UIdent (starting with an uppercase letter) *)

    let mk_uident lexeme region =
      Lexing.from_string lexeme |> scan_uident region lexicon

    (* Attributes *)

    let mk_attr lexeme region = Attr Region.{value=lexeme; region}

    (* Code injection *)

    let mk_lang lang region = Lang Region.{value=lang; region}

    (* Predicates *)

    let is_string   = function String _   -> true | _ -> false
    let is_verbatim = function Verbatim _ -> true | _ -> false
    let is_bytes    = function Bytes _    -> true | _ -> false
    let is_int      = function Int _      -> true | _ -> false
    let is_nat      = function Nat _      -> true | _ -> false
    let is_mutez    = function Mutez _    -> true | _ -> false
    let is_ident    = function Ident _    -> true | _ -> false
    let is_uident   = function UIdent _   -> true | _ -> false
    let is_lang     = function Lang _     -> true | _ -> false
    let is_minus    = function MINUS _    -> true | _ -> false
    let is_eof      = function EOF _      -> true | _ -> false

    let is_hexa = function
      UIdent Region.{value="A"|"B"|"C"|"D"|"E"|"F"
                          |"a"|"b"|"c"|"d"|"e"|"f"; _} -> true
    | _ -> false

    let is_sym = function
      SEMI _
    | COMMA _
    | LPAR _
    | RPAR _
    | LBRACE _
    | RBRACE _
    | LBRACKET _
    | RBRACKET _
    | CONS _
    | VBAR _
    | ARROW _
    | ASS _
    | EQ _
    | COLON _
    | LT _
    | LE _
    | GT _
    | GE _
    | NE _
    | PLUS _
    | MINUS _
    | SLASH _
    | TIMES _
    | DOT _
    | WILD _
    | CARET _ -> true
    | _ -> false
  end

include T

module type S = module type of T

(* END TRAILER *)
}
