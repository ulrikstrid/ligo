module Core      = LexerLib.Core
module Directive = LexerLib.Directive
module Region    = Simple_utils.Region
module ExtRegion = Stage_common.Ext_region

let ok x = Stdlib.Ok x

type lexeme = Token.lexeme

type t =
    (* Preprocessing directives *)

    Directive of Directive.t

    (* Literals *)

  | String   of lexeme ExtRegion.reg
  | Verbatim of lexeme ExtRegion.reg
  | Bytes    of (lexeme * Hex.t) ExtRegion.reg
  | Int      of (lexeme * Z.t) ExtRegion.reg
  | Nat      of (lexeme * Z.t) ExtRegion.reg
  | Mutez    of (lexeme * Z.t) ExtRegion.reg
  | Ident    of lexeme ExtRegion.reg
  | Constr   of lexeme ExtRegion.reg
  | Lang     of lexeme ExtRegion.reg ExtRegion.reg
  | Attr     of string ExtRegion.reg

  (* Symbols *)

  | ARROW    of ExtRegion.t  (* "->" *)
  | CONS     of ExtRegion.t  (* "::" *)
  | CARET    of ExtRegion.t  (* "^"  *)
  | MINUS    of ExtRegion.t  (* "-"  *)
  | PLUS     of ExtRegion.t  (* "+"  *)
  | SLASH    of ExtRegion.t  (* "/"  *)
  | TIMES    of ExtRegion.t  (* "*"  *)
  | LPAR     of ExtRegion.t  (* "("  *)
  | RPAR     of ExtRegion.t  (* ")"  *)
  | LBRACKET of ExtRegion.t  (* "["  *)
  | RBRACKET of ExtRegion.t  (* "]"  *)
  | LBRACE   of ExtRegion.t  (* "{"  *)
  | RBRACE   of ExtRegion.t  (* "}"  *)
  | COMMA    of ExtRegion.t  (* ","  *)
  | SEMI     of ExtRegion.t  (* ";"  *)
  | VBAR     of ExtRegion.t  (* "|"  *)
  | COLON    of ExtRegion.t  (* ":"  *)
  | DOT      of ExtRegion.t  (* "."  *)
  | WILD     of ExtRegion.t  (*  "_" *)
  | EQ       of ExtRegion.t  (* "="  *)
  | NE       of ExtRegion.t  (* "<>" *)
  | LT       of ExtRegion.t  (* "<"  *)
  | GT       of ExtRegion.t  (* ">"  *)
  | LE       of ExtRegion.t  (* "<=" *)
  | GE       of ExtRegion.t  (* ">=" *)
  | BOOL_OR  of ExtRegion.t  (* "||" *)
  | BOOL_AND of ExtRegion.t  (* "&&" *)

  (* Keywords *)

  | Begin     of ExtRegion.t  (* begin *)
  | Else      of ExtRegion.t  (* else  *)
  | End       of ExtRegion.t  (* end   *)
  | False     of ExtRegion.t  (* false *)
  | Fun       of ExtRegion.t  (* fun   *)
  | Rec       of ExtRegion.t  (* rec   *)
  | If        of ExtRegion.t  (* if    *)
  | In        of ExtRegion.t  (* in    *)
  | Let       of ExtRegion.t  (* let   *)
  | Match     of ExtRegion.t  (* match *)
  | Mod       of ExtRegion.t  (* mod   *)
  | Not       of ExtRegion.t  (* not   *)
  | Of        of ExtRegion.t  (* of    *)
  | Or        of ExtRegion.t  (* or    *)
  | Then      of ExtRegion.t  (* then  *)
  | True      of ExtRegion.t  (* true  *)
  | Type      of ExtRegion.t  (* type  *)
  | With      of ExtRegion.t  (* with  *)
  | Module    of ExtRegion.t  (* module *)
  | Struct    of ExtRegion.t  (* strcut *)

  (* Data constructors *)

  | C_None  of ExtRegion.t  (* None *)
  | C_Some  of ExtRegion.t  (* Some *)

  (* Virtual tokens *)

  | EOF of ExtRegion.t

let to_t = function
  Token.ARROW region -> ARROW {t_region=region;markup=[]}
| CONS region  -> CONS {t_region=region;markup=[]}
| CARET region -> CARET {t_region=region;markup=[]}
| MINUS region -> MINUS {t_region=region;markup=[]}
| PLUS region  -> PLUS {t_region=region;markup=[]}
| SLASH region -> SLASH {t_region=region;markup=[]}
| TIMES region -> TIMES {t_region=region;markup=[]}
| LPAR region -> LPAR {t_region=region;markup=[]}
| RPAR region -> RPAR {t_region=region;markup=[]}
| LBRACKET region -> LBRACKET {t_region=region;markup=[]}
| RBRACKET region -> RBRACKET {t_region=region;markup=[]}
| LBRACE region -> LBRACE {t_region=region;markup=[]}
| RBRACE region -> RBRACE {t_region=region;markup=[]}
| COMMA region -> COMMA {t_region=region;markup=[]}
| SEMI region -> SEMI {t_region=region;markup=[]}
| VBAR region -> VBAR {t_region=region;markup=[]}
| COLON region -> COLON {t_region=region;markup=[]}
| DOT region -> DOT {t_region=region;markup=[]}
| WILD region -> WILD {t_region=region;markup=[]}
| EQ region -> EQ {t_region=region;markup=[]}
| NE region -> NE {t_region=region;markup=[]}
| LT region -> LT {t_region=region;markup=[]}
| GT region -> GT {t_region=region;markup=[]}
| LE region -> LE {t_region=region;markup=[]}
| GE region -> GE {t_region=region;markup=[]}
| BOOL_OR region -> BOOL_OR {t_region=region;markup=[]}
| BOOL_AND region -> BOOL_AND {t_region=region;markup=[]}
| Begin region -> Begin {t_region=region;markup=[]}
| Else region -> Else {t_region=region;markup=[]}
| End region -> End {t_region=region;markup=[]}
| False region -> False {t_region=region;markup=[]}
| Fun region -> Fun {t_region=region;markup=[]}
| Rec region -> Rec {t_region=region;markup=[]}
| If region -> If {t_region=region;markup=[]}
| In region -> In {t_region=region;markup=[]}
| Let region -> Let {t_region=region;markup=[]}
| Match region -> Match {t_region=region;markup=[]}
| Mod region -> Mod {t_region=region;markup=[]}
| Not region -> Not {t_region=region;markup=[]}
| Of region -> Of {t_region=region;markup=[]}
| Or region -> Or {t_region=region;markup=[]}
| Then region -> Then {t_region=region;markup=[]}
| True region -> True {t_region=region;markup=[]}
| Type region -> Type {t_region=region;markup=[]}
| With region -> With {t_region=region;markup=[]}
| Module region -> Module {t_region=region;markup=[]}
| Struct region -> Struct {t_region=region;markup=[]}
| C_None region -> C_None {t_region=region;markup=[]}
| C_Some region -> C_Some {t_region=region;markup=[]}
| EOF region -> EOF {t_region=region;markup=[]}
| String i -> String {region={t_region = i.region; markup=[]}; value= i.value}
| Verbatim i -> Verbatim {region={t_region = i.region; markup=[]}; value= i.value}
| Bytes i -> Bytes {region={t_region = i.region; markup=[]}; value= i.value}
| Int i -> Int {region={t_region = i.region; markup=[]}; value= i.value}
| Nat i -> Nat {region={t_region = i.region; markup=[]}; value= i.value}
| Mutez i -> Mutez {region={t_region = i.region; markup=[]}; value= i.value}
| Ident i -> Ident {region={t_region = i.region; markup=[]}; value= i.value}
| Constr i -> Constr {region={t_region = i.region; markup=[]}; value= i.value}
| Lang i -> 
  let region = ExtRegion.{t_region=i.region; markup = []} in
  Lang {region; value = {region; value=i.value.value}}
| Attr i -> Attr {region={t_region = i.region; markup=[]}; value= i.value}
| Directive (Linemarker d) -> Directive (Linemarker d)

let to_ext_region_t = function
  ARROW region -> region
| CONS region  -> region
| CARET region -> region
| MINUS region -> region
| PLUS region  -> region
| SLASH region -> region
| TIMES region -> region
| RPAR region -> region
| LPAR region -> region
| LBRACKET region -> region
| RBRACKET region -> region
| LBRACE region -> region
| RBRACE region -> region
| COMMA region -> region
| SEMI region -> region
| VBAR region -> region
| COLON region -> region
| DOT region -> region
| WILD region -> region
| EQ region -> region
| NE region -> region
| LT region -> region
| GT region -> region
| LE region -> region
| GE region -> region
| BOOL_OR region -> region
| BOOL_AND region -> region
| Begin region -> region
| Else region -> region
| End region -> region
| False region -> region
| Fun region -> region
| Rec region -> region
| If region -> region
| In region -> region
| Let region -> region
| Match region -> region
| Mod region -> region
| Not region -> region
| Of region -> region
| Or region -> region
| Then region -> region
| True region -> region
| Type region -> region
| With region -> region
| Module region -> region
| Struct region -> region
| C_None region -> region
| C_Some region -> region
| EOF region -> region
| String i -> i.region
| Verbatim i -> i.region
| Bytes i -> i.region
| Int i -> i.region
| Nat i -> i.region
| Mutez i -> i.region
| Ident i -> i.region
| Constr i -> i.region
| Lang i -> i.region
| Attr i -> i.region
| Directive (Linemarker d) -> ExtRegion.make d.region []

let set_markup: ExtRegion.markup list -> t -> t = fun markup token ->
  let set_markup token markup = 
    ExtRegion.{token with markup=markup}
  in
  match token with 
    ARROW token -> ARROW (set_markup token markup)
  | CONS token  -> CONS (set_markup token markup)
  | CARET token -> CARET (set_markup token markup)
  | MINUS token -> MINUS (set_markup token markup)
  | PLUS token  -> PLUS (set_markup token markup)
  | SLASH token -> SLASH (set_markup token markup)
  | TIMES token -> TIMES (set_markup token markup)
  | LPAR token -> LPAR (set_markup token markup)
  | RPAR token -> RPAR (set_markup token markup)
  | LBRACKET token -> LBRACKET (set_markup token markup)
  | RBRACKET token -> RBRACKET (set_markup token markup)
  | LBRACE token -> LBRACE (set_markup token markup)
  | RBRACE token -> RBRACE (set_markup token markup)
  | COMMA token -> COMMA (set_markup token markup)
  | SEMI token -> SEMI (set_markup token markup)
  | VBAR token -> VBAR (set_markup token markup)
  | COLON token -> COLON (set_markup token markup)
  | DOT token -> DOT (set_markup token markup)
  | WILD token -> WILD (set_markup token markup)
  | EQ token -> EQ (set_markup token markup)
  | NE token -> NE (set_markup token markup)
  | LT token -> LT (set_markup token markup)
  | GT token -> GT (set_markup token markup)
  | LE token -> LE (set_markup token markup)
  | GE token -> GE (set_markup token markup)
  | BOOL_OR token -> BOOL_OR (set_markup token markup)
  | BOOL_AND token -> BOOL_AND (set_markup token markup)
  | Begin token -> Begin (set_markup token markup)
  | Else token -> Else (set_markup token markup)
  | End token -> End (set_markup token markup)
  | False token -> False (set_markup token markup)
  | Fun token -> Fun (set_markup token markup)
  | Rec token -> Rec (set_markup token markup)
  | If token -> If (set_markup token markup)
  | In token -> In (set_markup token markup)
  | Let token -> Let (set_markup token markup)
  | Match token -> Match (set_markup token markup)
  | Mod token -> Mod (set_markup token markup)
  | Not token -> Not (set_markup token markup)
  | Of token -> Of (set_markup token markup)
  | Or token -> Or (set_markup token markup)
  | Then token -> Then (set_markup token markup)
  | True token -> True (set_markup token markup)
  | Type token -> Type (set_markup token markup)
  | With token -> With (set_markup token markup)
  | Module token -> Module (set_markup token markup)
  | Struct token -> Struct (set_markup token markup)
  | C_None token -> C_None (set_markup token markup)
  | C_Some token -> C_Some (set_markup token markup)
  | EOF token -> EOF (set_markup token markup)
  | String i -> String {region = (set_markup i.region markup); value=i.value}
  | Verbatim i -> Verbatim {region = (set_markup i.region markup); value=i.value}
  | Bytes i -> Bytes {region = (set_markup i.region markup); value=i.value}
  | Int i -> Int {region = (set_markup i.region markup); value=i.value}
  | Nat i -> Nat {region = (set_markup i.region markup); value=i.value}
  | Mutez i -> Mutez {region = (set_markup i.region markup); value=i.value}
  | Ident i -> Ident {region = (set_markup i.region markup); value=i.value}
  | Constr i -> Constr {region = (set_markup i.region markup); value=i.value}
  | Lang i -> 
    let region = set_markup i.region markup in
    Lang {region; value = {region; value=i.value.value}}
  | Attr i -> Attr {region = (set_markup i.region markup); value=i.value}
  | Directive (Linemarker d) -> Directive (Linemarker d)

let rec lex_unit_to_closest_token_region: _ -> ExtRegion.t = function 
  Core.Token t :: _ -> ExtRegion.make (fst @@ Token.proj_token t) []
| Markup (Tabs {region; _} | Space {region; _} | Newline {region;_}
         | LineCom {region;_} | BlockCom {region; _} | BOM {region; _} ) :: [] -> ExtRegion.make region []
| Markup _ :: rest -> lex_unit_to_closest_token_region rest
| Directive (Linemarker {region;_}) :: [] -> ExtRegion.make region []
| Directive _ :: rest -> lex_unit_to_closest_token_region rest
| [] -> ExtRegion.ghost

let rec lex_unit_to_closest_token_region2: _ -> ExtRegion.t = function 
  Core.Token t :: _ -> ExtRegion.make (fst @@ Token.proj_token t) []
| Markup (LineCom {region;_} | BlockCom {region; _}) :: _ -> ExtRegion.make region []
| Markup _ :: rest -> lex_unit_to_closest_token_region2 rest
| Directive (Linemarker {region;_}) :: _ -> ExtRegion.make region []
| [] -> ExtRegion.ghost

let to_ext_region r = ExtRegion.{t_region=r; markup=[]}

let attach: (Token.token Core.lex_unit list, 'a) result -> (t list, 'a) result = function
  Stdlib.Ok lex_units ->
    let rec apply_comment (create_comment: lexeme ExtRegion.reg * ExtRegion.comment_position -> ExtRegion.markup) (region: Region.t) (value: string) result rest markup_queue = 
      (match result, rest with 
        hd :: tl, [] -> 
          apply ((set_markup (create_comment ((ExtRegion.{region={t_region=region; markup=[]}; value}: _ ExtRegion.reg), ExtRegion.Before) :: markup_queue) hd) :: tl) rest []
      | [], Core.Token hd :: rest -> 
        apply (to_t hd :: result) rest []
      | [], _ -> apply (set_markup (create_comment ({region = ExtRegion.make region []; value}, ExtRegion.After) :: markup_queue) (EOF ExtRegion.ghost) :: []) rest []
      | next_token :: _, (prev_token :: prev_rest) -> 
        let token = to_ext_region_t next_token in 
        let comment_stop_line = region#stop#line in
        let next_line = match token.markup with 
          [] -> token.t_region#start#line
        | ExtRegion.BlockCom ({region; _},_) :: _
        | LineCom ({region; _},_) :: _ -> region.t_region#start#line
        in        
        let previous_line = (lex_unit_to_closest_token_region2 rest).t_region#stop#line in

        if comment_stop_line + 1 = next_line && previous_line < comment_stop_line then 
          (* before next token *)
          
          apply (set_markup (create_comment ({region = ExtRegion.make region []; value}, ExtRegion.Before) :: token.markup) next_token :: result) rest []
        else  (
          let r = lex_unit_to_closest_token_region rest in
          let pos = if r.t_region#start#line = region#stop#line then
            ExtRegion.Inline (* inline after previous token *)
          else
            After (* after previous token *)
          in
          match prev_token with 
            Core.Token token ->
              apply ((set_markup (create_comment ({region=ExtRegion.make region []; value}, pos) :: markup_queue) (to_t token)) :: result) prev_rest []
          | _ ->
            apply result prev_rest ((create_comment ({region=ExtRegion.make region []; value}, pos)) :: markup_queue)
        )
      )
    and apply: t list -> Token.token Core.lex_unit list -> ExtRegion.markup list -> t list = fun result tokens markup_queue ->
      match tokens with 
        Core.Token token :: rest -> 
          apply ((set_markup markup_queue (to_t token))::result) rest []
      | Core.Markup (BlockCom c) :: rest -> 
        let value = String.sub c.value 2 (String.length c.value - 4) in
        apply_comment (fun (a, b) -> ExtRegion.BlockCom (a, b)) c.region value result rest markup_queue
      | Core.Markup (LineCom c) :: rest -> 
        let value = String.sub c.value 2 (String.length c.value - 2) in
        apply_comment (fun (a, b) -> ExtRegion.LineCom (a, b)) c.region value result rest markup_queue
      | Core.Markup _  :: rest -> apply result rest markup_queue
      | Core.Directive d  :: rest -> apply (Directive d :: result) rest markup_queue
      | [] -> result
    in 
    ok @@ apply [] (List.rev lex_units) []
  | Error _ as err -> err
