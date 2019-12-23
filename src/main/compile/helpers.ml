open Trace

type syntax = Pascaligo | Cameligo | ReasonLIGO

let syntax_enum : (string * syntax) list =
  [ ("pascaligo", Pascaligo) ;
    ("cameligo", Cameligo) ;
    ("reasonligo", ReasonLIGO) ;
  ]

(* Note: docs currently expect syntax_enum and syntax_extension_enum
   to line up, using "respectively." *)
let syntax_extension_enum : (string * syntax) list =
  [ (".ligo", Pascaligo) ;
    (".mligo", Cameligo) ;
    (".religo", ReasonLIGO) ;
  ]

let syntax_to_variant : string -> syntax result =
  fun s ->
  let rec find_syntax syntaxes =
    match syntaxes with
    | [] -> fail @@ simple_error (Format.asprintf "no such syntax: %s" s)
    | (syntax, v_syntax) :: _ when String.equal s syntax -> ok v_syntax
    | _ :: syntaxes -> find_syntax syntaxes in
  find_syntax syntax_enum

let detect_syntax : syntax option -> string option -> syntax result =
  fun syntax source_filename ->
  let subr s n =
    String.sub s (String.length s - n) n in
  let endswith s suffix =
    let suffixlen = String.length suffix in
    (  String.length s >= suffixlen
       && String.equal (subr s suffixlen) suffix)
  in
  let cannot_detect = simple_fail "cannot auto-detect syntax, please use -s name_of_syntax" in
  match (syntax , source_filename) with
  | Some syntax , _ -> ok syntax
  | None , Some sf ->
    let rec detect_syntax enum =
      match enum with
      | (ext, syntax) :: enum ->
        if endswith sf ext
        then Some syntax
        else detect_syntax enum
      | [] -> None in
    let syntax = detect_syntax syntax_extension_enum in
    begin
      match syntax with
      | Some syntax -> ok syntax
      | None -> cannot_detect
    end
  | None, None -> cannot_detect

let parsify_pascaligo source =
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Pascaligo.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
      Simplify.Pascaligo.simpl_program raw
  in ok simplified

let parsify_expression_pascaligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing expression") @@
    Parser.Pascaligo.parse_expression source in
  let%bind simplified =
    trace (simple_error "simplifying expression") @@
    Simplify.Pascaligo.simpl_expression raw in
  ok simplified

let parsify_cameligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Cameligo.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Cameligo.simpl_program raw in
  ok simplified

let parsify_expression_cameligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing expression") @@
    Parser.Cameligo.parse_expression source in
  let%bind simplified =
    trace (simple_error "simplifying expression") @@
    Simplify.Cameligo.simpl_expression raw in
  ok simplified

let parsify_reasonligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Reasonligo.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Cameligo.simpl_program raw in
  ok simplified

let parsify_expression_reasonligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing expression") @@
    Parser.Reasonligo.parse_expression source in
  let%bind simplified =
    trace (simple_error "simplifying expression") @@
    Simplify.Cameligo.simpl_expression raw in
  ok simplified

let parsify = fun (syntax : syntax) source_filename ->
  let%bind parsify = match syntax with
    | Pascaligo -> ok parsify_pascaligo
    | Cameligo -> ok parsify_cameligo
    | ReasonLIGO -> ok parsify_reasonligo in
  let%bind parsified = parsify source_filename in
  let%bind applied = Self_ast_simplified.all_program parsified in
  ok applied

let parsify_expression = fun syntax source ->
  let%bind parsify = match syntax with
    | Pascaligo -> ok parsify_expression_pascaligo
    | Cameligo -> ok parsify_expression_cameligo
    | ReasonLIGO -> ok parsify_expression_reasonligo
  in
  let%bind parsified = parsify source in
  let%bind applied = Self_ast_simplified.all_expression parsified in
  ok applied

let parsify_string_reasonligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Reasonligo.parse_string source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Cameligo.simpl_program raw in
  ok simplified

let parsify_string_pascaligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Pascaligo.parse_string source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Pascaligo.simpl_program raw in
  ok simplified

let parsify_string_cameligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Cameligo.parse_string source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Cameligo.simpl_program raw in
  ok simplified

let parsify_string = fun (syntax : syntax) source_filename ->
  let%bind parsify = match syntax with
    | Pascaligo -> ok parsify_string_pascaligo
    | Cameligo -> ok parsify_string_cameligo
    | ReasonLIGO -> ok parsify_string_reasonligo
  in
  let%bind parsified = parsify source_filename in
  let%bind applied = Self_ast_simplified.all_program parsified in
  ok applied

let pretty_print_pascaligo = fun source ->
  let%bind ast = Parser.Pascaligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state = Parser_pascaligo.ParserLog.mk_state
    ~offsets:true
    ~mode:`Byte
    ~buffer in
  Parser_pascaligo.ParserLog.pp_ast state ast;
  ok buffer

let pretty_print_cameligo = fun source ->
  let%bind ast = Parser.Cameligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state = Parser_cameligo.ParserLog.mk_state
    ~offsets:true
    ~mode:`Byte
    ~buffer in
  Parser.Cameligo.ParserLog.pp_ast state ast;
  ok buffer

let pretty_print_reasonligo = fun source ->
  let%bind ast = Parser.Reasonligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state = Parser.Reasonligo.ParserLog.mk_state
    ~offsets:true
    ~mode:`Byte
    ~buffer in
  Parser.Reasonligo.ParserLog.pp_ast state ast;
  ok buffer

let pretty_print = fun syntax source_filename ->
  let%bind v_syntax = detect_syntax syntax (Some source_filename) in
  (match v_syntax with
  | Pascaligo -> pretty_print_pascaligo
  | Cameligo -> pretty_print_cameligo
  | ReasonLIGO -> pretty_print_reasonligo)
  source_filename
