open Trace

type s_syntax = Syntax_name of string
type v_syntax = Pascaligo | Cameligo | ReasonLIGO

let syntax_to_variant : s_syntax -> string option -> v_syntax result =
  fun syntax source_filename ->
  let subr s n =
    String.sub s (String.length s - n) n in
  let endswith s suffix =
    let suffixlen = String.length suffix in
    (  String.length s >= suffixlen
       && String.equal (subr s suffixlen) suffix)
  in
  let (Syntax_name syntax) = syntax in
  match (syntax , source_filename) with
  | "auto" , Some sf when endswith sf ".ligo" -> ok Pascaligo
  | "auto" , Some sf when endswith sf ".mligo" -> ok Cameligo
  | "auto" , Some sf when endswith sf ".religo" -> ok ReasonLIGO
  | "auto" , _ -> simple_fail "cannot auto-detect syntax, pleas use -s name_of_syntax"
  | "pascaligo" , _ -> ok Pascaligo
  | "cameligo" , _ -> ok Cameligo
  | "reasonligo", _ -> ok ReasonLIGO
  | _ -> simple_fail "unrecognized parser"

let parsify_pascaligo ~loc_form = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Pascaligo.parse_file ~loc_form source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Pascaligo.simpl_program ~loc_form raw in
  ok simplified

let parsify_expression_pascaligo ~loc_form = fun source ->
  let%bind raw =
    trace (simple_error "parsing expression") @@
    Parser.Pascaligo.parse_expression ~loc_form source in
  let%bind simplified =
    trace (simple_error "simplifying expression") @@
    Simplify.Pascaligo.simpl_expression ~loc_form raw in
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

let parsify = fun ~loc_form (syntax : v_syntax) source_filename ->
  let parsify = match syntax with
    | Pascaligo -> parsify_pascaligo ~loc_form
    | Cameligo -> parsify_cameligo
    | ReasonLIGO -> parsify_reasonligo
  in
  let%bind parsified = parsify source_filename in
  let%bind applied = Self_ast_simplified.all_program parsified in
  ok applied

let parsify_expression ~loc_form = fun syntax source ->
  let parsify = match syntax with
    | Pascaligo -> parsify_expression_pascaligo ~loc_form
    | Cameligo -> parsify_expression_cameligo
    | ReasonLIGO -> parsify_expression_reasonligo
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

let parsify_string_pascaligo ~loc_form = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Pascaligo.parse_string ~loc_form source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Pascaligo.simpl_program ~loc_form raw in
  ok simplified

let parsify_string_cameligo = fun source ->
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.Cameligo.parse_string source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    Simplify.Cameligo.simpl_program raw in
  ok simplified

let parsify_string ~loc_form = fun (syntax : v_syntax) source_filename ->
  let parsify = match syntax with
    | Pascaligo -> parsify_string_pascaligo ~loc_form
    | Cameligo -> parsify_string_cameligo
    | ReasonLIGO -> parsify_string_reasonligo
  in
  let%bind parsified = parsify source_filename in
  let%bind applied = Self_ast_simplified.all_program parsified in
  ok applied
