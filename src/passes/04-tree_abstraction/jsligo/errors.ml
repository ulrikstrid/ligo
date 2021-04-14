open Simple_utils.Display

module Raw = Cst.Jsligo
module Parsing = Parsing.Jsligo

let stage = "abstracter"

type abs_error = [
  | `Concrete_jsligo_unknown_predefined_type of Raw.type_constr
  | `Concrete_jsligo_unknown_constant of string * Location.t
  | `Concrete_jsligo_recursive_fun of Region.t
  | `Concrete_jsligo_unsupported_pattern_type of Raw.pattern
  | `Concrete_jsligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_jsligo_unsupported_deep_list_pattern of Raw.pattern
  | `Concrete_jsligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_jsligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_jsligo_recursion_on_non_function of Location.t
  | `Concrete_jsligo_missing_funarg_annotation of Raw.variable
  | `Concrete_jsligo_funarg_tuple_type_mismatch of Region.t * Raw.pattern * Raw.type_expr
  | `Concrete_jsligo_unsupported_deep_pattern_matching of Region.t
  | `Concrete_jsligo_not_in_switch_or_loop of Region.t
  | `Concrete_jsligo_statement_not_supported_at_toplevel of Raw.statement
  | `Concrete_jsligo_not_a_valid_parameter of Raw.expr
  | `Concrete_jsligo_rest_not_supported_here of Raw.property
  | `Concrete_jsligo_property_not_supported of Raw.property
  | `Concrete_jsligo_expected_an_expression of Raw.array_item 
  | `Concrete_jsligo_new_not_supported of Raw.expr
  | `Concrete_jsligo_invalid_case of string * Raw.expr
  | `Concrete_jsligo_invalid_constructor of Raw.type_expr
  | `Concrete_jsligo_unsupported_match_pattern of Raw.expr
  | `Concrete_jsligo_unsupported_match_object_property of Raw.property
  | `Concrete_jsligo_expected_a_function of Raw.expr
  | `Concrete_jsligo_not_supported_assignment of Raw.expr
  | `Concrete_jsligo_array_rest_not_supported of Raw.array_item
  | `Concrete_jsligo_switch_not_supported of Raw.switch Region.reg
  | `Concrete_jsligo_break_not_implemented of Region.t
  | `Concrete_jsligo_expected_a_variable of Raw.expr
  | `Concrete_jsligo_expected_a_field_name of Raw.selection
  | `Concrete_jsligo_expected_an_int of Raw.expr
  | `Concrete_jsligo_invalid_list_pattern_match of Raw.array_item list
  ]

let unknown_predefined_type name = `Concrete_jsligo_unknown_predefined_type name
let unknown_constant s loc = `Concrete_jsligo_unknown_constant (s,loc)
let untyped_recursive_fun reg = `Concrete_jsligo_recursive_fun reg
let unsupported_pattern_type pl = `Concrete_jsligo_unsupported_pattern_type pl
let unsupported_deep_list_patterns cons = `Concrete_jsligo_unsupported_deep_list_pattern cons
let unsupported_string_singleton te = `Concrete_jsligo_unsupported_string_singleton te
let recursion_on_non_function reg = `Concrete_jsligo_recursion_on_non_function reg
let michelson_type_wrong texpr name = `Concrete_jsligo_michelson_type_wrong (texpr,name)
let michelson_type_wrong_arity loc name = `Concrete_jsligo_michelson_type_wrong_arity (loc,name)
let missing_funarg_annotation v = `Concrete_jsligo_missing_funarg_annotation v
let funarg_tuple_type_mismatch r p t = `Concrete_jsligo_funarg_tuple_type_mismatch (r, p, t)
let unsupported_deep_pattern_matching l = `Concrete_jsligo_unsupported_deep_pattern_matching l
let not_in_switch_or_loop b = `Concrete_jsligo_not_in_switch_or_loop b
let statement_not_supported_at_toplevel s = `Concrete_jsligo_statement_not_supported_at_toplevel s
let not_a_valid_parameter p = `Concrete_jsligo_not_a_valid_parameter p
let rest_not_supported_here p = `Concrete_jsligo_rest_not_supported_here p
let property_not_supported p = `Concrete_jsligo_property_not_supported p
let expected_an_expression p = `Concrete_jsligo_expected_an_expression p
let new_not_supported n = `Concrete_jsligo_new_not_supported n
let invalid_case s e = `Concrete_jsligo_invalid_case (s, e)
let invalid_constructor e = `Concrete_jsligo_invalid_constructor e
let unsupported_match_pattern p = `Concrete_jsligo_unsupported_match_pattern p
let unsupported_match_object_property o = `Concrete_jsligo_unsupported_match_object_property o
let expected_a_function e = `Concrete_jsligo_expected_a_function e
let not_supported_assignment e = `Concrete_jsligo_not_supported_assignment e
let array_rest_not_supported p = `Concrete_jsligo_array_rest_not_supported p
let switch_not_supported s = `Concrete_jsligo_switch_not_supported s
let break_not_implemented b = `Concrete_jsligo_break_not_implemented b
let expected_a_variable e = `Concrete_jsligo_expected_a_variable e
let expected_a_field_name f = `Concrete_jsligo_expected_a_field_name f
let expected_an_int e = `Concrete_jsligo_expected_an_int e
let invalid_list_pattern_match args = `Concrete_jsligo_invalid_list_pattern_match args

let error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_jsligo_unsupported_deep_pattern_matching l ->
      Format.fprintf f
      "@[<hv>%a@.Deep pattern matching is unsupported. @]"
        Snippet.pp_lift l
    | `Concrete_jsligo_unknown_predefined_type type_name ->
      Format.fprintf f
        "@[<hv>%a@.Unknown type \"%s\". @]"
        Snippet.pp_lift type_name.Region.region
        type_name.Region.value
    | `Concrete_jsligo_unknown_constant (s,loc) ->
      Format.fprintf f
      "@[<hv>%a@.Unknown constant: %s"
        Snippet.pp loc s
    | `Concrete_jsligo_recursive_fun reg ->
      Format.fprintf f
        "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to have a type annotation (for now). @]"
        Snippet.pp_lift reg
    | `Concrete_jsligo_unsupported_pattern_type pl ->
      Format.fprintf f
        "@[<hv>%a@.Invalid pattern matching.
If this is pattern matching over Booleans, then \"true\" or \"false\" is expected.
If this is pattern matching on a list, then one of the following is expected:
  * an empty list pattern \"[]\";
  * a cons list pattern \"[head, ...tail]\".
If this is pattern matching over variants, then a constructor of a variant is expected.

Other forms of pattern matching are not (yet) supported. @]"
        Snippet.pp_lift ((fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl)
    | `Concrete_jsligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        Snippet.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_jsligo_unsupported_deep_list_pattern cons ->
      Format.fprintf f
        "@[<hv>%a@.Invalid pattern matching. @.At this point, one of the following is expected:
  * an empty list pattern \"[]\";
  * a cons list pattern \"head :: tail\".@]"
        Snippet.pp_lift @@ Raw.pattern_to_region cons
    | `Concrete_jsligo_recursion_on_non_function reg ->
      Format.fprintf f "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
        Snippet.pp reg
    | `Concrete_jsligo_michelson_type_wrong (texpr,name) ->
      Format.fprintf f
       "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type. @]"
          Snippet.pp_lift (Raw.type_expr_to_region texpr)
          name
    | `Concrete_jsligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string. @]"
        Snippet.pp loc
        name
    | `Concrete_jsligo_missing_funarg_annotation v ->
      Format.fprintf f
        "@[<hv>%a@.Missing a type annotation for argument \"%s\". @]"
          Snippet.pp_lift v.region
          v.value
    | `Concrete_jsligo_funarg_tuple_type_mismatch (region, pattern, texpr) -> (
      let p = Parsing.pretty_print_pattern pattern |> Buffer.contents in
      let t = Parsing.pretty_print_type_expr texpr |> Buffer.contents in
      Format.fprintf
        f
        "@[<hv>%a@.The tuple \"%s\" does not match the type \"%s\". @]"
        Snippet.pp_lift region
        p
        t
    )
    | `Concrete_jsligo_not_in_switch_or_loop reg -> (
        Format.fprintf f "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
          Snippet.pp_lift reg
    )
    | `Concrete_jsligo_statement_not_supported_at_toplevel s -> (
        Format.fprintf f "@[<hv>%a@.Statement not supported at toplevel.@.Only let, const, and type statements are supported at the toplevel. @]"
          Snippet.pp_lift (Raw.statement_to_region s)
    )
    | `Concrete_jsligo_not_a_valid_parameter p -> (
      Format.fprintf f "@[<hv>%a@.Not a valid function parameter. @]"
          Snippet.pp_lift (Raw.expr_to_region p)
    )
    | `Concrete_jsligo_rest_not_supported_here p -> (
      Format.fprintf f "@[<hv>%a@.Rest property not supported here. @]"
          Snippet.pp_lift (Raw.property_to_region p)
    )
    | `Concrete_jsligo_property_not_supported p -> (
      Format.fprintf f "@[<hv>%a@.This kind of property not supported here. @]"
          Snippet.pp_lift (Raw.property_to_region p)
    )
    | `Concrete_jsligo_expected_an_expression a -> (
      Format.fprintf f "@[<hv>%a@.Expected an expression. @]"
          Snippet.pp_lift (Raw.array_item_to_region a)
    )
    | `Concrete_jsligo_new_not_supported p -> (
      Format.fprintf f "@[<hv>%a@.'new' keyword not supported. @]"
          Snippet.pp_lift (Raw.expr_to_region p)
    )
    | `Concrete_jsligo_invalid_case (s, e) -> (
      Format.fprintf f "@[<hv>%a@.Invalid '%s' field value. An anonymous arrow function was expected, eg. `None: () => foo`.@]"
          Snippet.pp_lift (Raw.expr_to_region e) @@ s
    )
    | `Concrete_jsligo_invalid_constructor e -> (
      Format.fprintf f "@[<hv>%a@.Invalid constructor. Expected a constructor like: `[\"Foo\"]` or `[\"Foo\", int, string]`.@]"
          Snippet.pp_lift (Raw.type_expr_to_region e)
    )
    | `Concrete_jsligo_unsupported_match_pattern e -> (
      Format.fprintf f "@[<hv>%a@.Unsupported match pattern.@]"
          Snippet.pp_lift (Raw.expr_to_region e)
    )
    | `Concrete_jsligo_unsupported_match_object_property p -> (
      Format.fprintf f "@[<hv>%a@.Unsupported pattern match object property.@]"
          Snippet.pp_lift (Raw.property_to_region p)
    )
    | `Concrete_jsligo_expected_a_function e -> (
      Format.fprintf f "@[<hv>%a@.Expected a function.@]"
          Snippet.pp_lift (Raw.expr_to_region e)
    )
    | `Concrete_jsligo_not_supported_assignment e -> (
      Format.fprintf f "@[<hv>%a@.Not supported assignment.@]"
          Snippet.pp_lift (Raw.expr_to_region e)
    )
    | `Concrete_jsligo_array_rest_not_supported e -> (
      Format.fprintf f "@[<hv>%a@.Rest property not supported here.@]"
          Snippet.pp_lift (Raw.array_item_to_region e)
    )
    | `Concrete_jsligo_switch_not_supported s -> (
      Format.fprintf f "@[<hv>%a@.Switch statement is not supported.@]"
          Snippet.pp_lift s.region
    )
    | `Concrete_jsligo_break_not_implemented b -> (
      Format.fprintf f "@[<hv>%a@.Break statement is not supported.@]"
          Snippet.pp_lift b
    )
    | `Concrete_jsligo_expected_a_variable e -> (
      Format.fprintf f "@[<hv>%a@.Expected a variable.@]"
          Snippet.pp_lift (Raw.expr_to_region e)
    )
    | `Concrete_jsligo_expected_a_field_name s -> (
      Format.fprintf f "@[<hv>%a@.Expected a field name.@]"
      Snippet.pp_lift (Raw.selection_to_region s)
    )
    | `Concrete_jsligo_expected_an_int e -> (
      Format.fprintf f "@[<hv>%a@.Expected an int.@]"
      Snippet.pp_lift (Raw.expr_to_region e)
    )
    | `Concrete_jsligo_invalid_list_pattern_match _l -> (
      Format.fprintf f "@[<hv>Invalid list pattern matching.@]"
    )
  )


let error_jsonformat : abs_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Concrete_jsligo_unsupported_deep_pattern_matching l ->
    let message = `String "Deep pattern matching is unsupported" in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson (Snippet.lift l));] in
    json_error ~stage ~content
  | `Concrete_jsligo_unknown_predefined_type type_name ->
    let message = `String "Unknown predefined type" in
    let t = `String type_name.Region.value in
    let loc = Format.asprintf "%a" Location.pp_lift type_name.Region.region in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("type", t ) ] in
    json_error ~stage ~content
  | `Concrete_jsligo_unknown_constant (s,loc) ->
    let message = `String ("Unknow constant: " ^ s) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Concrete_jsligo_recursive_fun reg ->
    let message = `String "Untyped recursive functions are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift reg in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unsupported_pattern_type pl ->
    let loc = Format.asprintf "%a"
      Location.pp_lift ((fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl) in
    let message = `String "Currently, only booleans, lists, options, and constructors are supported in patterns" in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unsupported_string_singleton te ->
    let message = `String "Unsupported singleton string type" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region te) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unsupported_deep_list_pattern cons ->
    let message = `String "Currently, only empty lists and x::y are supported in list patterns" in
    let loc = Format.asprintf "%a" Location.pp_lift @@ Raw.pattern_to_region cons in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_recursion_on_non_function reg ->
    let message = Format.asprintf "Only functions can be recursive." in
    let loc = Format.asprintf "%a" Location.pp reg in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc) ] in
    json_error ~stage ~content
  | `Concrete_jsligo_michelson_type_wrong (texpr,name) ->
    let message = Format.asprintf "Argument %s of %s must be a string singleton"
        (Cst_jsligo.Printer.type_expr_to_string ~offsets:true ~mode:`Point texpr) name in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region texpr) in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_jsligo_michelson_type_wrong_arity (loc,name) ->
    let message = Format.asprintf "%s does not have the right number of argument" name in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_jsligo_missing_funarg_annotation v ->
    let message = Format.asprintf "Missing type annotation for argument \"%s\"" v.value in
    let loc = Format.asprintf "%a" Location.pp_lift v.region in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_jsligo_funarg_tuple_type_mismatch (r, _, _) ->
    let message = Format.asprintf "The tuple does not match the type." in
    let loc = Format.asprintf "%a" Location.pp_lift r in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc);
    ] in
    json_error ~stage ~content
  | `Concrete_jsligo_not_in_switch_or_loop reg ->
    let message = `String "Not in switch or loop." in
    let loc = Format.asprintf "%a" Location.pp_lift reg in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_statement_not_supported_at_toplevel statement ->
    let message = `String "Statement not supported at toplevel." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.statement_to_region statement) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_not_a_valid_parameter expr ->
    let message = `String "Not a valid function parameter." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region expr) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_rest_not_supported_here p ->
    let message = `String "Rest property not supported here." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.property_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_property_not_supported p -> 
    let message = `String "This kind of property not supported here." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.property_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_an_expression p ->
    let message = `String "Expected an expression." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.array_item_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_new_not_supported p ->
    let message = `String "'new' keyword not supported." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_invalid_case (_, e) ->
    let message = `String "Invalid case." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_invalid_constructor e ->
    let message = `String "Invalid constructor." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unsupported_match_pattern e ->
    let message = `String "Unsupported match pattern." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_unsupported_match_object_property p ->
    let message = `String "Unsupported pattern match object property." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.property_to_region p) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_a_function e ->
    let message = `String "Expected a function." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_not_supported_assignment e ->
    let message = `String "Not supported asignment." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_array_rest_not_supported e ->
    let message = `String "Rest property not supported here." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.array_item_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_switch_not_supported s ->
    let message = `String "Switch statement not supported." in
    let loc = Format.asprintf "%a" Location.pp_lift s.region in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_break_not_implemented b ->
    let message = `String "Break statement not supported." in
    let loc = Format.asprintf "%a" Location.pp_lift b in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_a_variable e ->
    let message = `String "Expected a variable." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_a_field_name s ->
    let message = `String "Expected a field name." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.selection_to_region s) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_expected_an_int e ->
    let message = `String "Expected an int." in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region e) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_jsligo_invalid_list_pattern_match _l ->
    let message = `String "Expected an int." in
    (* let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region e) in *)
    let content = `Assoc [
      ("message", message);
      (* ("location", `String loc); *)
      ] in
    json_error ~stage ~content