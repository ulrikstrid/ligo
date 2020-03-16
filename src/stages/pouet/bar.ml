module Types = struct
  open Display

  type expression = E_toto of int | E_coco of string

  type program = expression list

  let foo_expression_ppformat ~display_format f a =
    match display_format with
    | Human_readable | Dev -> (
      match a with 
      | E_toto i ->
        let i' = Format.asprintf "toto %i" i in 
        Format.pp_print_string f i'
      | E_coco s ->
        let s' = Format.asprintf "coco %s" s in
        Format.pp_print_string f s'
    )
  let foo_expression_jsonformat a =
    match a with 
    | E_toto i ->
      `Assoc [("toto expression", `Int i)]
    | E_coco s ->
      `Assoc [(("coco expression"), `String s)]

  let foo_expression_format : 'a Display.format = {
    pp = foo_expression_ppformat;
    to_json = foo_expression_jsonformat;
  }
end

  let f_err : unit -> (Types.expression, Myerror.error) result =
    fun () -> Error (Bar_error1 42)
  let f_ok  : Foo.Types.expression -> (Types.expression, Myerror.error) result =
    fun _e -> Ok (E_toto 42)